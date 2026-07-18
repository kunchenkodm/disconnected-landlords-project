# Evaluation: The Cell-Reweighting Pipeline (06c / 06c-pilot / 06e)

Step-by-step reconstruction of the reweighted estimator with the actual code, focus on
the variance–covariance construction, the small-cell path, and proposed refinements.

Companion to `docs/06c_reweighting_methodology.md` (the original derivation note).

**Pipeline map**

| Stage | Script | Role |
|---|---|---|
| Matching | `05_matching.R` | 1:1 NN within exact strata → `matched_pairs_*.RData` (uprn, subclass) |
| Headline ATT | `06_run_regressions.R` | `PSM (Matched) + Subclass FE` → `results_table_LA.csv` |
| Cell system + weights | `06c_heterogeneous_effects.R` | control-pool cells, whitelist, $w_c$, archetypes → `hte_archetype_definitions_LA.csv` |
| Estimability pilot | `06c_heterogeneous_effects_pilot.R` | per-cell CATEs on disjoint samples, support gate |
| Controls ladder | `06e_hte_controls_ladder.R` | joint CATEs + $\tau_{rw}$ for all 9 spec × core combos → `hte_controls_ladder_LA.csv` |
| Figure | `07c_hte_controls_ladder_plot.R` | forest chart (single panel, `cp/pct` headline combo) |

---

## 1. Pair-difference collapse

Matching (script 05) produces 1:1 pairs within subclass $s$. The subclass-FE model collapses to a
pair-difference regression; the FE $\delta_s$ drops out:

$$
Y_{is} = \tau\,T_{is} + \gamma' X_{is} + \delta_s + \varepsilon_{is}
\;\;\Longrightarrow\;\;
\Delta Y_s = \tau + \gamma' \Delta X_s + \Delta\varepsilon_s
$$

where $\Delta X_s$ contains the spec's **continuous** matching covariates (exact vars are constant
within pair by construction, so they difference to zero). The matched `.RData` files store only
`(uprn, subclass)`; the join back to the lodgement-level table fans out over duplicate lodgements,
and one exact-var-consistent lodgement combination is selected per pair
(`scripts/06e_hte_controls_ladder.R:302-323`):

```r
pr <- dat[md, on = "uprn", allow.cartesian = TRUE, nomatch = 0]
pr[, tv := pr[[config$var]]]
pr <- pr[!is.na(tv)]

cons_vars <- setdiff(spec$exact_vars, "local_authority")
pr[, .stratum := do.call(paste, c(.SD, sep = "\r")), .SDcols = cons_vars]
both <- pr[, .(has_t = any(tv == 1L), has_c = any(tv == 0L)),
           by = .(subclass, .stratum)][has_t & has_c]
setorder(both, subclass, .stratum)
chosen <- both[, .(.stratum = .stratum[1L]), by = subclass]
```

Differences are then formed and the pair is assigned to a cell **from its control member**
(`scripts/06e_hte_controls_ladder.R:345-354`):

```r
pd <- merge(p_t, p_c, by = "subclass")
for (oc in ladder_outcomes) pd[, paste0("D_", oc) := get(paste0("t_", oc)) - get(oc)]
for (cv in spec$continuous_vars) pd[, (d_col_map[[cv]]) := get(paste0("t_", cv)) - get(cv)]
pd[, cell_id := make_cell_id(.SD)]     # from the CONTROL member's covariates
```

Spec-dependent $\Delta X_s$ (`scripts/06e_hte_controls_ladder.R:137-138`): `D_rooms`, `D_floor`,
and for the PPD specs `D_ppd` ($\Delta$ `ppd_price_sqm`) — this is what "including the price-paid
controls" means at the pair level; the `ppd`/`ppd_counciltax` **matching cores** additionally
guarantee the pairs were *formed* within the PPD sample on those covariates.

---

## 2. Cell system and reweighting weights (06c, frozen for 06e)

Cells: `property_type × built_form × construction_age_band × main_fuel × floor-area tercile`,
terciles cut on the eligible control pool. Weights are control-pool shares — treatment-invariant
by construction (`scripts/06c_heterogeneous_effects.R:261-298`):

```r
ctrl_pool <- unique(dat[!is.na(treat_for_profit) & treat_for_profit == 0L], by = "uprn")
ctrl_pool <- ctrl_pool[complete.cases(ctrl_pool[, ..base_matching_vars])]
tercile_cuts <- quantile(ctrl_pool$total_floor_area, probs = c(1/3, 2/3), na.rm = TRUE)

ctrl_pool[, cell_id := make_cell_id(.SD)]
cell_freq <- ctrl_pool[, .(n_control_pool = .N), by = cell_id][order(-n_control_pool)]
cell_freq[, share_control_pool := n_control_pool / sum(n_control_pool)]
cell_freq[, in_whitelist := rank <= HTE_MAX_CELLS]         # top 100

whitelist <- cell_freq[in_whitelist == TRUE, cell_id]
w_lookup  <- setNames(cell_freq$share_control_pool, cell_freq$cell_id)
```

$$
w_c = \frac{N^{pool}_c}{\sum_{c'} N^{pool}_{c'}}, \qquad \sum_{c \in \mathcal{C}} w_c = 1
$$

06e **reads** this cell system (cuts, whitelist, weights, modal cell A1) from
`hte_archetype_definitions_LA.csv` so the reference composition is identical across all 9
spec × core combos (`scripts/06e_hte_controls_ladder.R:97-111`). Only the estimation sample and
control set vary along the ladder.

---

## 3. The small-cell gate: what happens below the support floor

Per treatment × spec × core, a cell is estimated on its own coefficient **only if** it is in the
top-100 whitelist **and** has at least `HTE_MIN_PAIRS = 50` matched pairs; otherwise its pairs are
collapsed into a single `"_other"` bucket (`scripts/06e_hte_controls_ladder.R:412-414`):

```r
pair_cell_n <- pd[, .N, by = cell_id]
keep_cells <- pair_cell_n[cell_id %in% whitelist & N >= HTE_MIN_PAIRS, cell_id]
pd[, cell_est := fifelse(cell_id %in% keep_cells, cell_id, "_other")]
```

Consequences, in order of importance:

1. **The failing cell's weight is silently dropped and the remaining weights renormalised**
   (Section 5). $\tau_{rw}$ is then an effect *on the covered subpopulation*
   $\mathcal{K} \subset \mathcal{C}$, rescaled to sum to one — not on the full control pool.
   `coverage_full` records $\sum_{c \in \mathcal{K}} w_c$; the `"_other"` CATE is estimated but
   **discarded**.
2. **The covered set $\mathcal{K}$ varies by treatment and by spec × core combo.** Across the
   ladder the "fixed composition" is fixed only in the weights, not in which cells enter: a combo
   where a cell drops below 50 pairs (PPD cores shrink samples) reweights over a *different*
   renormalised subpopulation. Cross-combo and cross-treatment comparisons of $\tau_{rw}$ are
   therefore not exactly like-for-like when coverage differs.
3. **Why individual small-cell estimates would be unreliable if the floor were lowered (e.g. < 30
   pairs).** $\hat\beta_c$ is the intercept of a regression with $1 + p$ parameters ($p \le 3$
   continuous differences). With $n_c < 30$:
   - the LA-clustered variance for that coefficient is effectively computed on
     $G_c = $ #LAs contributing to the cell, and CRV1 is biased **downward** when $G_c$ is small
     and cluster sizes are unbalanced (Cameron–Miller); the model-wide dof correction uses total
     $G$, not $G_c$, so the reported $t$-statistic is overstated exactly for the cells least able
     to afford it;
   - the intercept extrapolates to $\Delta X_s = 0$; with few pairs, $\hat\gamma$ leverage from a
     handful of poorly-matched pairs moves $\hat\beta_c$ directly (in the joint model $\gamma$ is
     shared, which softens but does not remove this — see Section 4);
   - cells passing a *stochastic* gate near the threshold are selected partly on the realised
     configuration of their pairs, and their inverse-variance draws feed the renormalised sum
     with weight $w_c/\text{cov}$ — noise in small covered cells is *amplified* by
     renormalisation, not damped.
4. **The few-clusters guard is asymmetric in 06e.** The pilot classifies cells
   (`scripts/06c_heterogeneous_effects_pilot.R:389-392`):

   ```r
   cell_supp[, status := fcase(
     n_pairs >= HTE_MIN_PAIRS & n_las >= FEW_CLUSTERS_THRESHOLD, "ok",
     n_pairs >= HTE_MIN_PAIRS & n_las <  FEW_CLUSTERS_THRESHOLD, "suspect",
     default = "fail")]
   ```

   but 06e applies **no `n_las` gate to non-modal cells** — a cell with 60 pairs in 8 LAs enters
   $\tau_{rw}$ with a downward-biased variance contribution; only the *modal* cell carries
   `modal_few_clusters_flag` (`scripts/06e_hte_controls_ladder.R:502-503`).

---

## 4. CATE estimation: two implementations, one important difference

**(a) Joint model — production 06c and 06e.** One regression, cell intercepts via `i(cell_f)`,
**common** slopes $\gamma$, LA-clustered (`scripts/06e_hte_controls_ladder.R:443-458`):

$$
\Delta Y_s = \sum_{c} \beta_c \,\mathbb{1}\{Cell_s = c\} + \gamma' \Delta X_s + \Delta\varepsilon_s
$$

```r
est[, cell_f := factor(cell_est)]
cm <- feols(as.formula(paste0(dy, " ~ 0 + i(cell_f) + ", rhs)),   # rhs = D_rooms + D_floor [+ D_ppd]
            data = est, cluster = ~local_authority, lean = TRUE)
ct <- coeftable(cm)
cell_rows_idx <- grep("^cell_f::", rownames(ct))
b <- ct[cell_rows_idx, "Estimate"]
V <- vcov(cm)[cell_rows_idx, cell_rows_idx, drop = FALSE]
```

**(b) Per-cell model — 06c-pilot.** Each cell fit on its own disjoint pair sample with its **own**
slopes (`scripts/06c_heterogeneous_effects_pilot.R:461-474`):

```r
fml_cell <- as.formula(paste0(dy, " ~ 1 + D_rooms + D_floor"))
setkey(est, cell_id)
for (cc in cn) {
  sub <- est[.(cc), nomatch = NULL]
  if (nrow(sub) < HTE_MIN_PAIRS) next
  mm <- feols(fml_cell, data = sub, cluster = ~local_authority, lean = TRUE)
  b_by[cc] <- coeftable(mm)["(Intercept)", "Estimate"]
  ...
}
```

Differences: (i) joint imposes slope homogeneity $\gamma_c \equiv \gamma$, pilot does not — with
50-pair cells the pilot's per-cell $\hat\gamma_c$ is itself noisy and contaminates
$\hat\beta_c$; (ii) memory: the joint clustered vcov materialises an $n_{obs} \times K$ scores
matrix (the OOM failures logged in `hte_controls_ladder_errors_LA.csv`), the pilot's loop is
memory-trivial for any $K$; (iii) **the vcov treatment differs — next section.**

---

## 5. The reweighted estimator and its variance

$$
\hat\tau_{rw} \;=\; \sum_{c \in \mathcal{K}} \tilde w_c\, \hat\beta_c,
\qquad
\tilde w_c = \frac{w_c}{\sum_{c' \in \mathcal{K}} w_{c'}}
$$

with $\mathcal{K}$ = estimated (whitelist ∩ ≥50-pair) cells, `"_other"` excluded.
Weights are treated as **fixed constants** (defensible: the control pool has millions of
properties, so $\operatorname{Var}(\hat w_c) = O(1/N^{pool})$ is negligible), so the delta method
is a plain quadratic form.

**(a) 06c / 06e — full clustered quadratic form.** $V$ is the cell-block of the CRV1 sandwich from
the *joint* fit:

$$
\hat V_{CRV1} = \frac{G}{G-1}\cdot\frac{N-1}{N-K}\,
 (X'X)^{-1}\Big(\sum_{g=1}^{G} X_g' \hat u_g \hat u_g' X_g\Big)(X'X)^{-1},
\qquad
\widehat{SE}(\hat\tau_{rw}) = \sqrt{\tilde w' \, V_{\mathcal{K}\mathcal{K}} \, \tilde w}
$$

(`scripts/06e_hte_controls_ladder.R:482-491`):

```r
real_idx <- which(cell_names != "_other")
w_raw  <- unname(w_lookup[cell_names[real_idx]])
cov_full <- sum(w_raw)
w_norm <- w_raw / cov_full
tau_rw <- sum(w_norm * b[real_idx])
tau_rw_se <- sqrt(as.numeric(
  t(w_norm) %*% V[real_idx, real_idx, drop = FALSE] %*% w_norm))
```

Taking the sub-block $V_{\mathcal{K}\mathcal{K}}$ is exact here: $\hat\tau_{rw} = a'\hat\theta$
with $a$ zero on $\gamma$ and `"_other"`, so only the $\mathcal{K}$ rows/columns of the full vcov
enter. Off-diagonal terms $\operatorname{Cov}(\hat\beta_c, \hat\beta_{c'})$ are **non-zero**
because the same LA contributes pairs to many cells, and the LA-level score outer products
$X_g'\hat u_g \hat u_g' X_g$ link the cell dummies.

**(b) 06c-pilot — diagonal approximation.** The pilot assumes independence across cells
(`scripts/06c_heterogeneous_effects_pilot.R:326-345`):

```r
# Cells are estimated on DISJOINT matched-pair samples, so their CATEs are
# independent and the reweighting variance is Var(tau_rw) = sum_c w_norm_c^2 Var(beta_c).
reweight <- function(b_by, var_by, keep_ids) {
  ...
  out$tau <- sum(w_norm * b_by[ids])
  out$se  <- sqrt(sum(w_norm^2 * var_by[ids]))
  ...
}
```

$$
\widehat{\operatorname{Var}}^{pilot}(\hat\tau_{rw}) = \sum_{c} \tilde w_c^2\, \widehat{se}_c^{\,2}
$$

**The comment's premise is incomplete: disjoint pairs ≠ independent estimates.** The samples are
disjoint in *pairs* but not in *clusters* — an LA-level shock (assessor practices, local
retrofit programmes) hits pairs in every cell located in that LA, inducing
$\operatorname{Cov}(\hat\beta_c, \hat\beta_{c'}) > 0$ under positively correlated within-LA
errors. Dropping these terms **understates** $SE(\hat\tau_{rw})$ (for typically positive
covariances). The joint-model quadratic form in (a) is the correct one and should be the only
production variance; the pilot's diagonal form is acceptable only as a lower bound.

**Composition effect** (`scripts/06e_hte_controls_ladder.R:506`):

$$
\text{composition} = \hat\tau_{ATT} - \hat\tau_{rw}
$$

where $\hat\tau_{ATT}$ weights cells by the *treated matched-pair* distribution and
$\hat\tau_{rw}$ by the (covered, renormalised) *control-pool* distribution. Note its SE is not
reported — $\hat\tau_{ATT}$ and $\hat\tau_{rw}$ are estimated from the same pairs and are highly
correlated, so $SE(\hat\tau_{ATT} - \hat\tau_{rw})$ cannot be built from the two reported SEs.

---

## 6. Proposed refinements

Ordered by (impact ÷ effort). Items 1–3 are small, local changes to 06e.

### 6.1 Stop discarding `"_other"` — report a full-coverage estimator (one-line change)

`"_other"`'s CATE is already estimated and sits in `b` / `V`. Instead of renormalising over
covered cells, close the decomposition:

$$
\hat\tau_{rw}^{full} = \sum_{c \in \mathcal{K}} w_c \hat\beta_c
 + \Big(1 - \sum_{c \in \mathcal{K}} w_c\Big) \hat\beta_{other},
\qquad
\widehat{SE} = \sqrt{a' V a},\;\; a = (w_{\mathcal{K}}, 1 - \textstyle\sum w_{\mathcal{K}})
$$

```r
oth_idx <- which(cell_names == "_other")
a <- c(w_raw, 1 - cov_full)                       # unnormalised weights + remainder
idx <- c(real_idx, oth_idx)
tau_rw_closed    <- sum(a * b[idx])
tau_rw_closed_se <- sqrt(as.numeric(t(a) %*% V[idx, idx] %*% a))
```

This (i) removes the coverage-renormalisation estimand drift, (ii) makes $\tau_{rw}$ comparable
across combos/treatments with different coverage (fixes Section 3.2), (iii) costs nothing.
Caveat: `"_other"` weight should strictly be the control-pool share of *non-covered* cells, which
equals $1-\text{cov}$ only up to cells absent from the pool — compute it from `cell_freq` to be
exact. Report alongside (not instead of) the current renormalised $\tau_{rw}$.

### 6.2 Enforce the pilot's per-cell cluster gate in 06e

Apply `n_las >= FEW_CLUSTERS_THRESHOLD` to *all* covered cells (currently only the modal cell is
flagged). Cells failing it go to `"_other"` (harmless under 6.1) or carry a per-cell flag column
in `hte_controls_ladder_LA.csv` so 07c can hollow out suspect points.

```r
cell_ok <- cell_support[cell_names[real_idx], n_las >= FEW_CLUSTERS_THRESHOLD]
```

### 6.3 Empirical-Bayes shrinkage for small cells instead of a hard 50-pair cliff

The pilot already computes Cochran's $Q$ (`scripts/06c_heterogeneous_effects_pilot.R:314-323`).
Extend it to a DerSimonian–Laird random-effects layer and shrink before reweighting:

$$
\hat\tau^2_{DL} = \max\!\Big(0,\; \frac{Q - (K-1)}{\sum_c v_c^{-1} - \sum_c v_c^{-2}/\sum_c v_c^{-1}}\Big),
\qquad
\tilde\beta_c = \frac{\hat\tau^2}{\hat\tau^2 + v_c}\,\hat\beta_c
             + \frac{v_c}{\hat\tau^2 + v_c}\,\bar\beta
$$

```r
v <- diag(V)[real_idx]
w_iv <- 1 / v
bbar <- sum(w_iv * b[real_idx]) / sum(w_iv)
Q <- sum(w_iv * (b[real_idx] - bbar)^2)
tau2 <- max(0, (Q - (length(real_idx) - 1)) /
              (sum(w_iv) - sum(w_iv^2) / sum(w_iv)))
B <- tau2 / (tau2 + v)                             # shrinkage factors
b_shrunk <- B * b[real_idx] + (1 - B) * bbar
tau_rw_eb <- sum(w_norm * b_shrunk)
```

This lets the floor drop to ~20–30 pairs safely: a 25-pair cell contributes its weight but its
noisy $\hat\beta_c$ is pulled toward the precision-weighted mean rather than either trusted
(current pilot behaviour at 50) or discarded (current 06e behaviour below 50). Variance: the
shrunk estimator is still linear in $\hat\beta$ once $B$ and the IVW weights are held fixed,
$a_j = \tilde w_j B_j + \big(\sum_c \tilde w_c (1-B_c)\big)\, \frac{v_j^{-1}}{\sum_k v_k^{-1}}$,
so $SE = \sqrt{a'Va}$; or wild-bootstrap the whole map (6.4) to also propagate $\hat\tau^2_{DL}$.

### 6.4 Few-cluster-robust inference on $\tau_{rw}$ itself

$\tau_{rw}$ is a linear combination $a'\hat\theta$, so it can be tested directly with a wild
cluster bootstrap over LAs — no re-matching needed, `est` is small after the Section-4 slimming:

```r
# fwildclusterboot on the joint model; R = restriction vector = a (padded with 0s for gamma)
boottest(cm, clustid = "local_authority", R = a_padded, r = 0,
         B = 9999, type = "rademacher")
```

Alternatively CR3/jackknife (`vcov = "jackknife"` in recent fixest) for the joint fit.
Priority: medium — total $G \approx 300+$ LAs is comfortable for the *pooled* ATT, but cells
concentrated in few LAs make the *cell-block* of $V$ the weak point (Section 3.3).

### 6.5 Hierarchical fallback instead of one flat `"_other"`

`"_other"` pools a heterogeneous residual (rural oil cottages + park homes + …). Collapse failing
cells into their **coarse bucket** (`ptype_bucket × fuel_class × era` — already computed by the
06c greedy picker, `scripts/06c_heterogeneous_effects.R:292-295`) and estimate bucket CATEs; a
small cell then inherits its bucket's $\hat\beta$ rather than a global residual mean. Combine with
6.1 for full coverage with better local fidelity.

### 6.6 Comparability audit for the ladder figure

Add to `hte_controls_ladder_LA.csv`: `ess_cells = 1 / sum(w_norm^2)` (effective number of cells),
`max_w_norm`, and the covered-cell set hash. Then 07c can (a) annotate coverage per point,
(b) optionally re-run $\tau_{rw}$ on the **intersection** of covered cells across all 9 combos of
a treatment so the ladder holds the covered set literally fixed, not just the weight base.

### 6.7 Slope-homogeneity check (joint vs per-cell)

The joint model's common $\gamma$ vs the pilot's per-cell $\gamma_c$ is testable: interact
`D_rooms/D_floor` with a coarse bucket factor and Wald-test the interactions once per combo. If
rejected, report $\tau_{rw}$ from the interacted model as robustness (the intercepts then stop
absorbing cell-specific covariate-imbalance corrections).

### 6.8 Formalise the fixed-weights claim

One sentence + one number in the writeup: $w_c$ estimated on $N^{pool}$ in the millions implies
$SE(\hat w_c) \le \tfrac{1}{2\sqrt{N^{pool}}} \approx 2\times10^{-4}$, second-order relative to
$SE(\hat\beta_c)$; hence the delta method conditioning on $w$ is innocuous.

---

## 7. Bottom line

- The **estimator** ($\tau_{rw} = \tilde w'\hat\beta$) and the **06c/06e variance**
  ($\tilde w' V \tilde w$ from the single LA-clustered joint fit) are correctly constructed, and
  the joint-fit vcov is the right object — it carries the cross-cell within-LA covariances.
- The **pilot's diagonal variance** rests on a false independence premise (disjoint pairs, shared
  clusters) and should not be promoted to production.
- The **small-cell path** (< 50 pairs → `"_other"` → weight dropped → renormalise) is the main
  methodological soft spot: it drifts the estimand with coverage, makes covered sets differ
  across the ladder, and the whitelist floor interacts with the (absent) per-cell LA gate.
  Refinements 6.1–6.3 close it at minimal cost: keep `"_other"` in the sum, gate on clusters
  everywhere, and shrink rather than drop or trust small cells.
