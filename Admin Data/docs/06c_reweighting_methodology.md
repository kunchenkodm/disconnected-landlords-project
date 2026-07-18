# Econometric Methodology: Pair-Difference Collapse and Reweighting in `06c`

This note formalizes the econometric link between the baseline matching regression in script `06` and the heterogeneous treatment effect (HTE) reweighting procedure in script `06c`.

## 1. The Pair-Difference Collapse

In script `06`, the headline average treatment effect on the treated (ATT), $\tau$, is estimated via a linear fixed-effects model:

$$ Y_{is} = \tau \text{Treat}_{is} + \gamma_1 \text{Rooms}_{is} + \gamma_2 \text{Floor}_{is} + \delta_s + \epsilon_{is} $$

where $i$ indexes properties and $s$ indexes the exact-matching subclass. 

Because the matching procedure forms **1:1 nearest-neighbor pairs** within exact strata, each subclass $s$ contains exactly two properties: one treated ($Treat = 1$) and one control ($Treat = 0$). By taking the difference between the treated and control property within each subclass, the subclass fixed effect $\delta_s$ drops out:

$$ \Delta Y_s = \tau + \gamma_1 \Delta \text{Rooms}_s + \gamma_2 \Delta \text{Floor}_s + \Delta \epsilon_s $$

In script `06c`, this collapsed model is estimated to recover the pooled ATT:

```r
# Recover the baseline ATT via pair-difference
feols(D_y ~ 1 + D_rooms + D_floor, data = est, cluster = ~local_authority)
```

## 2. Cell-Specific CATEs

To explore heterogeneity, we divide properties into granular archetypes (cells) $c \in C$ based on covariates (e.g., property type, age band, fuel). The single intercept $\tau$ is replaced with a vector of cell-specific intercepts $\beta_c$:

$$ \Delta Y_s = \sum_{c \in C} \beta_c \cdot \mathbb{I}(Cell_s = c) + \gamma_1 \Delta \text{Rooms}_s + \gamma_2 \Delta \text{Floor}_s + \Delta \epsilon_s $$

```r
# Estimate cell-specific Conditional Average Treatment Effects (CATEs)
feols(D_y ~ 0 + i(cell_f) + D_rooms + D_floor, data = est, cluster = ~local_authority)
```

## 3. Reweighting and the Composition Effect

Because different landlord types (e.g., offshore vs. domestic) select into owning different property archetypes, comparing raw ATTs is confounded by composition. We reweight the CATEs to a common reference distribution—the **Eligible Control Pool**.

Let $w_c$ be the proportion of cell $c$ in the common control pool ($\sum w_c = 1$). The **Reweighted ATT** ($\tau_{rw}$) is:

$$ \tau_{rw} = \sum_{c \in C} w_c \cdot \beta_c $$

### Delta-Method Standard Errors and the VCOV Matrix ($V$)

Because $\tau_{rw}$ is a linear combination of the estimated cell CATEs ($\hat{\beta}$), its variance is calculated using the variance-covariance (VCOV) matrix of those estimates, $V$.

Crucially, the regression estimating the CATEs is **clustered at the Local Authority level**:
```r
feols(D_y ~ 0 + i(cell_f) + D_rooms + D_floor, ..., cluster = ~local_authority)
```
This means $V$ is a **cluster-robust covariance matrix**. It accounts for the fact that unobserved shocks to energy efficiency (e.g., local regulations, housing market dynamics, or regional EPC assessor practices) are correlated among properties within the same local authority.

Because the weights $W$ are treated as fixed population constants, the variance of the weighted sum $\sum w_c \hat{\beta}_c$ is given by the quadratic form $W^T V W$. This correctly propagates the clustered covariances between different archetypes within the same local authority into the final standard error:

$$ SE(\tau_{rw}) = \sqrt{W^T V W} $$

```r
# Reweight cell CATEs to the common market baseline
rw_row[, tau_rw_full := sum(w_norm * b[real_idx])]
rw_row[, tau_rw_full_se := sqrt(as.numeric(t(w_norm) %*% V[real_idx, real_idx] %*% w_norm))]

# Isolate the composition effect
rw_row[, composition_effect := att_coef - tau_rw_full]
```

The **Composition Effect** ($\tau - \tau_{rw}$) isolates the portion of the headline treatment effect driven purely by the specific mix of properties owned by the treated group.

## 4. Narrative Archetypes

While the full reweighting procedure (`tau_rw_full`) uses the entire whitelist of granular cells, presenting the CATEs for all ~100 cells is unwieldy. Furthermore, simply picking the top 10 most frequent cells yields near-duplicates (e.g., various sizes of period gas terraced houses).

To solve this, the script selects an interpretable set of 10 **Narrative Archetypes** using a **diversity-aware greedy selection algorithm**:

1. It groups cells into coarse "buckets" based on `Property Type` $\times$ `Fuel Class` $\times$ `Construction Era`.
2. It seeds the set with the overall modal cell in the control pool.
3. It iteratively selects the most frequent control-pool cell that fills a previously *unrepresented* bucket until 10 archetypes are chosen.

This guarantees a diverse, interpretable "narrative" set (e.g., modern electric flats, period gas houses, rural oil properties). The script then calculates a simplified reweighted ATT using *only* these 10 archetypes (`tau_rw_arch10`), with weights re-normalized to sum to 1 within this restricted set:

```r
# Calculate reweighted ATT using only the 10 narrative archetypes
w_arch <- w_arch_raw / cov_arch
rw_row[, tau_rw_arch10 := sum(w_arch * b[arch_idx])]
rw_row[, tau_rw_arch10_se := sqrt(as.numeric(t(w_arch) %*% V[arch_idx, arch_idx] %*% w_arch))]
```

This allows the study to present heterogeneous treatment effects in a highly readable exhibit while verifying that the core reweighting conclusions remain robust to this narrative simplification.
