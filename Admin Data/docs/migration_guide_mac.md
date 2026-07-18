# macOS migration pointer (2026-07-18)

This repo was migrated from a Windows laptop to a MacBook Pro in July 2026 and de-linked from the
dissertation project (project3 / Self-Dealing) and the Obsidian research vault.

The full migration handover — step-by-step guides, paste-ready Claude Code prompts, the data-delta
manifest, and the Claude memory export — lives in a standalone folder created on the Windows
machine at `C:\Users\Dmytro\Documents\GitHub\mac-migration-handover\` and copied to the Mac
(default location `~/migration-handover/`).

Key outcomes (see the handover README for detail):

- Data moved out of the repo to `~/ResearchData` (`raw/` shared, `ra/processed`, `ra/output`).
  `scripts/00_setup.R` reads roots from `DL_RAW_DIR` / `DL_PROCESSED_DIR` / `DL_OUTPUT_DIR`
  (set in `.Renviron`); unset vars fall back to the historical in-repo `data/` + `output/` layout.
- Dissertation extracted to the private repo `kunchenkodm/project3` (branch `main`);
  vault extracted to its own private repo. Neither lives in this repo/working tree anymore.
- Branch `migration-prep` (off `Full-Sample`) carries the July 2026 work and this patch set;
  merge into `Full-Sample` after review.
