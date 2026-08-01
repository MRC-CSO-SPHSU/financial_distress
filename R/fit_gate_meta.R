# MAIHDA decomposition of the DR-learner GATEs, one imputed dataset.
# Design: .claude/plans/2026-07-31-maihda-decomposition-design.md
#
# Consumes cate_one[[m]]$gate (12 rows) and returns the random-effects
# decomposition: tau2 with and without additive main effects, PCV, per-cell
# BLUPs and deleted residuals, and leave-one-cell-out PCV.

# Recover sex / race / education by SPLITTING THE LABEL, not by parsing the
# strata_id digits. strata_label comes from interaction(sex, race, hiqual,
# sep = ":") in R/strata_creation.R and make.names() has already stripped any
# ":" from the levels, so the split is exact. Reversing 100*sex + 10*race +
# hiqual would instead depend on as.numeric(factor) level ordering and, if that
# drifts, would silently mislabel cells rather than error.
.gate_factors <- function(gate_tbl) {
  parts <- strsplit(as.character(gate_tbl$strata_label), ":", fixed = TRUE)
  if (any(lengths(parts) != 3L)) {
    stop("fit_gate_meta: every strata_label must split into exactly three ",
         "parts on ':'; got lengths ",
         paste(sort(unique(lengths(parts))), collapse = "/"), ".")
  }
  m <- do.call(rbind, parts)
  out <- gate_tbl
  out$sex  <- factor(m[, 1])
  out$race <- factor(m[, 2])
  out$educ <- factor(m[, 3])

  n_cells <- nrow(unique(out[c("sex", "race", "educ")]))
  if (nrow(out) != 12L || n_cells != 12L) {
    stop("fit_gate_meta: expected a full 12-cell sex x race x education ",
         "design; got ", nrow(out), " rows spanning ", n_cells, " distinct cells.")
  }
  out
}
