# Local check of the MAIHDA layer against the design's recorded baseline.
# Reads cate_one from the MAIN checkout's store: the worktree has none.
here::i_am("scripts/run_gate_meta_local.R")
pacman::p_load(targets, dplyr, tibble, purrr, mice)
for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f)

MAIN_STORE <- "/Users/darwin.delcastillofernandez/Documents/GitHub/financial_distress/_targets"
cate_one <- targets::tar_read(cate_one, store = MAIN_STORE)
cat("imputations read:", length(cate_one), "\n")

one <- lapply(cate_one, function(x) fit_gate_meta(x$gate))
res <- pool_gate_meta(one)

cat("\n--- pooled scalars ---\n");  print(as.data.frame(res$scalars), digits = 4)
cat("\n--- moderator coefficients ---\n"); print(as.data.frame(res$coefs), digits = 4)
cat("\n--- cells ---\n")
print(as.data.frame(res$cells[, c("strata_label", "gate", "additive", "resid",
                                  "resid_ll", "resid_ul", "blup", "n_j_min")]),
      digits = 3)
cat("\n--- leave-one-cell-out PCV ---\n")
print(as.data.frame(res$loco[order(-res$loco$pcv_d), c("dropped_label", "pcv_d")]),
      digits = 3)
cat("\n--- per-imputation tests ---\n")
print(summary(res$tests[, c("QM", "QMp", "pcv")]))
cat("\nPCV negative in", sum(res$tests$pcv < 0), "of", nrow(res$tests), "imputations\n")
