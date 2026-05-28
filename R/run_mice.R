run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
  # wide_data$pidp <- as.numeric(wide_data$pidp) # not needed anymore

  method_list <- mice::make.method(wide_data)
  method_list[c("pidp", "sex_dv_base", "hiqual_dv_base", "race_base",
                "pcs_lagged_0", "econ_dist_bin_0", "sf12mcs_dv_0",
                "pcs_lagged_1", "econ_dist_bin_1", "sf12mcs_dv_1",
                "pcs_lagged_2", "econ_dist_bin_2", "sf12mcs_dv_2")] <-
                  c("", "logreg", "polr", "logreg",
                    "pmm", "logreg", "pmm",
                    "pmm", "logreg", "pmm",
                    "pmm", "logreg", "pmm")

  pred_mat <- mice::make.predictorMatrix(wide_data)
#  pred_mat[, "pidp"] <- 0
#  pred_mat["pidp", ] <- 0 # not needed anymore

  mice::mice(
    data            = wide_data,
    defaultMethod   = c("pmm", "logreg", "polyreg", "polr"),
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = method_list,
    predictorMatrix = pred_mat
  )
}
