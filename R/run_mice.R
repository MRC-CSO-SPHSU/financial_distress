run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
  wide_data$pidp <- as.numeric(wide_data$pidp)

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
  pred_mat[, "pidp"] <- 0
  pred_mat["pidp", ] <- 0

  imp <- mice::mice(
    data            = wide_data,
    defaultMethod   = c("pmm", "logreg", "polyreg", "polr"),
    m               = m,
    maxit           = maxit,
    seed            = seed,
    method          = method_list,
    predictorMatrix = pred_mat
  )

  # Surface mice's loggedEvents (auto-dropped constant/collinear predictors) in
  # the target's build log. They also remain on imp$loggedEvents for later use.
  le <- imp$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_mice: no logged events.")
  } else {
    message("run_mice: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  imp
}
