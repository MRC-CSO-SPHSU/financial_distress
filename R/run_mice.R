run_mice <- function(wide_data, m = 10, maxit = 10, seed = 20260522) {

  method_list <- mice::make.method(wide_data)

  methods_by_group <- c(
    # Time-invariant confounders
    sex_dv_base = "logreg",
    hiqual_dv_fact_base = "polr",
    race_base = "logreg",
    # Lagged confounders
    pcs_lagged = "pmm",
    mcs_lagged = "pmm",
    econ_dist_bin_lagged = "logreg",
    econ_benefits_lagged = "logreg",
    home_owner_lagged = "logreg",
    mastat_lagged = "logreg",
    dnc_lagged = "polyreg",
    econ_emp_bin_fact = "logreg",
    log_income = "pmm",       # continuous
    # Other confounders
    gor_dv_fact = "polr",
    age_dv = "norm",
    # Exposure
    econ_dist_bin = "logreg",
    # Outcome
    sf12mcs_dv = "pmm",
    sf12pcs_dv = "pmm"
  )

  group   <- sub("_(\\d+|base)$", "", names(method_list))
  matched <- group %in% names(methods_by_group)
  method_list[matched] <- methods_by_group[group[matched]]

  pred_mat <- mice::make.predictorMatrix(wide_data)

  mids <-  mice::mice(
           data            = wide_data,
           m               = m,
           maxit           = maxit,
           seed            = seed,
           method          = method_list,
           predictorMatrix = pred_mat
          )

  le <- mids$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_mice: no logged events.")
  } else {
    message("run_mice: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  mids
}
