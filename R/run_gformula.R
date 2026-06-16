# g-formula MI predictor matrix and imputation of counterfactual outcomes
# @ pending: improve make_predictor_matrix function so that it can work without manually modifying it's output

run_gformula <- function(wide_mids, wide_data_mi, M = 50) {
  regimes <- dplyr::tibble(a = 0:1, b = 0:1, c = 0:1) |>
    expand.grid() |>
    dplyr::arrange(a, b, c) |>
    t() |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    unclass() |> unname()

  predictor_matrix <- make_predictor_matrix(wide_data_mi)

# SF12 at wave 1 is predicted by all time-invariant confounders
predictor_matrix["sf12mcs_dv_base", c("sex_dv_base",
                                      "hiqual_dv_base",
                                      "race_base",
                                      "gor_dv_fact_base",
                                      "age_dv_base")] <- 1
# SF12 at wave 1 predicts all time-variant confounders at the earliest subsequent wave
predictor_matrix[c("pcs_lagged_0",
                   "econ_dist_bin_0",
                   "dnc_lagged_0",
                   "home_owner_lagged_0",
                   "econ_benefits_lagged_0",
                   "mastat_lagged_0"), "sf12mcs_dv_base"] <- 1
  
## also predict intermediate confounders and SF12 at wave 0
predictor_matrix[c("log_income_0",
                   "econ_emp_bin_fact_0",
                   "sf12mcs_dv_0"), "sf12mcs_dv_base"] <- 1

# but does not predict any other variable at wave 1 or 2
predictor_matrix[c("pcs_lagged_1", "pcs_lagged_2",
                   "econ_dist_bin_1", "econ_dist_bin_2",
                   "dnc_lagged_1", "dnc_lagged_2",
                   "home_owner_lagged_1", "home_owner_lagged_2",
                   "econ_benefits_lagged_1", "econ_benefits_lagged_2",
                   "mastat_lagged_1", "mastat_lagged_2",
                   "log_income_1", "log_income_2",
                   "econ_emp_bin_fact_1", "econ_emp_bin_fact_2",
                   "sf12mcs_dv_1", "sf12mcs_dv_2"), "sf12mcs_dv_base"] <- 0
  
# time varying confounders does not predict among themselves in the same wave
predictor_matrix[c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0"), 
                 c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0")] <- 0
predictor_matrix[c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1"),
                 c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1")] <- 0
predictor_matrix[c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2"),
                 c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2")] <- 0
  
# but all predict distress at time t
predictor_matrix[c("econ_dist_bin_0"), 
  c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0")] <- 1
predictor_matrix[c("econ_dist_bin_1"), 
  c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1")] <- 1
predictor_matrix[c("econ_dist_bin_2"), 
  c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2")] <- 1

## time-varying confounders at time t predicts themselves at t+1
predictor_matrix["pcs_lagged_1", "pcs_lagged_0"] <- 1
predictor_matrix["pcs_lagged_2", "pcs_lagged_1"] <- 1
predictor_matrix["econ_dist_bin_1", "econ_dist_bin_0"] <- 1
predictor_matrix["econ_dist_bin_2", "econ_dist_bin_1"] <- 1
predictor_matrix["dnc_lagged_1", "dnc_lagged_0"] <- 1
predictor_matrix["dnc_lagged_2", "dnc_lagged_1"] <- 1
predictor_matrix["home_owner_lagged_1", "home_owner_lagged_0"] <- 1
predictor_matrix["home_owner_lagged_2", "home_owner_lagged_1"] <- 1
predictor_matrix["econ_benefits_lagged_1", "econ_benefits_lagged_0"] <- 1
predictor_matrix["econ_benefits_lagged_2", "econ_benefits_lagged_1"] <- 1
predictor_matrix["mastat_lagged_1", "mastat_lagged_0"] <- 1
predictor_matrix["mastat_lagged_2", "mastat_lagged_1"] <- 1
predictor_matrix["log_income_1", "log_income_0"] <- 1
predictor_matrix["log_income_2", "log_income_1"] <- 1
predictor_matrix["econ_emp_bin_fact_1", "econ_emp_bin_fact_0"] <- 1
predictor_matrix["econ_emp_bin_fact_2", "econ_emp_bin_fact_1"] <- 1

## econ_dist_bin_t predicts econ_dist_bin_t+1
predictor_matrix["econ_dist_bin_1", "econ_dist_bin_0"] <- 1
predictor_matrix["econ_dist_bin_2", "econ_dist_bin_1"] <- 1

## sf_12mcs_dv_t only predicts sf_12mcs_dv_t+1 (not other vars)
predictor_matrix[, "sf12mcs_dv_0"] <- 0
predictor_matrix[, "sf12mcs_dv_1"] <- 0
predictor_matrix[, "sf12mcs_dv_2"] <- 0
predictor_matrix["sf12mcs_dv_1", "sf12mcs_dv_0"] <- 1
predictor_matrix["sf12mcs_dv_2", "sf12mcs_dv_1"] <- 1

## sf_12mcs_dv_t predicts econ_dist_bin_t+1
predictor_matrix["econ_dist_bin_1", "sf12mcs_dv_0"] <- 1
predictor_matrix["econ_dist_bin_2", "sf12mcs_dv_1"] <- 1

## sf_12mcs_dv_t predicts time varying confounders at t+1
predictor_matrix[c("pcs_lagged_1", 
                   "econ_dist_bin_1", 
                   "dnc_lagged_1", 
                   "home_owner_lagged_1", 
                   "econ_benefits_lagged_1", 
                   "mastat_lagged_1"), "sf12mcs_dv_0"] <- 1
predictor_matrix[c("pcs_lagged_2", 
                   "econ_dist_bin_2", 
                   "dnc_lagged_2", 
                   "home_owner_lagged_2", 
                   "econ_benefits_lagged_2", 
                   "mastat_lagged_2"), "sf12mcs_dv_1"] <- 1

# intermediate confounders at time t predict distress at time t
predictor_matrix[c("econ_dist_bin_0"), 
  c("log_income_0", "econ_emp_bin_fact_0")] <- 1
predictor_matrix[c("econ_dist_bin_1"), 
  c("log_income_1", "econ_emp_bin_fact_1")] <- 1
predictor_matrix[c("econ_dist_bin_2"), 
  c("log_income_2", "econ_emp_bin_fact_2")] <- 1

predictor_matrix["regime", ] <- 1
predictor_matrix["regime", "regime"] <- 0

  imps <- gFormulaMI::gFormulaImpute(
    data             = wide_mids,
    M                = M,
    trtVars          = c("econ_dist_bin_0", "econ_dist_bin_1", "econ_dist_bin_2"),
    trtRegimes       = regimes,
    predictorMatrix  = predictor_matrix,
    silent           = TRUE
  )

  fits <- imps %$%
    lm(sf12mcs_dv_2 ~ factor(regime) + 0)

  outvals <- gFormulaMI::syntheticPool(fits)

  regimes_3 <- tibble::tibble(
    intervention = regimes |>
      purrr::map(paste, collapse = "-") |>
      purrr::reduce(c)
  )

  out <- outvals |>
    tibble::as_tibble() |>
    tibble::rownames_to_column("Intervention") |>
    dplyr::transmute(
      mi_effect = Estimate,
      mi_se     = sqrt(Total),
      mi_ll     = `95% CI L`,
      mi_ul     = `95% CI U`
    ) |>
    dplyr::bind_cols(regimes_3)

  # Surface the g-formula imputation's loggedEvents in the build log.
  le <- imps$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_gformula: no logged events.")
  } else {
    message("run_gformula: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  out
}
