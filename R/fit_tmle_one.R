# Single-point TMLE (via ltmle) for one imputation.
#
# Estimand: the point-treatment ATE of financial distress (econ_dist_bin_0) on
# SF-12 MCS (sf12mcs_dv_0), adjusting for the confounder block W.
fit_tmle_one <- function(wide_mids, imp_idx, sl_libs) {
  # Assigning custom SL wrappers to globalenv() so ltmle can find them by name
  assign("SL.xgboost.ltmle", SL.xgboost.ltmle, envir = globalenv())
  assign("SL.glmnet.ltmle",  SL.glmnet.ltmle,  envir = globalenv())

  # Confounders (W). Listed before the A-node so ltmle treats them as baseline
  # covariates, not post-treatment L-nodes.
  W <- c("sex_dv_base", "hiqual_dv_base", "race_base", "sf12mcs_dv_base",
         "age_dv_0", "gor_dv_fact_0", "pcs_lagged_0", "dnc_lagged_0",
         "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0",
         "econ_emp_bin_fact_0", "log_income_0")

  dat <- mice::complete(wide_mids, action = imp_idx) |>
    dplyr::mutate(
      econ_dist_bin_0 = as.integer(as.character(econ_dist_bin_0)),
      sf12mcs_dv_0    = sf12mcs_dv_0 / 100   # bound outcome to [0, 1] for Yrange
    ) |>
    dplyr::select(dplyr::all_of(W), 
                  econ_dist_bin_0, 
                  sf12mcs_dv_0)  # order: W, A, Y

  Qform <- c(sf12mcs_dv_0 =
               paste("Q.kplus1 ~", paste(c(W, "econ_dist_bin_0"), collapse = " + ")))
  gform <- c(econ_dist_bin_0 =
               paste("econ_dist_bin_0 ~", paste(W, collapse = " + ")))

  ltmle::ltmle(
    data            = dat,
    Anodes          = "econ_dist_bin_0",
    Lnodes          = NULL,
    Ynodes          = "sf12mcs_dv_0",
    survivalOutcome = FALSE,
    Qform           = Qform,
    gform           = gform,
    abar            = list(1, 0),
    SL.library      = sl_libs,
    estimate.time   = FALSE,
    variance.method = "ic",
    Yrange          = c(0, 1)
  )
}
