fit_ltmle_one <- function(regime_label, imp_idx, ltmle_data_list,
                          regimes, Qform, gform, sl_libs) {
  # ltmle -> SuperLearner resolves SL.library names ("SL.xgboost.ltmle") against
  # the global environment. On a fresh batchtools worker the custom learner from
  # R/sl_wrappers.R isn't auto-shipped because sl_libs references it only as a
  # string: the symbol reference on the RHS makes targets detect and serialize
  # it with this target, and the assignment exposes it where SuperLearner's name
  # lookup can find it.
  assign("SL.xgboost.ltmle", SL.xgboost.ltmle, envir = globalenv())

  ltmle::ltmle(
    data            = ltmle_data_list[[imp_idx]],
    Anodes          = c("econ_dist_bin_0", "econ_dist_bin_1", "econ_dist_bin_2"),
    # All time-varying confounders measured after A_0 must be L-nodes (ltmle
    # requires every column after the first A/C node to be an A/C/L/Y node).
    Lnodes          = c("pcs_lagged_1", "econ_benefits_lagged_1", "home_owner_lagged_1",
                        "mastat_lagged_1", "dnc_lagged_1", "log_income_1", "econ_emp_bin_fact_1",
                        "pcs_lagged_2", "econ_benefits_lagged_2", "home_owner_lagged_2",
                        "mastat_lagged_2", "dnc_lagged_2", "log_income_2", "econ_emp_bin_fact_2"),
    Ynodes          = c("sf12mcs_dv_0",    "sf12mcs_dv_1",    "sf12mcs_dv_2"),
    survivalOutcome = FALSE,
    Qform           = Qform,
    gform           = gform,
    abar            = regimes[[regime_label]],
    SL.library      = sl_libs,
    estimate.time   = FALSE,
    variance.method = "ic",
    Yrange          = c(0, 1)
  )
}
