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
    Lnodes          = c("pcs_lagged_0",    "pcs_lagged_1",    "pcs_lagged_2"),
    Ynodes          = c("sf12mcs_dv_0",    "sf12mcs_dv_1",    "sf12mcs_dv_2"),
    survivalOutcome = FALSE,
    Qform           = Qform,
    gform           = gform,
    abar            = regimes[[regime_label]],
    SL.library      = sl_libs,
    estimate.time   = FALSE,
    variance.method = "ic",
    Yrange          = c(0, 1),
    gbounds = c(0.05, 0.95)
  )
}
