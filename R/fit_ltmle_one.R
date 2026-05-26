fit_ltmle_one <- function(regime_label, imp_idx, ltmle_data_list,
                          regimes, Qform, gform, sl_libs) {
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
    Yrange          = c(0, 1)
  )
}
