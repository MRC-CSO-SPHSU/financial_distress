prepare_ltmle_data <- function(wide_mids) {
  ltmle_prep <- function(data_list) {
    purrr::map(data_list, \(dat) {
      dat |>
        dplyr::mutate(
          econ_dist_bin_0 = as.integer(as.character(econ_dist_bin_0)),
          econ_dist_bin_1 = as.integer(as.character(econ_dist_bin_1)),
          econ_dist_bin_2 = as.integer(as.character(econ_dist_bin_2)),
          sf12mcs_dv_0    = sf12mcs_dv_0 / 100,
          sf12mcs_dv_1    = sf12mcs_dv_1 / 100,
          sf12mcs_dv_2    = sf12mcs_dv_2 / 100
        ) |>
        dplyr::select(
          sex_dv_base,
          hiqual_dv_base,
          race_base,
          pcs_lagged_0,     # W:   PCS at wave 2 (baseline)
          econ_dist_bin_0,  # A_0: FD at wave 3
          sf12mcs_dv_0,     # Y_0: MCS at wave 3 (between A_0 and A_1)
          pcs_lagged_1,     # L_1: PCS at wave 3 (between A_0 and A_1)
          econ_dist_bin_1,  # A_1: FD at wave 4
          sf12mcs_dv_1,     # Y_1: MCS at wave 4 (between A_1 and Y)
          pcs_lagged_2,     # L_2: PCS at wave 4 (between A_1 and Y)
          econ_dist_bin_2,  # A_2: FD at wave 5
          sf12mcs_dv_2      # Y_2: MCS at wave 5
        )
    })
  }

  ltmle_prep(mice::complete(wide_mids, action = "all"))
}
