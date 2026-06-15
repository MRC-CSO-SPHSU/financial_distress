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
          # ---- Baseline covariates (W): measured before the first treatment ----
          sex_dv_base,
          hiqual_dv_base,
          race_base,
          sf12mcs_dv_base,
          gor_dv_fact_base,
          age_dv_base,
          # ---- Wave 0 confounders (W: still pre-A_0, so baseline) --------------
          # Ordered so every confounder precedes A_0 (DAG: CF/IC/Emp -> FD).
          pcs_lagged_0,           # CF: PCS
          econ_benefits_lagged_0, # CF: benefits
          home_owner_lagged_0,    # CF: home ownership
          mastat_lagged_0,        # CF: marital status
          dnc_lagged_0,           # CF: dependents
          log_income_0,           # IC: log income
          econ_emp_bin_fact_0,    # Emp: employment status
          econ_dist_bin_0,        # A_0: FD at wave 3
          sf12mcs_dv_0,           # Y_0: MCS at wave 3
          # ---- Wave 1 confounders (L-nodes: post-A_0, affected by prior FD) ----
          pcs_lagged_1,           # CF: PCS
          econ_benefits_lagged_1, # CF: benefits
          home_owner_lagged_1,    # CF: home ownership
          mastat_lagged_1,        # CF: marital status
          dnc_lagged_1,           # CF: dependents
          log_income_1,           # IC: log income
          econ_emp_bin_fact_1,    # Emp: employment status
          econ_dist_bin_1,        # A_1: FD at wave 4
          sf12mcs_dv_1,           # Y_1: MCS at wave 4
          # ---- Wave 2 confounders (L-nodes) -----------------------------------
          pcs_lagged_2,           # CF: PCS
          econ_benefits_lagged_2, # CF: benefits
          home_owner_lagged_2,    # CF: home ownership
          mastat_lagged_2,        # CF: marital status
          dnc_lagged_2,           # CF: dependents
          log_income_2,           # IC: log income
          econ_emp_bin_fact_2,    # Emp: employment status
          econ_dist_bin_2,        # A_2: FD at wave 5
          sf12mcs_dv_2            # Y_2: MCS at wave 5
        )
    })
  }

  ltmle_prep(mice::complete(wide_mids, action = "all"))
}
