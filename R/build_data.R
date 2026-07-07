build_data <- function(pop_data) {
  int_data_wide <- pop_data[wave %in% 2:3][, t0 := t0 - 2][
      , `:=`(pcs_lagged = shift(sf12pcs_dv, type = "lag"),
             dnc_lagged = shift(dnc_fact, type = "lag"),
             home_owner_lagged = shift(home_owner, type = "lag"),
             econ_benefits_lagged = shift(econ_benefits, type = "lag"),
             mastat_lagged = shift(mastat_dv, type = "lag")
            ),
        by = pidp
    ][wave %in% 3]

  # Drop pidps with no usable MCS: baseline missing
  bad_pidps <- int_data_wide[
    , .(base_na = any(is.na(sf12mcs_dv_base))),
    by = pidp
  ][(base_na), pidp]

  final_data <- int_data_wide[!pidp %in% bad_pidps]

  wide_data <- final_data |>
    mutate(econ_dist_bin = as.factor(econ_dist_bin)) |>
    make_wide(
      pidp,
      t0,
      base_cols = c(sex_dv_base,
                    hiqual_dv_fact_base,
                    race_base,
                    sf12mcs_dv_base),
      outcome = sf12mcs_dv,
      age_dv,
      gor_dv_fact,
      pcs_lagged,
      econ_dist_bin,
      dnc_lagged,
      home_owner_lagged,
      econ_benefits_lagged,
      mastat_lagged,
      econ_emp_bin_fact,
      log_income,
      waves = c(0)
    )
  
  wide_data <- set_exposure(wide_data, exposure = "econ_dist_bin")
  
  wide_data
}
