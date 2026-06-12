build_wide_data <- function(pop_data) {
  int_data_wide <- pop_data[wave %in% 2:5][, t0 := t0 - 2][
      , `:=`(pcs_lagged = shift(sf12pcs_dv, type = "lag"),
             dnc_lagged = shift(dnc_fact, type = "lag"),
             home_owner_lagged = shift(home_owner, type = "lag"),
             econ_benefits_lagged = shift(econ_benefits, type = "lag"),
             mastat_lagged = shift(mastat_dv, type = "lag")
            ),
        by = pidp
    ][wave %in% 3:5]

  # Drop pidps with no usable MCS: baseline missing, OR both intermediate waves (t0=2 and t0=3) missing.
  # Operates per-pidp on long data
  bad_pidps <- int_data_wide[
    , .(base_na = any(is.na(sf12mcs_dv_base)),
        t0_na   = any(t0 == 2L & is.na(sf12mcs_dv)),
        t3_na   = any(t0 == 3L & is.na(sf12mcs_dv))),
    by = pidp
  ][base_na | (t0_na & t3_na), pidp]

  final_data <- int_data_wide[!pidp %in% bad_pidps]

  intervention_pattern_3 <- expand_grid(0:1, 0:1, 0:1) |>
    t() |>
    as_tibble(.name_repair = "minimal") |>
    unclass()

  wide_data <- final_data |>
    mutate(econ_dist_bin = as.factor(econ_dist_bin)) |>
    make_wide(
      pidp,
      t0,
      base_cols = c(sex_dv_base,
                    hiqual_dv_base,
                    race_base,
                    gor_dv_fact_base,
                    sf12mcs_dv_base,
                    age_dv_base),
      outcome = sf12mcs_dv,
      pcs_lagged,
      econ_dist_bin,
      dnc_lagged,
      home_owner_lagged,
      econ_benefits_lagged,
      mastat_lagged,
      econ_emp_bin_fact,
      log_income,
      waves = c(0, 1, 2)
    )

  wide_data
}