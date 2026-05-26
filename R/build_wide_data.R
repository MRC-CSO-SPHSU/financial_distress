build_wide_data <- function(pop_data) {
  int_data_wide <- pop_data[wave %in% 2:5][, t0 := t0 - 2][
      , `:=`(pcs_lagged = shift(sf12pcs_dv, type = "lag")),
        by = pidp
    ][wave %in% 3:5]

  missings <-
    int_data_wide[is.na(sf12mcs_dv) |
               is.na(pcs_lagged) | is.na(sex_dv_base) | is.na(race_base) |
               is.na(econ_dist_bin) | is.na(hiqual_dv_base), .(pidp)]

  final_data <- int_data_wide

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
                    race_base),
      outcome = sf12mcs_dv,
      pcs_lagged,
      econ_dist_bin,
      waves = c(0, 1, 2)
    )

  wide_data
}
