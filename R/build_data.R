build_data <- function(data, pre_wave = 2, target_wave = 3,
                       outcome = c("MCS", "PCS")) {
  library(dplyr)

  outcome <- match.arg(outcome)

  eligible_pidp <- intersect(
    data[wave == pre_wave & response == 1, pidp],
    data[wave == target_wave & response == 1, pidp]
  )

  pop_DT <- data[pidp %in% eligible_pidp]

  data.table::setorder(pop_DT, pidp, wave)

  int_data_wide <- pop_DT[
      , `:=`(t0 = t0 - 2L,
             pcs_lagged = shift(sf12pcs_dv, type = "lag"),
             mcs_lagged = shift(sf12mcs_dv, type = "lag"),
             dnc_lagged = shift(dnc_fact, type = "lag"),
             home_owner_lagged = shift(home_owner, type = "lag"),
             econ_benefits_lagged = shift(econ_benefits, type = "lag"),
             mastat_lagged = shift(mastat_dv, type = "lag"),
             econ_dist_bin_lagged = shift(econ_dist_bin, type = "lag")
            ),
        by = pidp
    ][wave %in% target_wave]

  t0_target <- target_wave - 3L

  # Drop pidps with no usable MCS/PCS: baseline missing
  outcome_col <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  base_col    <- paste0(outcome_col, "_base")

  bad_pidps <- int_data_wide[
    , .(base_na = any(is.na(get(base_col)))),
    by = pidp
  ][(base_na), pidp]

  final_data <- int_data_wide[!pidp %in% bad_pidps]

  # The lag is taken before this point, so econ_dist_bin_lagged is still integer;
  # factor both so mice/gFormulaMI treat them as the same binary node type.
  final_data <- final_data |>
    mutate(across(c(econ_dist_bin, econ_dist_bin_lagged), as.factor)) |>
    # Formula-safe factor levels before reshaping: run_mice() puts
    # sex x race x hiqual into a mice formula (design 8.5), and race_base's
    # "Non-white" / dnc_lagged's "2+" would not survive the re-parse.
    # econ_dist_bin's "0"/"1" are numeric-coded and left untouched by design.
    sanitize_factor_levels()

  if (outcome == "MCS") {
  wide_data <- final_data |>
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
      econ_dist_bin_lagged,
      dnc_lagged,
      home_owner_lagged,
      econ_benefits_lagged,
      mastat_lagged,
      econ_emp_bin_fact,
      log_income,
      waves = t0_target
    ) |> data.table::as.data.table()
  } else if (outcome == "PCS") {
    wide_data <- final_data |>
      make_wide(
        pidp,
        t0,
        base_cols = c(sex_dv_base,
                      hiqual_dv_fact_base,
                      race_base,
                      sf12pcs_dv_base),
        outcome = sf12pcs_dv,
        age_dv,
        gor_dv_fact,
        mcs_lagged,
        econ_dist_bin,
        econ_dist_bin_lagged,
        dnc_lagged,
        home_owner_lagged,
        econ_benefits_lagged,
        mastat_lagged,
        econ_emp_bin_fact,
        log_income,
        waves = t0_target
      ) |> data.table::as.data.table()
  }
  
  wide_data <- set_exposure(wide_data, 
                            exposure = econ_dist_bin)
  
  return(wide_data)
}
