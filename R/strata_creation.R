strata_creation <- function(mids) {
  # create strata ids after imputation
fits <- purrr::map(seq_len(mids$m), function(i) {
    dat_i <- mice::complete(mids, action = i) |>
      dplyr::mutate(
        # intersectional strata id: unique cell
        id = 100 * as.numeric(sex_dv_base) +
                     10 * as.numeric(race_base) +
                          as.numeric(hiqual_dv_fact_base),
        id = as.factor(id),
        # human-readable stratum name, e.g. "female:white:high"
        strata_label = interaction(sex_dv_base, race_base, hiqual_dv_fact_base,
                                   sep = ":", drop = TRUE)
      ) }
    )
  # counts per stratum (id + label) for each imputed dataset
  strata_tables <- purrr::map(fits, function(dat_i) {
    dplyr::count(dat_i, id, strata_label)
  })

  return(list(
    fits = fits,
    strata_tables = strata_tables
  ))
}