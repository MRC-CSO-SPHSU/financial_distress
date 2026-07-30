# Intersectional strata (sex x race x education) for the DR-learner GATE arm.
#
# Pure df -> df helper (design 2026-07-30, 3.8): called inside estimate_cate()
# on the same mice::complete() frame that produced psi, so scores and strata can
# never come from different row orders (make_wide() drops pidp, helpers.R:36).
# Keeps the 100*sex + 10*race + hiqual id and interaction() label of the
# original strata_creation().
make_strata <- function(df) {
  needed <- c("sex_dv_base", "race_base", "hiqual_dv_fact_base")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols)) {
    stop("make_strata: missing stratum column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  dplyr::mutate(df,
    # intersectional strata id: unique cell
    strata_id = 100 * as.numeric(sex_dv_base) +
                 10 * as.numeric(race_base) +
                      as.numeric(hiqual_dv_fact_base),
    strata_id = as.factor(strata_id),
    # human-readable stratum name, e.g. "Female:White:High"
    strata_label = interaction(sex_dv_base, race_base, hiqual_dv_fact_base,
                               sep = ":", drop = TRUE)
  )
}
