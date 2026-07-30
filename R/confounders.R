# Shared confounder set (W) for the marginal TMLE and the DR-learner GATE arm.
#
# Single source of truth: R/fit_tmle_one.R and R/meta_learner.R both call this,
# so the two adjustment sets cannot drift apart. A divergent W would mean the
# GATEs and the headline ATE rest on different identification claims
# (design 2026-07-30, 3.2). All measured pre-exposure (base = wave 1,
# lagged = wave 2, _0 = wave 3 contemporaneous).
confounders <- function(outcome = c("MCS", "PCS")) {
  outcome <- match.arg(outcome)
  c("sex_dv_base",
    "hiqual_dv_fact_base",
    "race_base",
    if (outcome == "MCS") "sf12mcs_dv_base" else "sf12pcs_dv_base",
    "age_dv_0",
    "gor_dv_fact_0",
    "econ_dist_bin_lagged_0",
    if (outcome == "MCS") "pcs_lagged_0" else "mcs_lagged_0",
    "dnc_lagged_0",
    "home_owner_lagged_0",
    "econ_benefits_lagged_0",
    "mastat_lagged_0",
    "econ_emp_bin_fact_0",
    "log_income_0")
}
