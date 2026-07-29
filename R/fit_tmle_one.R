# Single-point TMLE (via the tmle package) for one imputed database.
fit_tmle_one <- function(wide_mids, imp_idx, sl_libs, outcome) {
  # Assign custom SL wrappers to globalenv() so SuperLearner resolves them on each batchtools worker.
  assign("SL.xgboost.tmle", SL.xgboost.tmle, envir = globalenv())
  assign("SL.glmnet.tmle",  SL.glmnet.tmle,  envir = globalenv())

  # Confounders (W), all measured pre-exposure (wave 3 / baseline).
  W <- c("sex_dv_base", 
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

  dat <- mice::complete(wide_mids, action = imp_idx)
  A    <- as.integer(as.character(dat$econ_dist_bin_0))
  Y    <- dat[[if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"]]
  Wmat <- model.matrix(~ ., dat[W])[, -1]

  tmle::tmle(
    Y            = Y,
    A            = A,
    W            = Wmat,
    Q.SL.library = sl_libs,
    g.SL.library = sl_libs,
    cvQinit      = TRUE,
    family       = "gaussian", # for continuous outcomes
    fluctuation = "logistic" # to bound the outcome to [0,1] for the logistic fluctuation step
  )
}
