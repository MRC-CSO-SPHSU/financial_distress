# Meta-learner (R-learner) for heterogeneous treatment effects (HTEs) estimation
# across levels of strata (hiqual x sex x race)
estimate_cate <- function(wide_mids, imp_idx, sl_libs, outcome) {
  # Assign custom SL wrappers to globalenv() so SuperLearner resolves them on each batchtools worker.
  assign("SL.xgboost.tmle", SL.xgboost.tmle, envir = globalenv())
  assign("SL.glmnet.tmle",  SL.glmnet.tmle,  envir = globalenv())

  data.learner <- mice::complete(wide_mids, action = imp_idx)

  # Calculate outcome model
  outcome_model <- SuperLearner::SuperLearner(
    Y = data.learner[[if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"]],
    X = data.learner[, c("sex_dv_base", 
                         "race_base", 
                         "hiqual_dv_fact_base", 
                         "age_dv_0", 
                         "gor_dv_fact_0", 
                         "econ_emp_bin_fact_0", 
                         "econ_dist_bin_lagged_0", 
                         "log_income_0",
                         "dnc_lagged_0",
                         "home_owner_lagged_0",
                         "econ_benefits_lagged_0",
                         "mastat_lagged_0")],
    method = "method.NNLS",
    SL.library = sl_libs,
    family = gaussian(),
    cvControl = list(V = 10)
  )

  m.hat <- outcome_model$SL.predict

  # Calculate propensity score model
  propensity_model <- SuperLearner::SuperLearner(
    Y = as.integer(as.character(data.learner$econ_dist_bin_0)),
    X = data.learner[, c("sex_dv_base", 
                         "race_base", 
                         "hiqual_dv_fact_base", 
                         "age_dv_0", 
                         "gor_dv_fact_0", 
                         "econ_emp_bin_fact_0", 
                         "econ_dist_bin_lagged_0", 
                         "log_income_0",
                         "dnc_lagged_0",
                         "home_owner_lagged_0",
                         "econ_benefits_lagged_0",
                         "mastat_lagged_0")],
    method = "method.NNLS",
    SL.library = sl_libs,
    family = binomial(),
    cvControl = list(V = 10)
  )

  ps.hat <- propensity_model$SL.predict[,2]

  # Ensure positivity
  epsilon <- 0.01
  ps.hat.r <- ifelse(ps.hat.r < epsilon, epsilon, ifelse(ps.hat.r > 1 - epsilon, 1 - epsilon, ps.hat.r))

  resid.treat <- data.learner$econ_dist_bin_0 - ps.hat
  resid.out <- data.learner[[if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"]] - m.hat

  # compute pseudo outcome
  pseudo.outcome <- resid.out / resid.treat

  # compute weight
  W <- resid.treat ^2

  # Regress pseudo-outcome on covariates using weights w
  tau.r <- SuperLearner::SuperLearner(
    Y = resid.out / resid.treat,
    X = data.learner[, c("sex_dv_base", 
                         "race_base", 
                         "hiqual_dv_fact_base", 
                         "age_dv_0", 
                         "gor_dv_fact_0", 
                         "econ_emp_bin_fact_0", 
                         "econ_dist_bin_lagged_0", 
                         "log_income_0",
                         "dnc_lagged_0",
                         "home_owner_lagged_0",
                         "econ_benefits_lagged_0",
                         "mastat_lagged_0")],
    method = "method.NNLS",
    SL.library = sl_libs,
    family = gaussian(),
    cvControl = list(V = 10)
  )
}