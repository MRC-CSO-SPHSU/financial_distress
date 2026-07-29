# MAIHDA via hierarchical (cluster) TMLE.
#
# Estimand: the cluster-level PATE E[Yc(1) - Yc(0)], where the "cluster" is the
# intersectional stratum (sex x education x ethnicity) and Yc_j is the stratum mean
# outcome. Each stratum contributes equally regardless of size, which is the whole
# point relative to the individual-level ATE in fit_tmle_one().
#
# Estimator: Balzer et al. 2018, vendored verbatim in R/helpers_cluster_tmle.R.
# One fit per imputed dataset; branch over imp_idx and pool with pool_tmle_cluster().

run_tmle_cluster <- function(strata_data, imp_idx, sl_libs.Q, sl_libs.g,
                             outcome    = c("MCS", "PCS"),
                             clusterid  = "strata_id",
                             use_sl     = TRUE) {
  outcome <- match.arg(outcome)

  assign("SL.xgboost.tmle", SL.xgboost.tmle, envir = globalenv())
  assign("SL.glmnet.tmle",  SL.glmnet.tmle,  envir = globalenv())

  dat <- strata_data[[imp_idx]]

  # Individual-level confounders.
  ind.adj <- c(if (outcome == "MCS") "sf12mcs_dv_base" else "sf12pcs_dv_base",
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

  missing_adj <- setdiff(c(ind.adj, clusterid, "econ_dist_bin_0"), names(dat))
  if (length(missing_adj)) {
    stop("run_tmle_cluster: missing columns for outcome '", outcome, "': ",
         paste(missing_adj, collapse = ", "),
         "\n  (the cluster id comes from strata_creation(); the adjustment columns ",
         "come from the make_wide() call for this outcome in build_data().)")
  }

  outcome_col <- if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"

  # Cluster id
  id <- as.integer(as.factor(dat[[clusterid]]))

  # Exposure
  A <- as.integer(as.character(dat$econ_dist_bin_0))

  # Rescaling outcome
  y_raw <- dat[[outcome_col]]
  y_min <- min(y_raw, na.rm = TRUE)
  y_max <- max(y_raw, na.rm = TRUE)
  y_rng <- y_max - y_min

  # Factors must become dummies for the function
  Wmat <- model.matrix(~ ., dat[ind.adj])[, -1, drop = FALSE]
  colnames(Wmat) <- make.names(colnames(Wmat), unique = TRUE)

  n_j <- table(id)

  data_tmle <- data.frame(
    id    = id,
    A     = A,
    Y     = (y_raw - y_min) / y_rng,
    alpha = 1 / as.numeric(n_j[as.character(id)]),
    Wmat,
    check.names = FALSE
  )

  adj <- colnames(Wmat)

  fit <- do.estimation.inference(
    psi             = NA,
    data            = data_tmle,
    ind.adj         = adj,
    clust.adj       = NULL,
    Qinit.Indv      = TRUE,
    work.model      = TRUE,
    QAdj            = adj,
    gAdj            = adj,
    prob.txt        = 0.5,
    Do.SuperLearner = use_sl,
    Do.AdaptivePrespec = FALSE,
    SL.library.Q    = sl_libs.Q,
    SL.library.g    = sl_libs.g,
    verbose         = FALSE
  )

  # Back-transform outcome
  tibble::tibble(
    imp      = imp_idx,
    outcome  = outcome,
    n_strata = length(unique(id)),
    n_obs    = nrow(data_tmle),
    mean1    = fit$Risk1 * y_rng + y_min,
    mean0    = fit$Risk0 * y_rng + y_min,
    ate      = fit$RiskDiff * y_rng,
    var      = fit$var * y_rng^2,
    QAdj     = fit$QAdj,
    gAdj     = fit$gAdj
  )
}


