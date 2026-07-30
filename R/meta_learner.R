# DR-learner GATEs over sex x race x education -- score primitives.
# Design: .claude/plans/2026-07-30-dr-learner-gate-design.md (2, 3.6, 3.7).
# estimate_cate() below (added with the full rewrite) consumes these.

# Positivity bound. Matches tmle's default gbound so psi is comparable to
# tmle_results (design 3.7).
CATE_EPS <- 0.025

bound_propensity <- function(g_hat, eps = CATE_EPS) {
  pmin(pmax(g_hat, eps), 1 - eps)
}

# Negated AIPW / DR-learner score, on the Y(0) - Y(1) scale (design 2):
# Y(0) is the no-distress potential outcome, so positive psi = MCS points LOST
# to financial distress. THE SIGN IS DELIBERATE and this is the single flip
# site; estimate_cate() undoes it once (tau <- -fitted) for the BLP test.
# Do not "correct" it.
dr_pseudo_outcome <- function(A, Y, mu0_hat, mu1_hat, g_hat) {
  stopifnot(length(A) == length(Y),
            length(mu0_hat) == length(Y),
            length(mu1_hat) == length(Y),
            length(g_hat) == length(Y))
  if (anyNA(g_hat)) {
    stop("dr_pseudo_outcome: g_hat contains NA/NaN -- a degenerate ",
         "SuperLearner propensity fit. Cannot bound or use it.")
  }
  if (any(g_hat <= 0 | g_hat >= 1)) {
    stop("dr_pseudo_outcome: g_hat must be strictly inside (0, 1); ",
         "bound it with bound_propensity() first.")
  }
  (mu0_hat - mu1_hat) +
    (1 - A) / (1 - g_hat) * (Y - mu0_hat) -
     A      /      g_hat  * (Y - mu1_hat)
}

# Out-of-fold ensemble predictions from a SuperLearner fit's $Z matrix, with
# the design-3.6 hard assertions. $Z rows follow the *input* row order and
# carry no dimnames (verified against SuperLearner 2.0.40); a silent
# misalignment here corrupts every number downstream without erroring, hence
# stop() rather than tests alone. $SL.predict is the full-data refit scoring
# its own training rows -- never use it here.
sl_oof_predictions <- function(fit, n_expected, label) {
  if (is.null(fit$Z) || nrow(fit$Z) != n_expected) {
    stop("estimate_cate [", label, "]: nrow($Z) = ",
         if (is.null(fit$Z)) "NULL" else nrow(fit$Z),
         " but the training set had ", n_expected, " rows.")
  }
  if (length(fit$coef) != ncol(fit$Z)) {
    stop("estimate_cate [", label, "]: length($coef) = ", length(fit$coef),
         " != ncol($Z) = ", ncol(fit$Z), ".")
  }
  oof <- drop(fit$Z %*% fit$coef)
  if (anyNA(oof)) {
    stop("estimate_cate [", label, "]: out-of-fold ensemble prediction contains NA.")
  }
  oof
}

# DR-learner GATEs over sex x race x education, one imputed dataset
# (design 2026-07-30, 3). Pooled across imputations by pool_cate().
estimate_cate <- function(wide_mids, imp_idx, sl_libs, outcome) {
  # SuperLearner resolves learner names by string against globalenv() on fresh
  # batchtools workers -- same shim as fit_tmle_one(). Do not clean up.
  assign("SL.xgboost.tmle", SL.xgboost.tmle, envir = globalenv())
  assign("SL.glmnet.tmle",  SL.glmnet.tmle,  envir = globalenv())

  ## -- 3.1 complete the data -------------------------------------------------
  dat <- mice::complete(wide_mids, action = imp_idx)

  ## -- 3.2 confounders: byte-identical to the marginal TMLE's ---------------
  W <- confounders(outcome)

  ## -- 3.3 numeric design matrix (SL.glmnet/SL.xgboost cannot take factors) --
  Wmat <- as.data.frame(model.matrix(~ ., dat[W])[, -1])
  names(Wmat) <- make.names(names(Wmat), unique = TRUE)

  ## -- 3.4 treatment and outcome ---------------------------------------------
  A <- as.integer(as.character(dat$econ_dist_bin_0))   # factor, build_data.R:45
  Y <- dat[[if (outcome == "MCS") "sf12mcs_dv_0" else "sf12pcs_dv_0"]]
  n <- nrow(dat)

  ## -- 3.5 nuisances: T-style, project sl_libs -------------------------------
  mu0 <- SuperLearner::SuperLearner(
    Y = Y[A == 0], X = Wmat[A == 0, , drop = FALSE], family = gaussian(),
    SL.library = sl_libs, cvControl = list(V = 10))
  mu1 <- SuperLearner::SuperLearner(
    Y = Y[A == 1], X = Wmat[A == 1, , drop = FALSE], family = gaussian(),
    SL.library = sl_libs, cvControl = list(V = 10))
  gm  <- SuperLearner::SuperLearner(
    Y = A, X = Wmat, family = binomial(),
    SL.library = sl_libs, method = "method.NNloglik",
    cvControl = list(V = 10))

  ## -- 3.6 honest predictions without outer folds ----------------------------
  # Own-arm rows: $Z out-of-fold ensemble (hard-asserted). Other-arm rows:
  # predict() is honest because the fit never saw them. $SL.predict would be
  # the full-data refit scoring its own training rows -- never use it.
  mu0_hat <- numeric(n)
  mu1_hat <- numeric(n)
  mu0_hat[A == 0] <- sl_oof_predictions(mu0, sum(A == 0), "mu0")
  mu0_hat[A == 1] <- predict(mu0, newdata = Wmat[A == 1, , drop = FALSE])$pred
  mu1_hat[A == 1] <- sl_oof_predictions(mu1, sum(A == 1), "mu1")
  mu1_hat[A == 0] <- predict(mu1, newdata = Wmat[A == 0, , drop = FALSE])$pred
  g_hat <- sl_oof_predictions(gm, n, "g")

  ## -- 3.7 positivity bound and pseudo-outcome (THE sign-flip site) ----------
  g_hat <- bound_propensity(g_hat)
  psi   <- dr_pseudo_outcome(A, Y, mu0_hat, mu1_hat, g_hat)

  ## -- 3.8 strata and saturated projection -----------------------------------
  dat_s   <- make_strata(dat)
  proj_df <- data.frame(psi          = psi,
                        strata_id    = dat_s$strata_id,
                        strata_label = dat_s$strata_label)

  proj  <- estimatr::lm_robust(psi ~ 0 + strata_id, data = proj_df,
                               se_type = "HC2")
  cells <- names(coef(proj))
  lh    <- car::linearHypothesis(proj, paste0(cells[-1], " = ", cells[1]),
                                 test = "F")
  wald  <- tibble::tibble(imp = imp_idx,
                          F   = lh$F[2],
                          df1 = lh$Df[2],
                          df2 = lh$Res.Df[2],
                          p   = lh[2, "Pr(>F)"])

  ## -- 3.9 BLP global heterogeneity test (conventional Y(1)-Y(0) scale) ------
  m_hat <- g_hat * mu1_hat + (1 - g_hat) * mu0_hat  # E[Y|W]; no 4th SL fit
  tau   <- -proj$fitted.values                      # undo the 3.7 flip, once
  blp_fit <- lm(I(Y - m_hat) ~ 0 + I(A - g_hat) +
                  I((tau - mean(tau)) * (A - g_hat)))
  blp_ct  <- lmtest::coeftest(blp_fit, vcov = sandwich::vcovHC, type = "HC3")
  blp <- tibble::tibble(
    imp      = imp_idx,
    term     = c("beta1_ate", "beta2_hte"),
    estimate = unname(blp_ct[, "Estimate"]),
    se       = unname(blp_ct[, "Std. Error"]),
    p        = unname(blp_ct[, "Pr(>|t|)"])
  )

  ## -- 3.10 per-cell diagnostics ---------------------------------------------
  diag_tbl <- proj_df |>
    dplyr::mutate(A = A, g = g_hat) |>
    dplyr::group_by(strata_id, strata_label) |>
    dplyr::summarise(
      n_j              = dplyr::n(),
      n_exposed        = sum(A),
      pct_exposed      = 100 * mean(A),
      min_g            = min(g),
      median_g         = stats::median(g),
      share_g_at_bound = mean(g <= CATE_EPS | g >= 1 - CATE_EPS),
      .groups          = "drop"
    ) |>
    dplyr::mutate(strata_id = as.character(strata_id))

  est_tbl <- tibble::tibble(
    strata_id = sub("^strata_id", "", cells),
    estimate  = unname(coef(proj)),
    se        = unname(proj$std.error)
  )

  gate <- diag_tbl |>
    dplyr::inner_join(est_tbl, by = "strata_id") |>
    dplyr::mutate(imp = imp_idx, .before = 1)

  if (nrow(gate) != length(cells)) {
    stop("estimate_cate: projection produced ", length(cells),
         " cells but the diagnostics join kept ", nrow(gate),
         " -- cell ids disagree between coef names and make_strata().")
  }

  ## -- 3.11 return ------------------------------------------------------------
  list(
    gate = gate,
    blp  = blp,
    ate  = tibble::tibble(imp      = imp_idx,
                          estimate = mean(psi),
                          se       = stats::sd(psi) / sqrt(n)),
    wald = wald
  )
}
