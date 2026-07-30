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
