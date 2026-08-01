# Random-effects meta-regression by REML, written from the mathematics.
# No package dependency: the pipeline must not carry metafor to the cluster.
# Verified against metafor 4.8.0 to ~1e-12 (design 2026-07-31, 4.4).
#
# Model:  y_j = x_j' beta + u_j + e_j,   u_j ~ N(0, tau2),  e_j ~ N(0, v_j)
#   =>    y   ~ N(X beta, diag(v) + tau2 I),   v KNOWN (squared GATE SEs).
#
# Given tau2 this is weighted least squares with w_j = 1/(v_j + tau2). tau2 is
# the ONLY quantity needing numerical work, and it is a scalar.

# Weighted-LS pieces at a given tau2.
.wls <- function(yi, vi, X, tau2) {
  wi   <- 1 / (vi + tau2)
  XtW  <- crossprod(X, diag(wi, nrow = length(yi)))
  XtWX <- XtW %*% X
  vb0  <- solve(XtWX)
  beta <- vb0 %*% (XtW %*% yi)
  r    <- as.vector(yi - X %*% beta)
  list(wi = wi, vb0 = vb0, beta = beta, r = r, RSS = sum(wi * r^2),
       logdet = as.numeric(determinant(XtWX, logarithm = TRUE)$modulus))
}

# REML profile log-likelihood, up to an additive constant free of tau2:
#   l(tau2) = -1/2 sum log(v_j + tau2) - 1/2 log|X'WX| - 1/2 sum w_j r_j^2
.ll_reml <- function(tau2, yi, vi, X) {
  f <- .wls(yi, vi, X, tau2)
  -0.5 * sum(log(vi + tau2)) - 0.5 * f$logdet - 0.5 * f$RSS
}

# REML score: dl/dtau2 = -1/2 tr(P) + 1/2 y'PPy, with the REML projection
#   P = W - WX (X'WX)^-1 X'W.   Its root is the interior REML estimate.
.score_reml <- function(tau2, yi, vi, X) {
  P  <- .P_matrix(yi, vi, X, tau2)
  0.5 * (sum((P %*% yi)^2) - sum(diag(P)))
}

# REML projection matrix at a given tau2 (shared by .score_reml and se(tau2)).
.P_matrix <- function(yi, vi, X, tau2) {
  W  <- diag(1 / (vi + tau2), nrow = length(yi))
  WX <- W %*% X
  W - WX %*% solve(crossprod(X, WX)) %*% t(WX)
}

rma_reml <- function(yi, vi, X, knha = TRUE, level = 0.95) {
  k <- length(yi); p <- ncol(X)
  stopifnot(k > p, all(vi > 0), nrow(X) == k, !anyNA(yi), !anyNA(vi))

  ## tau2: bounded scalar maximisation, then polished on the score root.
  tau2_max <- 100 * max(vi)
  opt  <- optimize(.ll_reml, c(0, tau2_max), yi = yi, vi = vi, X = X,
                   maximum = TRUE, tol = .Machine$double.eps^0.5)
  tau2 <- opt$maximum
  if (.ll_reml(0, yi, vi, X) >= opt$objective) {
    tau2 <- 0                                   # REML boundary solution, exactly 0
  } else {
    # optimize()'s golden section only reaches sqrt(eps); the score root reaches ~1e-12.
    lo <- max(0, tau2 * 0.5); hi <- min(tau2_max, tau2 * 2 + 1e-8)
    s_lo <- .score_reml(lo, yi, vi, X); s_hi <- .score_reml(hi, yi, vi, X)
    if (is.finite(s_lo) && is.finite(s_hi) && s_lo * s_hi < 0)
      tau2 <- uniroot(.score_reml, c(lo, hi), yi = yi, vi = vi, X = X,
                      tol = .Machine$double.eps^0.75)$root
  }

  ## coefficients at the REML tau2
  f    <- .wls(yi, vi, X, tau2)
  dfs  <- k - p
  s2w  <- f$RSS / dfs                           # Knapp-Hartung scale factor
  vb   <- if (knha) s2w * f$vb0 else f$vb0
  se   <- sqrt(diag(vb))
  beta <- as.vector(f$beta)
  crit <- qt(1 - (1 - level) / 2, dfs)

  # Strip names to match metafor output format
  beta <- unname(beta)
  se   <- unname(se)

  list(k = k, p = p, dfs = dfs, tau2 = tau2, s2w = s2w,
       beta = beta, se = se, tval = beta / se,
       pval = 2 * pt(-abs(beta / se), dfs),
       ci.lb = beta - crit * se, ci.ub = beta + crit * se,
       vb = vb, vb0 = f$vb0, yi = yi, vi = vi, X = X)
}
