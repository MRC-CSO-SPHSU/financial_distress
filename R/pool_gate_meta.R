# Pool the MAIHDA decomposition across imputations with Rubin's rules.
# SHRINK-THEN-POOL: the whole second stage runs inside each imputation and
# Rubin's rules are applied afterwards, because tau2 is a complete-data
# estimand. Pooling the GATEs first instead shrinks tau2 (averaged estimates
# have less spread, Rubin SEs are larger) and cuts PCV by roughly 4x --
# measured, not hypothetical (design 2026-07-31, 3).
#
# Mirrors pool_cate()/pool_tmle(): pool.scalar(n = Inf, k = 1), 1.96 cutoffs.

# tau2 is variance-scaled and bounded below by zero, so it is pooled on the log
# scale. eps keeps a boundary tau2 = 0 out of log(0); it is small against the
# observed tau2 range (0.70-5.37) so it does not bias the back-transform.
POOL_TAU2_EPS <- 1e-6

.pool_pair <- function(Q, U) {
  pooled <- mice::pool.scalar(Q = Q, U = U, n = Inf, k = 1)
  se <- sqrt(pooled$t)
  tibble::tibble(estimate = pooled$qbar, se = se,
                 ll = pooled$qbar - 1.96 * se, ul = pooled$qbar + 1.96 * se)
}

# Delta method on log(tau2 + eps): se_log = se_tau2 / (tau2 + eps).
#
# CAUTION: back-transforming a mean of logs is a GEOMETRIC mean, so a single
# imputation with tau2 = 0 contributes log(1e-6) = -13.8 and drags the pooled
# tau2 to near zero however large the others are. Measured on a 3-imputation
# fixture: one boundary value pulled pooled tau2_main from ~0.15 to 0.0033 and
# PCV to 0.998 -- a spurious "entirely additive" reading. tau2 was interior in
# all 50 real imputations, so this does not bite today, but it would the moment
# the imputation model or m changes. Hence the warning in pool_gate_meta().
.pool_tau2 <- function(tau2, se_tau2, eps = POOL_TAU2_EPS) {
  Q <- log(tau2 + eps)
  U <- (se_tau2 / (tau2 + eps))^2
  pooled <- mice::pool.scalar(Q = Q, U = U, n = Inf, k = 1)
  se <- sqrt(pooled$t)
  tibble::tibble(estimate = exp(pooled$qbar) - eps,
                 ll = exp(pooled$qbar - 1.96 * se) - eps,
                 ul = exp(pooled$qbar + 1.96 * se) - eps)
}

pool_gate_meta <- function(gate_meta_one) {
  m <- length(gate_meta_one)

  scalars_all <- purrr::map_dfr(gate_meta_one, "scalars")
  coefs_all   <- purrr::map_dfr(gate_meta_one, "coefs")
  cells_all   <- purrr::map_dfr(gate_meta_one, "cells")
  loco_all    <- purrr::map_dfr(gate_meta_one, "loco")

  # A boundary tau2 = 0 in ANY imputation collapses the geometric mean (see
  # .pool_tau2). Loud, because the failure direction is "no heterogeneity found"
  # -- the most dangerous way for a decomposition to be silently wrong.
  n_tau2_zero <- sum(scalars_all$tau2_null == 0) + sum(scalars_all$tau2_main == 0)
  if (n_tau2_zero > 0) {
    warning("pool_gate_meta: tau2 hit the zero boundary in ", n_tau2_zero,
            " model fit(s) across ", m, " imputations. Log-scale pooling is a ",
            "geometric mean, so pooled tau2 is pulled toward zero and PCV ",
            "toward 1. Inspect gate_meta_results$tests before reporting PCV.")
  }

  t2_null <- .pool_tau2(scalars_all$tau2_null, scalars_all$se_tau2_null)
  t2_main <- .pool_tau2(scalars_all$tau2_main, scalars_all$se_tau2_main)
  mu_res  <- .pool_pair(scalars_all$mu, scalars_all$se_mu^2)

  # vt depends only on v_j and X, never on tau2, so averaging it is benign.
  vt_null_bar <- mean(scalars_all$vt_null)
  vt_main_bar <- mean(scalars_all$vt_main)

  # Extract scalar values before tibble constructor to avoid shadowing in
  # evaluation context (first assignment to 'mu' would rebind the variable).
  t2n_est <- t2_null$estimate[[1]]
  t2n_ll  <- t2_null$ll[[1]]
  t2n_ul  <- t2_null$ul[[1]]
  t2m_est <- t2_main$estimate[[1]]
  t2m_ll  <- t2_main$ll[[1]]
  t2m_ul  <- t2_main$ul[[1]]
  mu_est  <- mu_res$estimate[[1]]
  mu_se   <- mu_res$se[[1]]
  mu_ll   <- mu_res$ll[[1]]
  mu_ul   <- mu_res$ul[[1]]

  scalars <- tibble::tibble(
    tau2_null    = t2n_est,
    tau2_null_ll = t2n_ll, tau2_null_ul = t2n_ul,
    tau2_main    = t2m_est,
    tau2_main_ll = t2m_ll, tau2_main_ul = t2m_ul,
    # PCV from the POOLED tau2 values, never the mean of per-imputation PCVs:
    # individual imputations can give a negative PCV at J = 12 (REML lets the
    # additive model fit worse than the null). Reported as a point estimate
    # with no interval -- its sampling uncertainty is not credibly quantifiable
    # from 12 cells.
    pcv     = (t2n_est - t2m_est) / t2n_est,
    I2_null = 100 * t2n_est / (vt_null_bar + t2n_est),
    I2_main = 100 * t2m_est / (vt_main_bar + t2m_est),
    mu      = mu_est, se_mu = mu_se, mu_ll = mu_ll, mu_ul = mu_ul
  )

  coefs <- coefs_all |>
    dplyr::group_by(term) |>
    dplyr::group_modify(function(d, key) .pool_pair(d$estimate, d$se^2)) |>
    dplyr::ungroup()

  pool_col <- function(d, est, se, prefix) {
    out <- .pool_pair(d[[est]], d[[se]]^2)
    stats::setNames(out, paste0(prefix, c("", "_se", "_ll", "_ul")))
  }

  cells <- cells_all |>
    dplyr::group_by(strata_id, strata_label) |>
    dplyr::group_modify(function(d, key) {
      dplyr::bind_cols(
        pool_col(d, "estimate",      "se",           "gate"),
        pool_col(d, "additive_pred", "se_pred",      "additive"),
        pool_col(d, "blup",          "se_blup",      "blup"),
        pool_col(d, "del_resid",     "se_del_resid", "resid"),
        tibble::tibble(
          n_j_min          = min(d$n_j),
          n_j_max          = max(d$n_j),
          pct_exposed      = mean(d$pct_exposed),
          min_g            = min(d$min_g),
          median_g         = stats::median(d$median_g),
          share_g_at_bound = mean(d$share_g_at_bound)
        )
      )
    }) |>
    dplyr::ungroup()

  loco <- loco_all |>
    dplyr::group_by(dropped_id, dropped_label) |>
    dplyr::group_modify(function(d, key) {
      n <- .pool_tau2(d$tau2_null_d, d$se_tau2_null_d)
      a <- .pool_tau2(d$tau2_main_d, d$se_tau2_main_d)
      tibble::tibble(tau2_null_d = n$estimate, tau2_main_d = a$estimate,
                     pcv_d = (n$estimate - a$estimate) / n$estimate)
    }) |>
    dplyr::ungroup()

  # Q and F statistics do not Rubin-pool. Return the per-imputation rows and let
  # the report summarise median and range -- the convention pool_cate() already
  # uses for its Wald test.
  tests <- scalars_all |>
    dplyr::select(imp, QE_null, QE_main, QM, QMp, pcv)

  list(scalars = scalars, coefs = coefs, cells = cells,
       loco = loco, tests = tests, m = m, n_tau2_zero = n_tau2_zero)
}
