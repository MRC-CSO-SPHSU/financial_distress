# Pool single-point TMLE estimates across imputations with Rubin's rules.
# For each estimand (the two counterfactual marginal means and the ATE) we
# pull the point estimate and its IC variance from every imputation and combine
# them with mice::pool.scalar.

pool_tmle <- function(tmle_one) {
  summaries <- purrr::map(
    tmle_one, \(fit) summary(fit, estimator = "tmle")$effect.measures
  )

  estimands <- tibble::tribble(
    ~measure,    ~estimand,     ~type,
    "control",   "No distress", "mean",
    "treatment", "Distress",    "mean",
    "ATE",       "ATE",         "ate"
  )

  purrr::pmap_dfr(estimands, function(measure, estimand, type) {
    Q <- purrr::map_dbl(summaries, \(s) s[[measure]]$estimate)
    U <- purrr::map_dbl(summaries, \(s) s[[measure]]$std.dev)^2
    pooled <- mice::pool.scalar(Q = Q, U = U, n = Inf, k = 1)
    se <- sqrt(pooled$t)
    tibble::tibble(
      estimand = estimand,
      type     = type,
      effect   = pooled$qbar,
      se       = se,
      ll       = pooled$qbar - 1.96 * se,
      ul       = pooled$qbar + 1.96 * se
    )
  }) |>
    dplyr::mutate(dplyr::across(c(effect, se, ll, ul), ~ .x * 100))
}
