pool_ltmle <- function(ltmle_one, regime_labels) {
  ltmle_fits <- split(ltmle_one, regime_labels)
  ltmle_fits <- ltmle_fits[unique(regime_labels)]

  purrr::imap_dfr(ltmle_fits, function(fits_per_imp, label) {
    trs <- purrr::map(fits_per_imp,
                      \(fit) summary(fit, estimator = "tmle")$treatment)
    Q   <- purrr::map_dbl(trs, "estimate")
    U   <- purrr::map_dbl(trs, "std.dev")^2
    pooled <- mice::pool.scalar(Q = Q, U = U, n = Inf, k = 1)
    se  <- sqrt(pooled$t)
    tibble::tibble(
      intervention = label,
      ltmle_effect = pooled$qbar,
      ltmle_se     = se,
      ltmle_ll     = pooled$qbar - 1.96 * se,
      ltmle_ul     = pooled$qbar + 1.96 * se
    )
  }) |>
    dplyr::mutate(dplyr::across(
      c(ltmle_effect, ltmle_se, ltmle_ll, ltmle_ul),
      ~ round(.x * 100, 3)
    ))
}
