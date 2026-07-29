# Pool MAIHDA cluster-TMLE estimates across imputations with Rubin's rules.

pool_tmle_cluster <- function(tmle_maihda_one) {

  fits <- dplyr::bind_rows(tmle_maihda_one)

  J <- fits$n_strata[1]
  if (!all(fits$n_strata == J)) {
    warning("stratum count varies across imputations: ",
            paste(sort(unique(fits$n_strata)), collapse = "/"),
            " - a stratum is empty in some imputed datasets.")
  }

  estimands <- tibble::tribble(
    ~col,    ~estimand,     ~type,
    "mean0", "No distress", "mean",
    "mean1", "Distress",    "mean",
    "ate",   "ATE",         "ate"
  )

  purrr::pmap_dfr(estimands, function(col, estimand, type) {
    if (type == "mean") {
      return(tibble::tibble(
        estimand = estimand,
        type     = type,
        effect   = mean(fits[[col]]),
        se       = NA_real_,
        ll       = NA_real_,
        ul       = NA_real_,
        df       = NA_real_,
        fmi      = NA_real_,
        n_strata = J
      ))
    }

    pooled <- mice::pool.scalar(Q = fits[[col]], U = fits$var, n = J, k = 2)

    se <- sqrt(pooled$t)
    cutoff <- if (is.finite(pooled$df) && pooled$df > 0) {
      stats::qt(0.975, df = pooled$df)
    } else {
      stats::qnorm(0.975)
    }

    tibble::tibble(
      estimand = estimand,
      type     = type,
      effect   = pooled$qbar,
      se       = se,
      ll       = pooled$qbar - cutoff * se,
      ul       = pooled$qbar + cutoff * se,
      df       = pooled$df,
      fmi      = pooled$fmi,
      n_strata = J
    )
  })
}
