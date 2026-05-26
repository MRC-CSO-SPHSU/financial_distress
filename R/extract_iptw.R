# G-computation marginal means from the IPTW-weighted lm, pooled across
# imputations via Rubin's rules. Lifts the extract_iptw helper + its caller
# in report/05_imputation.qmd verbatim, with `a_levels`/`strategies` constructed
# inside the function from `wide_data` so the call has no implicit deps.
extract_iptw <- function(iptw_fit, wide_mids, wide_data, prefix = "iptw_sw") {
  a_levels <- levels(wide_data$econ_dist_bin_0)
  strategies <- tidyr::expand_grid(
    econ_dist_bin_0 = factor(a_levels, levels = a_levels),
    econ_dist_bin_1 = factor(a_levels, levels = a_levels),
    econ_dist_bin_2 = factor(a_levels, levels = a_levels)
  )
  data_list <- mice::complete(wide_mids, action = "all")
  mira_obj  <- iptw_fit

  m     <- length(mira_obj$analyses)
  coefs <- purrr::map(mira_obj$analyses, coef)
  vcovs <- purrr::map(mira_obj$analyses, vcov)
  b     <- purrr::reduce(coefs, `+`) / m
  W     <- purrr::reduce(vcovs, `+`) / m
  B     <- purrr::map(coefs, \(bk) tcrossprod(bk - b)) |> purrr::reduce(`+`) / (m - 1)
  V     <- W + (1 + 1 / m) * B
  rhs   <- delete.response(terms(mira_obj$analyses[[1]]))

  build_contrast <- function(a0, a1, a2) {
    purrr::map(data_list, \(dat) {
      cf <- dat |>
        dplyr::mutate(
          econ_dist_bin_0 = factor(as.character(a0), levels = a_levels),
          econ_dist_bin_1 = factor(as.character(a1), levels = a_levels),
          econ_dist_bin_2 = factor(as.character(a2), levels = a_levels)
        )
      colMeans(model.matrix(rhs, data = cf))
    }) |>
      purrr::reduce(`+`) / length(data_list)
  }

  L <- do.call(rbind, purrr::pmap(
    list(strategies$econ_dist_bin_0,
         strategies$econ_dist_bin_1,
         strategies$econ_dist_bin_2),
    build_contrast
  ))

  est <- as.numeric(L %*% b)
  se  <- sqrt(diag(L %*% V %*% t(L)))

  strategies |>
    dplyr::transmute(
      intervention = paste(as.integer(econ_dist_bin_0) - 1L,
                           as.integer(econ_dist_bin_1) - 1L,
                           as.integer(econ_dist_bin_2) - 1L, sep = "-"),
      "{prefix}_effect" := round(est, 3),
      "{prefix}_se"     := round(se, 3),
      "{prefix}_ll"     := round(est - 1.96 * se, 3),
      "{prefix}_ul"     := round(est + 1.96 * se, 3)
    ) |>
    dplyr::select("intervention", dplyr::starts_with(prefix))
}
