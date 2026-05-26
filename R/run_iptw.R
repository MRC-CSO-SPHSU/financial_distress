run_iptw <- function(wide_mids) {
  # Fit denominator models, build per-imputation IPTW weights, and run the
  # weighted outcome model inside one with() so everything stays aligned with
  # wide_mids. Result is a mira ready for mice::pool().
  with(wide_mids, {
    # Denominators: P(A_t | baseline, time-varying confounder, prior treatment)
    d0 <- glm(econ_dist_bin_0 ~ sex_dv_base + hiqual_dv_base + race_base + pcs_lagged_0,
              family = binomial())
    d1 <- glm(econ_dist_bin_1 ~ sex_dv_base + hiqual_dv_base + race_base + pcs_lagged_1 * econ_dist_bin_0,
              family = binomial())
    d2 <- glm(econ_dist_bin_2 ~ sex_dv_base + hiqual_dv_base + race_base + pcs_lagged_2 * econ_dist_bin_1,
              family = binomial())

    # Numerators: P(A_t | prior treatment) — stabilising marginals
    n0 <- glm(econ_dist_bin_0 ~ 1, family = binomial())
    n1 <- glm(econ_dist_bin_1 ~ econ_dist_bin_0, family = binomial())
    n2 <- glm(econ_dist_bin_2 ~ econ_dist_bin_1, family = binomial())

    sw <- purrr::reduce(
      purrr::pmap(
        list(list(n0, n1, n2),
             list(d0, d1, d2),
             list(econ_dist_bin_0, econ_dist_bin_1, econ_dist_bin_2)),
        \(n, d, a) ifelse(a == 1,
                          fitted(n) / fitted(d),
                          (1 - fitted(n)) / (1 - fitted(d)))
      ),
      `*`
    )

    lm(sf12mcs_dv_2 ~ econ_dist_bin_0 + econ_dist_bin_1 + econ_dist_bin_2 +
         sex_dv_base + hiqual_dv_base + race_base +
         econ_dist_bin_0 * econ_dist_bin_1 * econ_dist_bin_2,
       weights = sw)
  })
}
