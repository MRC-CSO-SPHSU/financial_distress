# `wide_data_mi` mirrors the original chunk's parameter name. In report/05_imputation.qmd
# the symbol was not defined in the file, so the targets DAG passes `wide_data` to this
# slot — behavior-equivalent to the working version of this chunk in code/04_full_waves.qmd.
run_gformula <- function(wide_mids, wide_data_mi, M = 50) {
  regimes <- dplyr::tibble(a = 0:1, b = 0:1, c = 0:1) |>
    expand.grid() |>
    dplyr::arrange(a, b, c) |>
    t() |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    unclass() |> unname()

  predictor_matrix <- make_predictor_matrix(wide_data_mi)

  ## pcs_lagged_t predicts pcs_lagged_t+1
  predictor_matrix["pcs_lagged_1", "pcs_lagged_0"] <- 1
  predictor_matrix["pcs_lagged_2", "pcs_lagged_1"] <- 1

  ## econ_dist_bin_t predicts econ_dist_bin_t+1
  predictor_matrix["econ_dist_bin_1", "econ_dist_bin_0"] <- 1
  predictor_matrix["econ_dist_bin_2", "econ_dist_bin_1"] <- 1

  ## sf_12mcs_dv_t only predicts sf_12mcs_dv_t+1 (not other vars)
  predictor_matrix[, "sf12mcs_dv_0"] <- 0
  predictor_matrix[, "sf12mcs_dv_1"] <- 0
  predictor_matrix[, "sf12mcs_dv_2"] <- 0
  predictor_matrix["sf12mcs_dv_1", "sf12mcs_dv_0"] <- 1
  predictor_matrix["sf12mcs_dv_2", "sf12mcs_dv_1"] <- 1

  ## sf_12mcs_dv_t predicts econ_dist_bin_t+1
  predictor_matrix["econ_dist_bin_1", "sf12mcs_dv_0"] <- 1
  predictor_matrix["econ_dist_bin_2", "sf12mcs_dv_1"] <- 1

  ## sf_12mcs_dv_t predicts pcs_lagged_t+1
  predictor_matrix["pcs_lagged_1", "sf12mcs_dv_0"] <- 1
  predictor_matrix["pcs_lagged_2", "sf12mcs_dv_1"] <- 1

  ## pipd does not predict or is predicted
  predictor_matrix["pipd", ] <- 0
  predictor_matrix[, "pipd"] <- 0
  
  predictor_matrix["regime", ] <- 1
  predictor_matrix["regime", "regime"] <- 0

  imps <- gFormulaMI::gFormulaImpute(
    data             = wide_mids,
    M                = M,
    trtVars          = c("econ_dist_bin_0", "econ_dist_bin_1", "econ_dist_bin_2"),
    trtRegimes       = regimes,
    predictorMatrix  = predictor_matrix,
    silent           = TRUE
  )

  fits <- imps %$%
    lm(sf12mcs_dv_2 ~ factor(regime) + 0)

  outvals <- gFormulaMI::syntheticPool(fits)

  regimes_3 <- tibble::tibble(
    intervention = regimes |>
      purrr::map(paste, collapse = "-") |>
      purrr::reduce(c)
  )

  out <- outvals |>
    tibble::as_tibble() |>
    tibble::rownames_to_column("Intervention") |>
    dplyr::transmute(
      mi_effect = Estimate,
      mi_se     = sqrt(Total),
      mi_ll     = `95% CI L`,
      mi_ul     = `95% CI U`
    ) |>
    dplyr::bind_cols(regimes_3)

  # Surface the g-formula imputation's loggedEvents (constant/collinear
  # predictors dropped during the counterfactual imputation) in the build log.
  le <- imps$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_gformula: no logged events.")
  } else {
    message("run_gformula: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  out
}
