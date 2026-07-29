#######################################################################################################
# PROJECT: Financial distress and health outcomes. A LTMLE analysis of the UKHLS
# DESCRIPTION: Helpers for data manipulation and formatting
#######################################################################################################
# COUNTRY: UK
# DATA: UKHLS EUL version - UKDA-6614-stata [to wave 0] and WAS EUL version - UKDA-7215-stata [to wave 7]
# AUTHORS:	Darwin del Castillo
# LAST UPDATE: 11 May 2026
#######################################################################################################

# reshaping dataset from long to wide format, by adding attributes to each variable group
make_wide <- function(df, id_col, time_col, base_cols, outcome, ..., static = FALSE, waves = NULL) {
  
  require(rlang)
  require(dplyr)
  require(tidyr)

  outcome_name <- as_name(ensym(outcome))
  
  t_conf <- enquos(...)
  
  if (!is.null(waves)) {
    df <- df |> dplyr::filter({{time_col}} %in% waves)
  }

  t_max <- max(df |> dplyr::pull({{time_col}}))
  t_min <- min(df |> dplyr::pull({{time_col}}))

  df_out <- df |>
    dplyr::select({{id_col}}, {{time_col}}, {{base_cols}}, !!!t_conf, {{outcome}}) |>
    tidyr::pivot_wider(
      id_cols = c({{id_col}}, {{base_cols}}),
      names_from = {{time_col}},
      values_from = -c({{id_col}}, {{time_col}}, {{base_cols}})) |>
    dplyr::select(
#      {{id_col}},
      {{base_cols}},
      ends_with("0"),
      ends_with("1"),
      ends_with("2"),
      ends_with("3"),
      ends_with("4"),
      ends_with("5"),
      ends_with("6"),
      ends_with("7"),
      ends_with("8"),
      ends_with("9")
    )

  if (static) {
      intermediate_cols <- paste0(outcome_name, "_", (t_min + 1):(t_max - 1))
      df_out <- df_out |> dplyr::select(-any_of(intermediate_cols))
    }

  attr(df_out, "n_tvars") <- length(t_conf)
  attr(df_out, "n_base") <- length(df[1,] |> dplyr::select({{base_cols}}))
  attr(df_out, "baseline_vars") <- names(dplyr::select(df_out, {{base_cols}}))
  attr(df_out, "time_lagged")   <- names(dplyr::select(df_out, contains("lagged")))
  attr(df_out, "outcome_var")   <- paste0(outcome_name, "_", t_max)
  attr(df_out, "outcome_baseline") <- paste0(outcome_name, "_base")
  attr(df_out, "time_points") <- length(unique(df |> dplyr::pull({{time_col}})))
  attr(df_out, "intermediate_vars") <- names(dplyr::select(df_out, !contains("lagged"), -c({{base_cols}})))

  
  return(df_out)
}

# tag the exposure column(s) so downstream code (predictor matrix) knows which nodes are treatment.
set_exposure <- function(df, exposure) {
  require(rlang)
  exposure <- as_name(ensym(exposure))
  exposure_vars <- grep(paste0("^", exposure, "_\\d+$"), names(df), value = TRUE)
  if (length(exposure_vars) == 0L) stop("no exposure columns matched '", exposure, "_<wave>'")
  attr(df, "exposure_vars") <- exposure_vars
  return(df)
}

# function to replicate DAG arrows (counterfactual imputation for gFormulaMI)
make_counterfactual_matrix <- function(return_vals) {
  n_tvars <- attr(return_vals, "n_tvars")
  n_base <- attr(return_vals, "n_base")
  baseline_vars <- attr(return_vals, "baseline_vars")
  time_lagged <- attr(return_vals, "time_lagged")
  outcome_var <- attr(return_vals, "outcome_var")
  outcome_baseline <- attr(return_vals, "outcome_baseline")
  time_points <- attr(return_vals, "time_points")
  intermediate_vars <- attr(return_vals, "intermediate_vars")
  exposure <- attr(return_vals, "exposure_vars")

  n_vars <- length(return_vals) + 1
  
  p_mat <- matrix(0, ncol = n_vars, nrow = n_vars)
  
  nodes <- c(colnames(return_vals), "regime")
  dimnames(p_mat) <- list(nodes, nodes)

  # time-invariant confounders are lower-triangular among themselves: each is
  # predicted by every baseline variable preceding it in the visit (column) order.
  # Without this they carry all-zero predictor rows and gFormulaMI draws each one
  # from its own marginal in the synthetic block, destroying their joint
  # distribution. The chain rule over the column order reproduces that joint.
  base_ordered <- intersect(nodes, baseline_vars)
  for (i in seq_along(base_ordered)[-1]) {
    p_mat[base_ordered[i], base_ordered[seq_len(i - 1L)]] <- 1
  }

  # baseline outcome is predicted by all time-invariant confounders
  p_mat[outcome_baseline, setdiff(c(baseline_vars, outcome_baseline), outcome_baseline)] <- 1

  # exposure is predicted by all time-invariant confounders and baseline outcome
  # if it's time-varying, only the most recent exposure is predicted by baseline confounders and baseline outcome
  if (time_points <= 1){
    p_mat[exposure, c(baseline_vars, outcome_baseline)] <- 1
  } else {
    p_mat[exposure, c(baseline_vars)] <- 1
  }
  
  # time-lagged confounders are predicted by baseline outcome and all time-invariant confounders
  p_mat[time_lagged, c(baseline_vars)] <- 1
  
  # time-lagged confounders predict exposure (single-time) or lagged_t predicts exposure_t (time-varying)
  if (time_points <= 1) {
    p_mat[exposure, time_lagged] <- 1
  } else {
    for (t in seq_len(time_points - 1)) {
      exp_t <- exposure[endsWith(exposure, paste0("_", t))]
      lag_t <- time_lagged[endsWith(time_lagged, paste0("_lagged_", t))]
      p_mat[exp_t, lag_t] <- 1
    }
  }

  # time-varying covariates that are neither the exposure, the outcome, nor a lag
  # (age_dv_0, gor_dv_fact_0, econ_emp_bin_fact_0, log_income_0). With a single
  # timepoint the loops below never fire, so wire them up explicitly — otherwise
  # they get all-zero predictor rows and drop out of both the exposure and the
  # outcome model, which the point-treatment TMLE (fit_tmle_one) adjusts for.
  covar_vars <- setdiff(intermediate_vars, c(exposure, outcome_var, outcome_baseline))

  if (time_points <= 1 && length(covar_vars) > 0) {
    # gFormulaMI runs mice with maxit = 1 and the default (column-order) visit
    # sequence, so a predictor is only useful if it precedes its target.
    for (v in covar_vars) {
      earlier <- nodes[seq_len(match(v, nodes) - 1L)]
      p_mat[v, intersect(c(baseline_vars, outcome_baseline, time_lagged), earlier)] <- 1
    }
    # they confound the exposure–outcome relationship
    p_mat[exposure, covar_vars]    <- 1
    p_mat[outcome_var, covar_vars] <- 1
  }

  # outcome is predicted by all time-invariant confounders, all time-lagged confounders, baseline outcome,
  # and exposure (single-time) or exposure_t (time-varying)
  p_mat[outcome_var, c(baseline_vars, time_lagged, outcome_baseline)] <- 1

  if (time_points <= 1) {
    p_mat[outcome_var, exposure] <- 1
  } else {
    for (t in seq_len(time_points - 1)) {
      exp_t <- exposure[endsWith(exposure, paste0("_", t))]
      p_mat[outcome_var, exp_t] <- 1
    }
  }

  # time-varying confounders are predicted by their immediate prior period
  # same for outcomes, predictors, lagged confounders
  for (t in seq_len(time_points - 1)) {
    prev_c <- intermediate_vars[endsWith(intermediate_vars, paste0("_", t))]
    curr_c <- intermediate_vars[endsWith(intermediate_vars, paste0("_", t + 1))]
    p_mat[curr_c, prev_c] <- 1
    
    prev_o <- paste0(outcome_var, "_", t)
    curr_o <- paste0(outcome_var, "_", t + 1)
    p_mat[curr_o, prev_o] <- 1

    prev_lag <- time_lagged[endsWith(time_lagged, paste0("_lagged_", t))]
    curr_lag <- time_lagged[endsWith(time_lagged, paste0("_lagged_", t + 1))]
    p_mat[curr_lag, prev_lag] <- 1
  }
  # regime node is predicted by all other variables (i.e., all confounders and the outcome)
  p_mat["regime", 1:(n_vars - 1)] <- 1
  
  return(p_mat)
}