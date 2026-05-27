#######################################################################################################
# PROJECT: Financial distress and health outcomes. A LTMLE analysis of the UKHLS
# DESCRIPTION: Helpers for data manipulation and formatting
#######################################################################################################
# COUNTRY: UK
# DATA: UKHLS EUL version - UKDA-6614-stata [to wave 0] and WAS EUL version - UKDA-7215-stata [to wave 7]
# AUTHORS:	Darwin del Castillo
# LAST UPDATE: 11 May 2026
#######################################################################################################

# @pending: add mice native predictor matrix function to custom predictor matrix to improve counterfactual imputation pipeline.

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
      {{id_col}},
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
  
  df_out
}


make_predictor_matrix <- function(return_vals) {
  n_tvars <- attr(return_vals, "n_tvars")
  n_base <- attr(return_vals, "n_base")
  n_vars <- length(return_vals) + 1
  
  p_mat <- matrix(0, ncol = n_vars, nrow = n_vars)
  
  p_mat[(n_base + 1):n_vars, 1:(n_base)] <- 1
  
  p_mat
  
  
  for (n in 1:(n_vars - n_base)) {
    p_mat[n_base + n, max(0, n_base + n - n_tvars):(n_base + n - 1)] <-
      1
  }
  
  p_mat[n_vars, 1:(n_vars - 1)] <- 1
  
  rownames(p_mat) <- colnames(p_mat) <- c(colnames(return_vals), "regime")
  
  p_mat
  
}