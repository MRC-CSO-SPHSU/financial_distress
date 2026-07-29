# testing files of the pipeline and specific functions
here::i_am("tests/test_code.R")

## reproducing targets environment
pacman::p_load(testthat,
  data.table,
  dplyr,
  tidyr,
  tibble,
  purrr,
  rlang,
  here,
  mice,
  lattice)

## testing functions
### does build_wide_data not crash and produce the expected wide columns
test_that("build_wide_data produces expected output", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data <- import_data(force = TRUE) |> clean_data() |> preproc_data()

  wide_data <- build_data(pop_data)
  expect_true(is.data.frame(wide_data))

  expected_cols <- c(
    "sex_dv_base", "hiqual_dv_base", "race_base", "gor_dv_fact_base",
    "sf12mcs_dv_0", "sf12mcs_dv_1", "sf12mcs_dv_2",
    "pcs_lagged_0", "pcs_lagged_1", "pcs_lagged_2",
    "econ_dist_bin_0", "econ_dist_bin_1", "econ_dist_bin_2",
    "dnc_lagged_0", "dnc_lagged_1", "dnc_lagged_2",
    "home_owner_lagged_0", "home_owner_lagged_1", "home_owner_lagged_2",
    "econ_benefits_lagged_0", "econ_benefits_lagged_1", "econ_benefits_lagged_2",
    "mastat_lagged_0", "mastat_lagged_1", "mastat_lagged_2",
    "econ_emp_bin_fact_0", "econ_emp_bin_fact_1", "econ_emp_bin_fact_2",
    "log_income_0", "log_income_1", "log_income_2"
  )
  expect_true(all(expected_cols %in% names(wide_data)))
})

### does run_mice not crash and generate mids object with expected variables
#### also if new make_counterfactual_matrix works fine
test_that("run_mice produces expected output", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "run_mice.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data  <- import_data(force = TRUE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data)

  # setting exposure
  wide_data <- set_exposure(wide_data, exposure = "econ_dist_bin")

  # counterfactual predictor matrix
  predictor_matrix <- make_counterfactual_matrix(wide_data)
  
  # mini-mice call to check structure only
  mids <- run_mice(wide_data, m = 2, maxit = 2, seed = 42)

  # returns a proper mids object with the requested number of imputations
  expect_s3_class(mids, "mids")
  expect_equal(mids$m, 2L)

  # dianosing mice imputation, only check these results with at least 15 iterations
  source(here::here("tests", "diagnose_mice.R"))
  diagnose_mice(mids, wide_data,
                plot_dir = here::here("tests", "figs", "wide", "mice_diag"))

  # mice ran without logged events (warnings or errors during imputation)
  expect_null(mids$loggedEvents)

  # does counterfactual matrix have regime row and column
  expect_true("regime" %in% rownames(predictor_matrix))
  
  # are all regime row values 1 except for the regime column
  off_diag <- predictor_matrix["regime", colnames(predictor_matrix) != "regime"]
  expect_true(all(off_diag == 1))
  expect_equal(unname(predictor_matrix["regime", "regime"]), 0)
})

### does fit_tmle_one not crash
test_that("fit_tmle_one runs without error", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "run_mice.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))
  source(here::here("R", "sl_wrappers.R"))
  source(here::here("R", "fit_tmle_one.R"))

  pop_data  <- import_data(force = TRUE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data)

  # mini-mice call to check structure only
  mids <- run_mice(wide_data, m = 2, maxit = 2, seed = 42)

  # fit TMLE on the first imputation
  tmle_fit <- fit_tmle_one(mids, 
                           imp_idx = 1, 
                          sl_libs = c("SL.xgboost.ltmle", 
                                      "SL.glm", 
                                      "SL.mean", 
                                      "SL.gam", 
                                      "SL.nnet",
                                      "SL.glmnet.ltmle"))

  # fit_tmle_one calls ltmle::ltmle(), which returns an "ltmle" object
  expect_s3_class(tmle_fit, "ltmleEffectMeasures")
})

### does make_predictor_matrix for g-formula MI produced a predictor matrix consistent with the DAG assumptions
test_that("make_predictor_matrix produces expected output", {
  source(here::here("R/build_wide_data.R"))
  source(here::here("R/run_mice.R"))
  source(here::here("R/run_gformula.R"))
  source(here::here("fnct", "import_cleaning.R"))
  source(here::here("fnct", "helpers.R"))

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
  wide_data <- build_wide_data(pop_data)

  pred_mat <- make_predictor_matrix(wide_data)
  
## post visual verification
predictor_matrix |> as.data.frame() |> 
  openxlsx::write.xlsx(here::here("tests", "matrix", "predictor_matrix.xlsx"), overwrite = TRUE, rowNames = TRUE, colNames = TRUE)

# returns a proper predictor matrix with the expected dimensions
expect_true(is.matrix(pred_mat))
expect_equal(dim(pred_mat), c(ncol(wide_data), ncol(wide_data)))
})

## can i add an additional variable

test_that("will a new variable exist after imputation", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "run_mice.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data  <- import_data(force = TRUE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data)

  # setting exposure
  wide_data <- set_exposure(wide_data, exposure = "econ_dist_bin")

  # counterfactual predictor matrix
  predictor_matrix <- make_counterfactual_matrix(wide_data)
  
  # mini-mice call to check structure only
  mids <- run_mice(wide_data, m = 2, maxit = 2, seed = 42)

  # returns a proper mids object with the requested number of imputations
  expect_s3_class(mids, "mids")
  expect_equal(mids$m, 2L)

  # adding new var
  as_long_mids <- mice::complete(mids, action = "long", include = TRUE)
  
  as_long_mids <- as_long_mids |> mutate(new_var = paste0("var_", as.character(sex_dv_base)))

  imp_new <- mice::as.mids(as_long_mids)
  
  # mice ran without logged events (warnings or errors during imputation)
  expect_null(mids$loggedEvents)

  # does counterfactual matrix have regime row and column
  expect_true("regime" %in% rownames(predictor_matrix))
  
  # are all regime row values 1 except for the regime column
  off_diag <- predictor_matrix["regime", colnames(predictor_matrix) != "regime"]
  expect_true(all(off_diag == 1))
  expect_equal(unname(predictor_matrix["regime", "regime"]), 0)
})


## verifying how many strata will be for MAIHDA
test_that("how many strata will be for MAIHDA", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "run_mice.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data  <- import_data(force = TRUE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data, pre_wave = 2, target_wave = 3, outcome = "MCS", clust.id = TRUE)

  # mini-mice call to check structure only
  mids <- run_mice(wide_data, m = 5, maxit = 2, seed = 42)

  # counterfactual predictor matrix
  predictor_matrix <- make_counterfactual_matrix(wide_data)

  # returns a proper mids object with the requested number of imputations
  expect_s3_class(mids, "mids")

  # mice ran without logged events (warnings or errors during imputation)
  expect_null(mids$loggedEvents)

  # how many strata will be for MAIHDA in observed outcome data
  ## creating strata_id per each imputed dataset
  fits <- purrr::map(seq_len(mids$m), function(i) {
    dat_i <- mice::complete(mids, action = i) |>
      dplyr::mutate(
        # intersectional strata id: unique cell
        strata_id = 100 * as.numeric(sex_dv_base) +
                     10 * as.numeric(race_base) +
                          as.numeric(hiqual_dv_fact_base),
        strata_id = as.factor(strata_id),
        # human-readable stratum name, e.g. "female:white:high"
        strata_label = interaction(sex_dv_base, race_base, hiqual_dv_fact_base,
                                   sep = ":", drop = TRUE)
      ) }
    )
  # counts per stratum (id + label) for each imputed dataset
  strata_tables <- purrr::map(fits, function(dat_i) {
    dplyr::count(dat_i, strata_id, strata_label)
  })

})