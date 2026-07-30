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
  lattice,
  SuperLearner,
  estimatr,
  car,
  lmtest,
  sandwich)

## testing functions
### does build_data not crash and produce the expected single-wave wide columns
test_that("build_data produces expected output", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data <- import_data(force = FALSE) |> clean_data() |> preproc_data()

  wide_data <- build_data(pop_data, outcome = "MCS")
  expect_true(is.data.frame(wide_data))

  # single target wave: everything is _base or _0 (codebase-map 1)
  expected_cols <- c(
    "sex_dv_base", "hiqual_dv_fact_base", "race_base", "sf12mcs_dv_base",
    "sf12mcs_dv_0", "age_dv_0", "gor_dv_fact_0",
    "pcs_lagged_0", "econ_dist_bin_0", "econ_dist_bin_lagged_0",
    "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0",
    "mastat_lagged_0", "econ_emp_bin_fact_0", "log_income_0"
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

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
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
  source(here::here("R", "confounders.R"))
  source(here::here("R", "fit_tmle_one.R"))

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data, outcome = "MCS")

  mids <- run_mice(wide_data, m = 2, maxit = 2, seed = 42)

  tmle_fit <- fit_tmle_one(mids,
                           imp_idx = 1,
                           sl_libs = c("SL.mean", "SL.glm"),
                           outcome = "MCS")

  # fit_tmle_one calls tmle::tmle(), which returns a "tmle" object
  expect_s3_class(tmle_fit, "tmle")
  expect_true(is.numeric(tmle_fit$estimates$ATE$psi))
})

## can i add an additional variable

test_that("will a new variable exist after imputation", {
  source(here::here("R", "build_data.R"))
  source(here::here("R", "run_mice.R"))
  source(here::here("R", "import_cleaning.R"))
  source(here::here("R", "helpers.R"))

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
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

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
  wide_data <- build_data(pop_data, pre_wave = 2, target_wave = 3, outcome = "MCS")

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

## ---- DR-learner GATE arm (design 2026-07-30) --------------------------------

### shared confounder set: byte-identical between fit_tmle_one and estimate_cate
test_that("confounders returns the exact fit_tmle_one adjustment set", {
  source(here::here("R", "confounders.R"))

  expect_identical(
    confounders("MCS"),
    c("sex_dv_base",
      "hiqual_dv_fact_base",
      "race_base",
      "sf12mcs_dv_base",
      "age_dv_0",
      "gor_dv_fact_0",
      "econ_dist_bin_lagged_0",
      "pcs_lagged_0",
      "dnc_lagged_0",
      "home_owner_lagged_0",
      "econ_benefits_lagged_0",
      "mastat_lagged_0",
      "econ_emp_bin_fact_0",
      "log_income_0")
  )
  # PCS swaps exactly the two outcome-dependent entries
  expect_identical(setdiff(confounders("PCS"), confounders("MCS")),
                   c("sf12pcs_dv_base", "mcs_lagged_0"))
  expect_identical(setdiff(confounders("MCS"), confounders("PCS")),
                   c("sf12mcs_dv_base", "pcs_lagged_0"))
  expect_error(confounders("XXX"))
})