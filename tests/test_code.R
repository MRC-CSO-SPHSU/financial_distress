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
  source(here::here("R/build_wide_data.R"))
  source(here::here("fnct", "import_cleaning.R"))
  source(here::here("fnct", "helpers.R"))

  pop_data <- import_data(force = TRUE) |> clean_data() |> preproc_data()

  wide_data <- build_wide_data(pop_data)
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

### does run_mice not crash and produce mids object with expected variables
test_that("run_mice produces expected output", {
  source(here::here("R/build_wide_data.R"))
  source(here::here("R/run_mice.R"))
  source(here::here("fnct", "import_cleaning.R"))
  source(here::here("fnct", "helpers.R"))

  pop_data  <- import_data(force = FALSE) |> clean_data() |> preproc_data()
  wide_data <- build_wide_data(pop_data)

  # mini-mice call to check structure only
  mids <- run_mice(wide_data, m = 2, maxit = 2, seed = 20260522)

  # returns a proper mids object with the requested number of imputations
  expect_s3_class(mids, "mids")
  expect_equal(mids$m, 2L)

  # diagnosis must print BEFORE the assertion below: in testthat a failed
  # expectation aborts the rest of the test_that block
  source(here::here("tests", "diagnose_mice.R"))
  diagnose_mice(mids, wide_data,
                plot_dir = here::here("tests", "figs", "mice_diag"))

  # mice ran without logged events (warnings or errors during imputation)
  expect_null(mids$loggedEvents)
})
