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

  # dianosing mice imputation
  source(here::here("tests", "diagnose_mice.R"))
  diagnose_mice(mids, wide_data,
                plot_dir = here::here("tests", "figs", "mice_diag"))

  # mice ran without logged events (warnings or errors during imputation)
  expect_null(mids$loggedEvents)
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

# to verify the predictor matrix, a external visualisation aid is necessary as there are many row/column pairs
predictor_matrix |> as.data.frame() |> 
  openxlsx::write.xlsx(here::here("tests", "matrix", "predictor_matrix.xlsx"), overwrite = TRUE, rowNames = TRUE, colNames = TRUE)

# SF12 at wave 1 is predicted by all time-invariant confounders
predictor_matrix["sf12mcs_dv_base", c("sex_dv_base",
                                      "hiqual_dv_base",
                                      "race_base",
                                      "gor_dv_fact_base",
                                      "age_dv_base")] <- 1
# SF12 at wave 1 predicts all time-variant confounders at the earliest subsequent wave
predictor_matrix[c("pcs_lagged_0",
                   "econ_dist_bin_0",
                   "dnc_lagged_0",
                   "home_owner_lagged_0",
                   "econ_benefits_lagged_0",
                   "mastat_lagged_0"), "sf12mcs_dv_base"] <- 1
  
## also predict intermediate confounders and SF12 at wave 0
predictor_matrix[c("log_income_0",
                   "econ_emp_bin_fact_0",
                   "sf12mcs_dv_0"), "sf12mcs_dv_base"] <- 1

# but does not predict any other variable at wave 1 or 2
predictor_matrix[c("pcs_lagged_1", "pcs_lagged_2",
                   "econ_dist_bin_1", "econ_dist_bin_2",
                   "dnc_lagged_1", "dnc_lagged_2",
                   "home_owner_lagged_1", "home_owner_lagged_2",
                   "econ_benefits_lagged_1", "econ_benefits_lagged_2",
                   "mastat_lagged_1", "mastat_lagged_2",
                   "log_income_1", "log_income_2",
                   "econ_emp_bin_fact_1", "econ_emp_bin_fact_2",
                   "sf12mcs_dv_1", "sf12mcs_dv_2"), "sf12mcs_dv_base"] <- 0
  
# time varying confounders does not predict among themselves in the same wave
predictor_matrix[c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0"), 
                 c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0")] <- 0
predictor_matrix[c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1"),
                 c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1")] <- 0
predictor_matrix[c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2"),
                 c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2")] <- 0
  
# but all predict distress at time t
predictor_matrix[c("econ_dist_bin_0"), 
  c("pcs_lagged_0", "dnc_lagged_0", "home_owner_lagged_0", "econ_benefits_lagged_0", "mastat_lagged_0")] <- 1
predictor_matrix[c("econ_dist_bin_1"), 
  c("pcs_lagged_1", "dnc_lagged_1", "home_owner_lagged_1", "econ_benefits_lagged_1", "mastat_lagged_1")] <- 1
predictor_matrix[c("econ_dist_bin_2"), 
  c("pcs_lagged_2", "dnc_lagged_2", "home_owner_lagged_2", "econ_benefits_lagged_2", "mastat_lagged_2")] <- 1

## time-varying confounders at time t predicts themselves at t+1
predictor_matrix["pcs_lagged_1", "pcs_lagged_0"] <- 1
predictor_matrix["pcs_lagged_2", "pcs_lagged_1"] <- 1
predictor_matrix["econ_dist_bin_1", "econ_dist_bin_0"] <- 1
predictor_matrix["econ_dist_bin_2", "econ_dist_bin_1"] <- 1
predictor_matrix["dnc_lagged_1", "dnc_lagged_0"] <- 1
predictor_matrix["dnc_lagged_2", "dnc_lagged_1"] <- 1
predictor_matrix["home_owner_lagged_1", "home_owner_lagged_0"] <- 1
predictor_matrix["home_owner_lagged_2", "home_owner_lagged_1"] <- 1
predictor_matrix["econ_benefits_lagged_1", "econ_benefits_lagged_0"] <- 1
predictor_matrix["econ_benefits_lagged_2", "econ_benefits_lagged_1"] <- 1
predictor_matrix["mastat_lagged_1", "mastat_lagged_0"] <- 1
predictor_matrix["mastat_lagged_2", "mastat_lagged_1"] <- 1
predictor_matrix["log_income_1", "log_income_0"] <- 1
predictor_matrix["log_income_2", "log_income_1"] <- 1
predictor_matrix["econ_emp_bin_fact_1", "econ_emp_bin_fact_0"] <- 1
predictor_matrix["econ_emp_bin_fact_2", "econ_emp_bin_fact_1"] <- 1

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

## sf_12mcs_dv_t predicts time varying confounders at t+1
predictor_matrix[c("pcs_lagged_1", 
                   "econ_dist_bin_1", 
                   "dnc_lagged_1", 
                   "home_owner_lagged_1", 
                   "econ_benefits_lagged_1", 
                   "mastat_lagged_1"), "sf12mcs_dv_0"] <- 1
predictor_matrix[c("pcs_lagged_2", 
                   "econ_dist_bin_2", 
                   "dnc_lagged_2", 
                   "home_owner_lagged_2", 
                   "econ_benefits_lagged_2", 
                   "mastat_lagged_2"), "sf12mcs_dv_1"] <- 1

# intermediate confounders at time t predict distress at time t
predictor_matrix[c("econ_dist_bin_0"), 
  c("log_income_0", "econ_emp_bin_fact_0")] <- 1
predictor_matrix[c("econ_dist_bin_1"), 
  c("log_income_1", "econ_emp_bin_fact_1")] <- 1
predictor_matrix[c("econ_dist_bin_2"), 
  c("log_income_2", "econ_emp_bin_fact_2")] <- 1

predictor_matrix["regime", ] <- 1
predictor_matrix["regime", "regime"] <- 0
  
## post visual verification
predictor_matrix |> as.data.frame() |> 
  openxlsx::write.xlsx(here::here("tests", "matrix", "predictor_matrix.xlsx"), overwrite = TRUE, rowNames = TRUE, colNames = TRUE)

# returns a proper predictor matrix with the expected dimensions
expect_true(is.matrix(pred_mat))
expect_equal(dim(pred_mat), c(ncol(wide_data), ncol(wide_data)))
})