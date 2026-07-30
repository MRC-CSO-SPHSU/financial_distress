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

### make_strata: pure df -> df, same id arithmetic as the old strata_creation()
test_that("make_strata builds the 12-cell id and label", {
  source(here::here("R", "strata_creation.R"))

  set.seed(1)
  toy <- data.frame(
    sex_dv_base         = factor(sample(c("Female", "Male"), 240, TRUE)),
    race_base           = factor(sample(c("Non.white", "White"), 240, TRUE)),
    hiqual_dv_fact_base = factor(sample(c("High", "Low", "Medium"), 240, TRUE)),
    other_col           = rnorm(240)
  )

  out <- make_strata(toy)

  expect_equal(nrow(out), 240L)
  expect_true(all(c("strata_id", "strata_label") %in% names(out)))
  expect_s3_class(out$strata_id, "factor")
  expect_s3_class(out$strata_label, "factor")
  expect_equal(nlevels(out$strata_id), 12L)
  # id arithmetic: 100*sex + 10*race + hiqual on the factor level indices
  expect_equal(as.character(out$strata_id),
               as.character(100 * as.numeric(toy$sex_dv_base) +
                             10 * as.numeric(toy$race_base) +
                                  as.numeric(toy$hiqual_dv_fact_base)))
  # row order and other columns untouched
  expect_equal(out$other_col, toy$other_col)
  # label format sex:race:educ
  expect_true(all(grepl("^[^:]+:[^:]+:[^:]+$", as.character(out$strata_label))))
})

test_that("make_strata errors when a stratum column is absent", {
  source(here::here("R", "strata_creation.R"))
  expect_error(make_strata(data.frame(sex_dv_base = factor("Female"))), "missing")
})

### DR score primitives (design 3.6-3.7; 7: AIPW identity, sign, Z alignment)
test_that("dr_pseudo_outcome: mean(psi) recovers the true ATE with known nuisances", {
  source(here::here("R", "meta_learner.R"))

  set.seed(101)
  n   <- 20000
  x   <- rnorm(n)
  g   <- plogis(-0.5 + 0.8 * x)
  A   <- rbinom(n, 1, g)
  mu0 <- 50 + 3 * x
  mu1 <- mu0 - 2                       # true conventional ATE = -2
  Y   <- ifelse(A == 1, mu1, mu0) + rnorm(n, 0, 2)

  psi <- dr_pseudo_outcome(A, Y, mu0, mu1, bound_propensity(g))

  # psi is on the Y(0) - Y(1) scale, so the truth here is +2
  expect_equal(mean(psi), 2, tolerance = 0.1)
})

test_that("dr_pseudo_outcome: sign is Y(0) - Y(1), opposite to mu1 - mu0", {
  source(here::here("R", "meta_learner.R"))

  set.seed(102)
  n   <- 500
  g   <- rep(0.4, n)
  A   <- rbinom(n, 1, g)
  mu0 <- rnorm(n, 50, 5)
  mu1 <- mu0 - 3
  Y   <- ifelse(A == 1, mu1, mu0)      # zero residual: IPW terms vanish exactly

  psi <- dr_pseudo_outcome(A, Y, mu0, mu1, g)

  expect_equal(mean(psi), -(mean(mu1) - mean(mu0)), tolerance = 1e-12)
  expect_true(sign(mean(psi)) == -sign(mean(mu1) - mean(mu0)))
})

test_that("dr_pseudo_outcome refuses unbounded propensities", {
  source(here::here("R", "meta_learner.R"))
  expect_error(
    dr_pseudo_outcome(c(0L, 1L), c(1, 2), c(1, 1), c(1, 1), c(0, 0.5)),
    "bound"
  )
})

test_that("bound_propensity clamps to [0.025, 0.975]", {
  source(here::here("R", "meta_learner.R"))
  expect_equal(bound_propensity(c(0, 0.5, 1)), c(0.025, 0.5, 0.975))
})

test_that("SuperLearner $Z is out-of-fold, input-row-ordered and nameless", {
  source(here::here("R", "meta_learner.R"))

  set.seed(103)
  n  <- 300
  X  <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  Yc <- 2 * X$x1 - X$x2 + rnorm(n)
  fit <- SuperLearner::SuperLearner(Y = Yc, X = X, family = gaussian(),
                                    SL.library = c("SL.mean", "SL.glm"),
                                    cvControl = list(V = 5))

  # $Z carries no dimnames: index by libraryNames position, never by name
  expect_null(dimnames(fit$Z))
  idx <- which(fit$libraryNames == "SL.glm_All")
  expect_length(idx, 1L)

  # manual V-fold refit reproduces Z exactly, in input row order (the design's
  # empirical verification of 3.6, kept as a regression test against
  # SuperLearner version changes)
  manual <- rep(NA_real_, n)
  for (v in seq_along(fit$validRows)) {
    rows <- fit$validRows[[v]]
    gfit <- glm(y ~ ., data = data.frame(y = Yc[-rows], X[-rows, ]),
                family = gaussian())
    manual[rows] <- predict(gfit, newdata = X[rows, ], type = "response")
  }
  expect_equal(max(abs(manual - fit$Z[, idx])), 0, tolerance = 1e-8)

  # the hard-assertion wrapper: right length through, wrong length stops
  expect_length(sl_oof_predictions(fit, n, "test"), n)
  expect_error(sl_oof_predictions(fit, n - 1, "test"), "rows")
})

### toy generators for the DR-learner tests (used by the estimate_cate,
### pool_cate and congenial run_mice tests; defined once, here only)
# Synthetic wide frame shaped like the real one: all 14 columns of
# confounders("MCS"), the exposure and the outcome. Balanced 12-cell design,
# every A x S cell populated (deterministic 40% exposed per cell).
# tau_by_cell: length-12, the treatment effect (conventional Y(1) - Y(0)
# scale) per stratum, indexed in strata_id order (ids sorted ascending).
make_toy_wide_df <- function(n_per_cell = 100, tau_by_cell = rep(-2, 12), seed = 1) {
  set.seed(seed)
  grid <- expand.grid(
    sex_dv_base         = factor(c("Female", "Male")),
    race_base           = factor(c("Non.white", "White")),
    hiqual_dv_fact_base = factor(c("High", "Low", "Medium")),
    KEEP.OUT.ATTRS = FALSE
  )
  df <- grid[rep(seq_len(12), each = n_per_cell), ]
  n  <- nrow(df)

  df$sf12mcs_dv_base        <- rnorm(n, 50, 8)
  df$age_dv_0               <- rnorm(n, 45, 10)
  df$gor_dv_fact_0          <- factor(sample(c("London", "North.East", "Wales"), n, TRUE))
  df$econ_dist_bin_lagged_0 <- factor(rbinom(n, 1, 0.2))
  df$pcs_lagged_0           <- rnorm(n, 50, 8)
  df$dnc_lagged_0           <- factor(sample(c("Zero", "One", "Two.plus"), n, TRUE))
  df$home_owner_lagged_0    <- factor(rbinom(n, 1, 0.6))
  df$econ_benefits_lagged_0 <- factor(rbinom(n, 1, 0.3))
  df$mastat_lagged_0        <- factor(sample(c("Partnered", "Not.partnered"), n, TRUE))
  df$econ_emp_bin_fact_0    <- factor(sample(c("Employed", "Not.employed"), n, TRUE))
  df$log_income_0           <- rnorm(n, 10, 1)

  # exposure: exactly 40% treated inside every cell (all 24 A x S cells filled)
  n_tr <- round(0.4 * n_per_cell)
  a <- unlist(lapply(seq_len(12), function(j) {
    sample(rep(c(1L, 0L), c(n_tr, n_per_cell - n_tr)))
  }))
  df$econ_dist_bin_0 <- factor(a)

  # map each row's stratum to its rank in strata_id (= sorted 100s+10r+h) order
  sid      <- 100 * as.numeric(df$sex_dv_base) +
               10 * as.numeric(df$race_base) +
                    as.numeric(df$hiqual_dv_fact_base)
  sid_rank <- match(sid, sort(unique(sid)))

  df$sf12mcs_dv_0 <- 0.5 * df$sf12mcs_dv_base +
    2 * (df$sex_dv_base == "Female") +
    tau_by_cell[sid_rank] * a +
    rnorm(n, 0, 1)

  rownames(df) <- NULL
  df
}

# Punch a few holes and impute so estimate_cate sees a genuine mids object.
make_toy_wide_mids <- function(df, m = 2, seed = 7) {
  set.seed(seed)
  df_na <- df
  df_na$sf12mcs_dv_0[sample(nrow(df), round(0.05 * nrow(df)))] <- NA
  df_na$log_income_0[sample(nrow(df), round(0.05 * nrow(df)))] <- NA
  mice::mice(df_na, m = m, maxit = 1, seed = seed, printFlag = FALSE)
}

### design 7 "factor handling": estimate_cate runs on a small mids whose W
### contains factors
test_that("estimate_cate runs on a small mids with factor confounders", {
  source(here::here("R", "confounders.R"))
  source(here::here("R", "strata_creation.R"))
  source(here::here("R", "sl_wrappers.R"))
  source(here::here("R", "meta_learner.R"))

  df   <- make_toy_wide_df(n_per_cell = 60, seed = 11)
  mids <- make_toy_wide_mids(df, m = 2, seed = 11)

  res <- estimate_cate(mids, imp_idx = 1, sl_libs = c("SL.mean", "SL.glm"),
                       outcome = "MCS")

  expect_named(res, c("gate", "blp", "ate", "wald"))
  expect_equal(nrow(res$gate), 12L)
  expect_setequal(
    names(res$gate),
    c("imp", "strata_id", "strata_label", "n_j", "n_exposed", "pct_exposed",
      "min_g", "median_g", "share_g_at_bound", "estimate", "se")
  )
  expect_false(anyNA(res$gate$estimate))
  expect_true(all(res$gate$se > 0))
  expect_equal(sum(res$gate$n_j), nrow(df))
  expect_true(all(res$gate$min_g >= 0.025 & res$gate$min_g <= 0.975))

  expect_equal(nrow(res$blp), 2L)
  expect_equal(res$blp$term, c("beta1_ate", "beta2_hte"))

  expect_equal(res$wald$df1, 11)

  # exact identity: mean(psi) is the n_j-weighted mean of the cell GATEs
  expect_equal(res$ate$estimate,
               sum(res$gate$estimate * res$gate$n_j) / sum(res$gate$n_j),
               tolerance = 1e-10)
})