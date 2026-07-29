# _targets.R — pipeline orchestration for the FD analysis from the UKHLS.
# Parallel backend: future + future.batchtools. The controller process running
# tar_make_future() submits each worker target as its own SLURM job via the
# slurm.tmpl template. When deployed in a local machine, it runs the analysis via future.callr.
# which calls separate r sessions in the background to solve future calls.

library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
library(future.callr)

# Detect SLURM at runtime, if not in cluster, run locally with future.callr (in a separate r process)
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))

if (on_slurm) {
  future::plan(
    future.batchtools::batchtools_slurm,
    template  = "slurm.tmpl",
    resources = list(
      ncpus    = 2,
      memory   = 24 * 1024,      # MB per CPU (= 24 GB per worker).
      walltime = 6 * 60 * 60,    # seconds (= 6 h wall)
      account  = "none"
    )
  )
} else {
  future::plan(future.callr::callr, workers = 4)
}

message("--- TARGETS FUTURE PLAN ---")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("on_slurm:        ", on_slurm)
message("plan:            ", paste(class(future::plan()), collapse = "/"))
message("---------------------------")

# ---- Packages attached to every target's evaluation environment ------------
# notes: bit64 is needed for data.table to handle 64-bit integers (pipd)
tar_option_set(
  packages = c(
    "data.table",
    "bit64",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "magrittr",
    "rlang",
    "here",
    "mice",
    "tmle",
    "miceadds",
    "SuperLearner",
    "xgboost",
    "gam",
    "arm",
    "quarto"
  ),
  format = "rds",
  seed   = 42
)

# ---- Source extracted functions (R/) -----------
for (f in c(list.files("R", "\\.R$", full.names = TRUE))) source(f)

# ---- Configuration ---------------------------------------------------------
## mice configs
mice_m      <- 50
mice_maxit  <- 15
seed_random <- 42
## gFormulaMI configs
gform_M <- 50
## Outcome scale for the whole pipeline: "MCS" or "PCS"
outcome_scale <- "MCS"


## SuperLearner library for the TMLE Q- and g-models. SL.xgboost.tmle is the
## custom wrapper in R/sl_wrappers.R (bounded-outcome handling).
sl_libs <- c("SL.mean", "SL.glm", "SL.glmnet.tmle", "SL.gam", "SL.nnet", "SL.xgboost.tmle")

# ---- DAG -------------------------------------------------------------------
list(
  # Data prep: import/clean/preproc, then split into wide + long frames.
  tar_target(pop_data,
    if (on_slurm) {
      import_data(force = FALSE) |> clean_data() |> preproc_data()
    } else {
      import_data(force = TRUE) |> clean_data() |> preproc_data()
    }
  ),
  tar_target(wide_data,
             build_data(pop_data, outcome = outcome_scale)),

  # Wide-format multiple imputation (one mids object, backs the single-point TMLE).
  tar_target(wide_mids,
             run_mice(wide_data,
                      m     = mice_m,
                      maxit = mice_maxit,
                      seed  = seed_random)),

  # Single-point TMLE: one fit per imputation (branch over imp_idx), then
  # Pool all the estimates with Rubin's rules.
  tar_target(tmle_imp_idx, seq_len(mice_m)),
  tar_target(
    tmle_one,
    fit_tmle_one(
      wide_mids = wide_mids,
      imp_idx   = tmle_imp_idx,
      sl_libs   = sl_libs,
      outcome   = outcome_scale
    ),
    pattern   = map(tmle_imp_idx), # one branch per imputation
    iteration = "list"             # collect the per-imputation tmle fits in a list
  ),
  tar_target(tmle_results,
             pool_tmle(tmle_one)),

  # Sensitivity analysis: g-formula via gFormulaMI (multiple imputation of counterfactual outcomes).
  # Only estimates marginal means
  tar_target(mi_results,
             run_gformula(
               wide_mids     = wide_mids,
               wide_data_mi  = wide_data,
               M             = gform_M,
               estimand      = "factor(regime) + 0"
             )),

  # Estimating ATE via g-formula MI (multiple imputation of counterfactual outcomes).
  tar_target(mi_ate_results,
             run_gformula(
               wide_mids     = wide_mids,
               wide_data_mi  = wide_data,
               M             = gform_M,
               estimand      = "factor(regime)"
              )),
  # Comparison of TMLE and g-formula MI estimates.
  tar_target(comparison,
             assemble_comparison(tmle_results, 
                                 mi_results,
                                 mi_ate_results
            )),

# ---------- MAIHDA: multilevel analysis of individual heterogeneity and discriminatory accuracy ---------------
  # Step 1: Creating strata ids after imputation
  tar_target(strata_ids,
             strata_creation(wide_mids)),
  # Step 2: MAIHDA via hierarchical TMLE — stratum-level PATE, one fit per imputation.
  tar_target(
    maihda_one,
    run_tmle_cluster(
      strata_data = strata_ids$fits,
      imp_idx     = tmle_imp_idx,
      sl_libs.Q   = sl_libs,
      sl_libs.g   = sl_libs,
      outcome     = outcome_scale
    ),
    pattern   = map(tmle_imp_idx),
    iteration = "list"
  ),
  tar_target(maihda_results,
             pool_tmle_cluster(maihda_one)),
  # Report
  tarchetypes::tar_quarto(report,
                          "07_single_treatment.qmd")
)