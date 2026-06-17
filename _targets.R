# _targets.R — pipeline orchestration for the financial_distress UKHLS analysis.
# Parallel backend: future + future.batchtools. The controller process running
# tar_make_future() submits each worker target as its own SLURM job via the
# slurm.tmpl template; locally it' single-threaded via future.callr. 
# Not advised to run locally as ltmle step takes a long time (~9h total)

pacman::p_load(targets,
               tarchetypes,
               future,
               future.batchtools,
               future.callr)

# Detect SLURM at runtime or local machine. If in SLURM, the code will submit parallel jobs via slurm.tmpl
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))

if (on_slurm) {
  future::plan(
    future.batchtools::batchtools_slurm,
    template  = "slurm.tmpl",
    resources = list(
      ncpus    = 2,
      memory   = 24 * 1024,      # MB per CPU  (= 24 GB per worker).
      walltime = 6 * 60 * 60,    # seconds     (= 6 h wall)
      account  = "none"
    )
  )
} else {
  future::plan(future.callr::callr, workers = 4)
}

message("=== TARGETS FUTURE PLAN ===")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("on_slurm:        ", on_slurm)
message("plan:            ", paste(class(future::plan()), collapse = "/"))
message("===========================")

# ---- Packages attached to every target's evaluation environment ------------
# notes: bit64 is needed for data.table to handle 64-bit integers (pipd)
tar_option_set(
  packages = c(
    "data.table", "bit64", "dplyr", "tidyr", "tibble", "purrr", "magrittr",
    "rlang", "here",
    "mice", "ltmle", "SuperLearner", "xgboost", "gam", "arm",
    "gFormulaMI",
    "quarto"
  ),
  format = "rds",
  seed   = 20260522
)

# ---- Source extracted functions (R/) and project helpers (fnct/) -----------
for (f in c(list.files("R",    "\\.R$", full.names = TRUE),
            list.files("fnct", "\\.R$", full.names = TRUE))) source(f)

# ---- Configuration ---------------------------------------------------------
## mice and gformulaMI configs
mice_m      <- 35  
mice_maxit  <- 15
gformula_M  <- 50
seed_random <- 20260522

## ltmle configs
sl_libs <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.gam", "SL.nnet", "SL.svm", "SL.xgboost.ltmle")

regimes <- list(
  "0-0-0" = c(0, 0, 0),
  "0-0-1" = c(0, 0, 1),
  "0-1-0" = c(0, 1, 0),
  "0-1-1" = c(0, 1, 1),
  "1-0-0" = c(1, 0, 0),
  "1-0-1" = c(1, 0, 1),
  "1-1-0" = c(1, 1, 0),
  "1-1-1" = c(1, 1, 1)
)

# Qform — one entry per L/Y node. ltmle collapses consecutive L/Y nodes into
# blocks locked at the Y nodes.
Qform <- c(
    sf12mcs_dv_0 = "Q.kplus1 ~ sex_dv_base + 
                               hiqual_dv_base + 
                               race_base + 
                               gor_dv_fact_base + 
                               age_dv_base + 
                               sf12mcs_dv_base +
                               pcs_lagged_0 + 
                               econ_benefits_lagged_0 + 
                               home_owner_lagged_0 + 
                               mastat_lagged_0 + 
                               dnc_lagged_0 + 
                               log_income_0 + 
                               econ_emp_bin_fact_0 +
                               econ_dist_bin_0",
    sf12mcs_dv_1 = "Q.kplus1 ~ sex_dv_base + 
                               hiqual_dv_base + 
                               race_base + 
                               gor_dv_fact_base + 
                               age_dv_base + 
                               sf12mcs_dv_base +
                               pcs_lagged_0 + 
                               econ_benefits_lagged_0 + 
                               home_owner_lagged_0 + 
                               mastat_lagged_0 + 
                               dnc_lagged_0 + 
                               log_income_0 + 
                               econ_emp_bin_fact_0 +
                               econ_dist_bin_0 + 
                               sf12mcs_dv_0 +
                               pcs_lagged_1 + 
                               econ_benefits_lagged_1 + 
                               home_owner_lagged_1 + 
                               mastat_lagged_1 + 
                               dnc_lagged_1 + 
                               log_income_1 + 
                               econ_emp_bin_fact_1 +
                               econ_dist_bin_1",
    sf12mcs_dv_2 = "Q.kplus1 ~ sex_dv_base + 
                               hiqual_dv_base + 
                               race_base + 
                               gor_dv_fact_base + 
                               age_dv_base + 
                               sf12mcs_dv_base +
                               pcs_lagged_0 + 
                               econ_benefits_lagged_0 + 
                               home_owner_lagged_0 + 
                               mastat_lagged_0 + 
                               dnc_lagged_0 + 
                               log_income_0 + 
                               econ_emp_bin_fact_0 +
                               econ_dist_bin_0 + 
                               sf12mcs_dv_0 +
                               pcs_lagged_1 + 
                               econ_benefits_lagged_1 + 
                               home_owner_lagged_1 + 
                               mastat_lagged_1 + 
                               dnc_lagged_1 + 
                               log_income_1 + 
                               econ_emp_bin_fact_1 +
                               econ_dist_bin_1 + 
                               sf12mcs_dv_1 +
                               pcs_lagged_2 + 
                               econ_benefits_lagged_2 + 
                               home_owner_lagged_2 + 
                               mastat_lagged_2 + 
                               dnc_lagged_2 + 
                               log_income_2 + 
                               econ_emp_bin_fact_2 +
                               econ_dist_bin_2"
)

# gform — one entry per A node.
gform <- c(
    econ_dist_bin_0 = "econ_dist_bin_0 ~ sex_dv_base + 
                                         hiqual_dv_base + 
                                         race_base + 
                                         gor_dv_fact_base + 
                                         age_dv_base + 
                                         sf12mcs_dv_base +
                                         pcs_lagged_0 + 
                                         econ_benefits_lagged_0 + 
                                         home_owner_lagged_0 + 
                                         mastat_lagged_0 + 
                                         dnc_lagged_0 + 
                                         log_income_0 + 
                                         econ_emp_bin_fact_0",
    econ_dist_bin_1 = "econ_dist_bin_1 ~ sex_dv_base + 
                                         hiqual_dv_base + 
                                         race_base + 
                                         gor_dv_fact_base + 
                                         age_dv_base + 
                                         sf12mcs_dv_base +
                                         pcs_lagged_1 + 
                                         econ_benefits_lagged_1 + 
                                         home_owner_lagged_1 + 
                                         mastat_lagged_1 + 
                                         dnc_lagged_1 + 
                                         log_income_1 + 
                                         econ_emp_bin_fact_1 +
                                         econ_dist_bin_0 + 
                                         sf12mcs_dv_0",
    econ_dist_bin_2 = "econ_dist_bin_2 ~ sex_dv_base + 
                                         hiqual_dv_base + 
                                         race_base + 
                                         gor_dv_fact_base + 
                                         age_dv_base + 
                                         sf12mcs_dv_base +
                                         pcs_lagged_2 + 
                                         econ_benefits_lagged_2 + 
                                         home_owner_lagged_2 + 
                                         mastat_lagged_2 + 
                                         dnc_lagged_2 + 
                                         log_income_2 + 
                                         econ_emp_bin_fact_2 +
                                         econ_dist_bin_1 + 
                                         sf12mcs_dv_1 + 
                                         econ_dist_bin_0 + 
                                         sf12mcs_dv_0"
)

# grid for dynamic branching of the LTMLE step.
work_grid <- tidyr::expand_grid(
  regime_label = names(regimes),
  imp_idx      = seq_len(mice_m)
)

# ---- DAG -------------------------------------------------------------------
list(
  # Data prep
  tar_target(pop_data,        import_data(force = FALSE) |> clean_data() |> preproc_data()),
  tar_target(wide_data,       build_wide_data(pop_data)),

  # Multiple imputation (no parallelised, but possible with futuremice)
  tar_target(wide_mids,       run_mice(wide_data,
                                       m     = mice_m,
                                       maxit = mice_maxit,
                                       seed  = seed_random)),

  # LTMLE: prepare data, branch over (regime × imputation), pool.
  tar_target(ltmle_data_list, prepare_ltmle_data(wide_mids)),
  tar_target(work_grid_t,     work_grid),
  tar_target(
    ltmle_one,
    fit_ltmle_one(
      regime_label    = work_grid_t$regime_label,
      imp_idx         = work_grid_t$imp_idx,
      ltmle_data_list = ltmle_data_list,
      regimes         = regimes,
      Qform           = Qform,
      gform           = gform,
      sl_libs         = sl_libs
    ),
    pattern   = map(work_grid_t),
    iteration = "list"
  ),
  tar_target(ltmle_results,   pool_ltmle(ltmle_one, work_grid_t$regime_label)),

  # Sensitivity analyses - depend only on wide_mids, thus it runs in parallel
  tar_target(mi_results,      run_gformula(wide_mids,
                                           wide_data_mi = wide_data,
                                           M = gformula_M)),
  # Final comparison + report
  tar_target(comparison,      assemble_comparison(ltmle_results, mi_results)),
  tarchetypes::tar_quarto(report,          "06_full_models.qmd")
)
