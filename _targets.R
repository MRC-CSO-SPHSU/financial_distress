# _targets.R — pipeline orchestration for the financial_distress UKHLS analysis.
#
# Heavy compute (mice, LTMLE, gFormulaImpute, IPTW) lives here as cached targets;
# report/05_imputation.qmd is a thin report that reads them via tar_read().
# Statistical code (formulas, regimes, SL library, Rubin pooling) is preserved
# verbatim — extracted from the original chunks into R/ functions.
#
# Local: `tar_make()` runs the DAG in-process.
# Cluster: same call dispatches LTMLE branches as SLURM jobs via crew.cluster.

pacman::p_load(targets,
               tarchetypes,
               crew,
               crew.cluster)

# Detect SLURM at runtime: require BOTH a SLURM job context AND a working sbatch on PATH. This
# avoids the silent fallback to crew_controller_local on clusters where compute nodes lack sbatch
# (or where nested submission is forbidden by policy).
in_slurm_job <- nzchar(Sys.getenv("SLURM_JOB_ID"))
sbatch_path  <- Sys.which("sbatch")
on_slurm     <- in_slurm_job && nzchar(sbatch_path)

message("=== CREW CONTROLLER SELECTION ===")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", sbatch_path, "'")
message("in_slurm_job:    ", in_slurm_job)
message("on_slurm:        ", on_slurm)
message("controller type: ", if (on_slurm) "crew_controller_slurm" else "crew_controller_local")
message("=================================")

controller_obj <- if (on_slurm) {
  dir.create("crew_logs", showWarnings = FALSE)
  crew_controller_slurm(
    name                           = "fd_slurm",
    workers                        = 20,
    seconds_idle                   = 300,
    options_cluster                = crew_options_slurm(
      script_lines = c(
        "module purge",
        "module load apps/miniforge",
        'source "$(conda info --base)/etc/profile.d/conda.sh"',
        "conda activate quarto",
        "export OMP_NUM_THREADS=1",
        "export OPENBLAS_NUM_THREADS=1",
        "export MKL_NUM_THREADS=1",
        "export RANGER_NUM_THREADS=1"
      ),
      cpus_per_task            = 2,
      memory_gigabytes_per_cpu = 6,
      time_minutes             = 60,
      partition                = NULL,            # set if your cluster requires one
      log_output               = "crew_logs/crew-%A.out",
      log_error                = "crew_logs/crew-%A.err"
    )
  )
} else {
  crew::crew_controller_local(name = "fd_local", workers = 4)
}

# ---- Packages attached to every target's evaluation environment ------------
tar_option_set(
  packages = c(
    "data.table", "dplyr", "tidyr", "tibble", "purrr", "magrittr",
    "rlang", "here",
    "mice", "ltmle", "SuperLearner", "ranger", "gam", "arm",
    "gFormulaMI",
    "mori",
    "quarto"
  ),
  format = "rds",
  seed   = 20260522,
  controller = controller_obj
)

# ---- Source extracted functions (R/) and project helpers (fnct/) -----------
for (f in c(list.files("R",    "\\.R$", full.names = TRUE),
            list.files("fnct", "\\.R$", full.names = TRUE))) source(f)

# ---- Configuration ---------------------------------------------------------
# Dev settings; flip to final values for the production run.
mice_m      <- 5    # final: 35
mice_maxit  <- 10   # final: 15
gformula_M  <- 20   # final: 50
seed_random <- 20260522

sl_libs <- c("SL.mean", "SL.glm", "SL.bayesglm", "SL.gam", "SL.ranger")

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

Qform <- c(
    pcs_lagged_0 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base",
    pcs_lagged_1 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base + econ_dist_bin_0 + pcs_lagged_0 + sf12mcs_dv_0",
    pcs_lagged_2 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base +
                               econ_dist_bin_0 + pcs_lagged_0 + pcs_lagged_1 +
                               econ_dist_bin_1 + sf12mcs_dv_1 + sf12mcs_dv_0",
    sf12mcs_dv_0 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base + pcs_lagged_0 + econ_dist_bin_0",
    sf12mcs_dv_1 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base + pcs_lagged_0 + 
                               econ_dist_bin_0 + pcs_lagged_1 + econ_dist_bin_1 + sf12mcs_dv_0",
    sf12mcs_dv_2 = "Q.kplus1 ~ race_base + sex_dv_base + hiqual_dv_base + 
                               pcs_lagged_0 + econ_dist_bin_0 + pcs_lagged_1 + econ_dist_bin_1 + 
                               pcs_lagged_2 + econ_dist_bin_2 + sf12mcs_dv_0 + sf12mcs_dv_1"
)

gform <- c(
    econ_dist_bin_0 = "econ_dist_bin_0 ~ race_base + sex_dv_base + hiqual_dv_base + pcs_lagged_0",
    econ_dist_bin_1 = "econ_dist_bin_1 ~ race_base + sex_dv_base + hiqual_dv_base + pcs_lagged_1 + econ_dist_bin_0 + sf12mcs_dv_0",
    econ_dist_bin_2 = "econ_dist_bin_2 ~ race_base + sex_dv_base + hiqual_dv_base + pcs_lagged_2 + econ_dist_bin_0 + econ_dist_bin_1 + sf12mcs_dv_1 + sf12mcs_dv_0"
)

# (regime × imputation) grid for dynamic branching of the LTMLE step.
work_grid <- tidyr::expand_grid(
  regime_label = names(regimes),
  imp_idx      = seq_len(mice_m)
)

# ---- DAG -------------------------------------------------------------------
list(
  # Data prep
  tar_target(pop_data,        import_data(force = FALSE) |> clean_data() |> preproc_data()),
  tar_target(wide_data,       build_wide_data(pop_data)),

  # Multiple imputation (single-threaded; one target, cached)
  tar_target(wide_mids,       run_mice(wide_data,
                                       m     = mice_m,
                                       maxit = mice_maxit,
                                       seed  = seed_random)),

  # LTMLE: prepare data, branch over (regime × imputation), pool
#  tar_target(ltmle_data_list, prepare_ltmle_data(wide_mids)),

  # Share imputed datasets through OS shared memory so workers co-located on
  # the same node attach via zero-copy ALTREP instead of holding independent
  # copies. mori only shares within a machine, so the saving materialises only
  # for workers SLURM packs onto the same node (or under crew_controller_local).
  # `cue = "always"` because the shared segment lives only for the duration of
  # the current tar_make() — a stale .rds reference from a previous run would
  # point at a segment that no longer exists.
#  tar_target(
#    ltmle_data_list_shared,
#    mori::share(ltmle_data_list),
#    cue = tar_cue(mode = "always")
#  ),
#  tar_target(work_grid_t,     work_grid),
#  tar_target(
#    ltmle_one,
#    fit_ltmle_one(
#      regime_label    = work_grid_t$regime_label,
#      imp_idx         = work_grid_t$imp_idx,
#      ltmle_data_list = ltmle_data_list_shared,
#      regimes         = regimes,
#      Qform           = Qform,
#      gform           = gform,
#      sl_libs         = sl_libs
#    ),
#    pattern   = map(work_grid_t),
#    iteration = "list"
#  ),
#  tar_target(ltmle_results,   pool_ltmle(ltmle_one, work_grid_t$regime_label)),

  # Sensitivity analyses — both depend only on wide_mids, run in parallel
  tar_target(mi_results,      run_gformula(wide_mids,
                                           wide_data_mi = wide_data,
                                           M = gformula_M)),
  tar_target(iptw_fit,        run_iptw(wide_mids)),
  tar_target(iptw_results,    extract_iptw(iptw_fit, wide_mids, wide_data)),

  # Final comparison + report
  tar_target(comparison,      assemble_comparison(mi_results, iptw_results)),
  tar_quarto(report,          "05_imputation.qmd")
)
