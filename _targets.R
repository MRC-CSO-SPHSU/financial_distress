# _targets.R — pipeline orchestration for the financial_distress UKHLS analysis.
#
# Heavy compute (mice, LTMLE, gFormulaImpute, IPTW) lives here as cached targets;
# report/05_imputation.qmd is a thin report that reads them via tar_read().
# Statistical code (formulas, regimes, SL library, Rubin pooling) is preserved
# verbatim — extracted from the original chunks into R/ functions.
#
# Parallel backend: future + future.batchtools. The controller process running
# tar_make_future() submits each worker target as its own SLURM job via the
# slurm.tmpl template; locally it falls back to background R processes via
# future.callr. (Branch `main` uses crew.cluster instead; see exp/future
# commit history for the migration rationale.)

pacman::p_load(targets,
               tarchetypes,
               future,
               future.batchtools,
               future.callr)

# Detect SLURM at runtime: require both a SLURM job context AND a working
# sbatch on PATH. The conjunction prevents the local fallback from triggering
# on login nodes that happen to have sbatch but no SLURM_JOB_ID.
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))

if (on_slurm) {
  future::plan(
    future.batchtools::batchtools_slurm,
    template  = "slurm.tmpl",
    resources = list(
      ncpus    = 2,
      memory   = 24 * 1024,  # MB per CPU  (= 48 GB per worker). One LTMLE branch peaked
                             # >24 GB (OOM at the old ceiling); SuperLearner accumulates
                             # fitted objects across 9 Q/g nodes. Re-check seff and trim.
      walltime = 60 * 60,    # seconds     (= 60 min wall; LTMLE branches can run long)
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
tar_option_set(
  packages = c(
    "data.table", "dplyr", "tidyr", "tibble", "purrr", "magrittr",
    "rlang", "here",
    "mice", "ltmle", "SuperLearner", "ranger", "gam", "arm",
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

  # LTMLE: prepare data, branch over (regime × imputation), pool.
  # Each branch is its own SLURM job under future.batchtools, so workers
  # almost never co-locate — ltmle_data_list is materialised from .rds per
  # worker. mori::share() was used under crew (see main branch) when SLURM
  # could pack workers onto one node; not useful here.
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

  # Sensitivity analyses — both depend only on wide_mids, run in parallel
  tar_target(mi_results,      run_gformula(wide_mids,
                                           wide_data_mi = wide_data,
                                           M = gformula_M)),
  tar_target(iptw_fit,        run_iptw(wide_mids)),
  tar_target(iptw_results,    extract_iptw(iptw_fit, wide_mids, wide_data)),

  # Final comparison + report
  tar_target(comparison,      assemble_comparison(ltmle_results, mi_results, iptw_results)),
  tar_quarto(report,          "05_imputation.qmd")
)
