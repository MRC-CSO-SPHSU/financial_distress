## Minimal reproducer: can crew.cluster launch ONE worker on this cluster?
## Run from inside an srun shell on a compute node, after `conda activate quarto`.
##   Rscript test_crew_slurm.R
## All diagnostic output goes to stderr so you see it as the script runs.

suppressPackageStartupMessages(library(crew.cluster))

message("=== ENVIRONMENT CHECK ===")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("R version:       ", R.version.string)
message("crew.cluster ver: ", as.character(packageVersion("crew.cluster")))
message("crew ver:        ", as.character(packageVersion("crew")))
message("mirai ver:       ", as.character(packageVersion("mirai")))
message("==========================")

ctl <- crew_controller_slurm(
  name            = "probe",
  workers         = 1,
  seconds_idle    = 30,
  options_cluster = crew_options_slurm(
    script_lines = c(
      "module purge",
      "module load apps/miniforge",
      'source "$(conda info --base)/etc/profile.d/conda.sh"',
      "conda activate quarto"
    ),
    cpus_per_task = 1,
    time_minutes  = 5,
    log_output    = "probe-%A.out",
    log_error     = "probe-%A.err"
  )
)

message("--- starting controller ---")
ctl$start()

message("--- pushing trivial task ---")
ctl$push(command = list(
  worker_host = Sys.info()[["nodename"]],
  worker_pid  = Sys.getpid(),
  worker_time = format(Sys.time())
))

message("--- waiting up to 180s for a worker to materialize ---")
ok <- ctl$wait(seconds_timeout = 180)
message("wait() returned: ", ok)

message("--- controller summary (tasks pushed/popped, worker activity) ---")
print(ctl$summary())

message("--- task result (one row tibble: name, command, result, error, ...) ---")
res <- ctl$pop()
print(res)

if (!is.null(res) && !is.null(res$result) && length(res$result) >= 1L) {
  message("--- result payload from worker ---")
  print(res$result[[1]])
}
if (!is.null(res) && !is.null(res$error) && length(res$error) >= 1L &&
    !is.na(res$error[[1]])) {
  message("--- worker reported an error ---")
  print(res$error[[1]])
}

message("--- shutting down ---")
ctl$terminate()
message("done.")
