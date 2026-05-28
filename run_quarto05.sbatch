#!/bin/bash
#SBATCH --job-name=fd_targets
#SBATCH --time=24:00:00            # controller blocks while batchtools worker jobs run
#SBATCH --cpus-per-task=2          # orchestration only; worker targets go to child SLURM jobs
#SBATCH --mem=96G                  # MICE mids + gFormulaMI + IPTW held in controller memory
#SBATCH --output=targets_%j.out
#SBATCH --error=targets_%j.err
#SBATCH --account=none
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk

set -uo pipefail

module purge
module load apps/miniforge
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

# Keep BLAS / ranger from oversubscribing the controller process.
# Worker jobs export their own copy via slurm.tmpl.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

# Node-local scratch for batchtools worker spillover and targets temp files.
export TMPDIR="${SLURM_TMPDIR:-/tmp}"

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version

cd "${SLURM_SUBMIT_DIR}"

# tar_make_future() dispatches each worker target as its own SLURM job via
# future.batchtools (template: slurm.tmpl). `workers = 4` caps concurrent
# child jobs. The final tar_quarto(report) target renders 05_imputation.qmd.
Rscript -e 'targets::tar_make_future(workers = 4L)' 2>&1 | tee "targets_make_${SLURM_JOB_ID}.log"
status=${PIPESTATUS[0]}
echo "tar_make_future exit status: $status"

exit $status
