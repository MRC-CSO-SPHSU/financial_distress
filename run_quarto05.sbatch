#!/bin/bash
#SBATCH --job-name=fd_targets
#SBATCH --time=24:00:00            # controller blocks waiting on crew LTMLE workers
#SBATCH --cpus-per-task=2          # orchestration only; LTMLE branches go to crew workers
#SBATCH --mem=64G                  # MICE mids + mori::share segment + gFormulaMI + IPTW
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
# Worker jobs export their own copy via crew_controller_slurm(script_lines = ...) in _targets.R.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

# Node-local scratch for crew worker spillover and targets temp files.
export TMPDIR="${SLURM_TMPDIR:-/tmp}"

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version

cd "${SLURM_SUBMIT_DIR}"

# tar_make() runs in-process targets here and dispatches the ltmle_one map()
# branches as child SLURM jobs via crew_controller_slurm (see _targets.R).
# The final tar_quarto(report) target renders report/05_imputation.qmd.
Rscript -e 'targets::tar_make()' 2>&1 | tee "targets_make_${SLURM_JOB_ID}.log"
status=${PIPESTATUS[0]}
echo "tar_make exit status: $status"
exit $status
