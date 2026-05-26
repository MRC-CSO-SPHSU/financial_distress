#!/bin/bash
#SBATCH --job-name=imp05
#SBATCH --time=8:00:00            # bump for m=35 final run
#SBATCH --cpus-per-task=10        # 10 workers covers 8x5=40 fits in ~4 passes
#SBATCH --mem=96G                 # SL.ranger eats a lot of memory
#SBATCH --output=quarto_%j.out
#SBATCH --error=quarto_%j.err
#SBATCH --account=none
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk

set -uo pipefail

module purge
module load apps/miniforge
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

# Keep BLAS / ranger from oversubscribing inside each future worker
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

# Node-local scratch for future/furrr spillover
export TMPDIR="${SLURM_TMPDIR:-/tmp}"

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version

cd "${SLURM_SUBMIT_DIR}"

quarto render code/05_imputation.qmd 2>&1 | tee "quarto_render_${SLURM_JOB_ID}.log"
status=${PIPESTATUS[0]}
echo "quarto render exit status: $status"
exit $status