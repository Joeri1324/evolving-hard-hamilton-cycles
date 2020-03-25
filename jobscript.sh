#SBATCH --job-name=distantly_supervised_dataset
#SBATCH --time=8:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=48
#SBATCH --ntasks-per-node=1
#SBATCH --mem=16G
#SBATCH --mail-type=END
#SBATCH --mail-user=joeri.sleegers@student.uva.nl

module purge
module load sbt

cp -r $HOME/evolving-hard-hamilton-cycles/ $TMPDIR
cd $TMPDIR/evolving-hard-hamilton-cycles

sbt run
wait

cp -r results/ $HOME/evolving-hard-hamilton-cycles/results
