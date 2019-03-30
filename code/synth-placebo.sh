#!/bin/bash
#----------------------------------------------------
#SBATCH -J synth-placebo           # Job name
#SBATCH -o synth-placebo.o%j       # Name of stdout output file
#SBATCH -e synth-placebo.e%j       # Name of stderr error file
#SBATCH -p normal		        # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 24:00:00        		# Run time (hh:mm:ss)
#SBATCH --mail-user=poulos@berkeley.edu
#SBATCH --mail-type=all    		# Send email at begin and end of job

R --no-save < synth-placebo.R
