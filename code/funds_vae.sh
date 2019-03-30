#!/bin/bash
#----------------------------------------------------
#SBATCH -J funds_vae           # Job name
#SBATCH -o funds_vae.o%j       # Name of stdout output file
#SBATCH -e funds_vae.e%j       # Name of stderr error file
#SBATCH -p normal		        # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 12:00:00        		# Run time (hh:mm:ss)
#SBATCH --mail-user=poulos@berkeley.edu
#SBATCH --mail-type=all    		# Send email at begin and end of job

python train_vae.py 0 5000 'west-educpc' 'treated-gans'
python train_vae.py 1 5000 'west-educpc' 'control'

python train_vae.py 0 5000 'south-educpc' 'treated-gans'
python train_vae.py 1 5000 'south-educpc' 'control'
