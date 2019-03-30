#!/bin/bash
#----------------------------------------------------
#SBATCH -J funds_encoder_decoder           # Job name
#SBATCH -o funds_encoder_decoder.o%j       # Name of stdout output file
#SBATCH -e funds_encoder_decoder.e%j       # Name of stderr error file
#SBATCH -p normal		        # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 12:00:00        		# Run time (hh:mm:ss)
#SBATCH --mail-user=poulos@berkeley.edu
#SBATCH --mail-type=all    		# Send email at begin and end of job

python code/train_encoder_decoder.py 0 2000 'west-revpc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'west-revpc' 'control'

python code/train_encoder_decoder.py 0 2000 'south-revpc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'south-revpc' 'control'

python code/train_encoder_decoder.py 0 2000 'west-exppc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'west-exppc' 'control'

python code/train_encoder_decoder.py 0 2000 'south-exppc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'south-exppc' 'control'

python code/train_encoder_decoder.py 0 2000 'west-educpc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'west-educpc' 'control'

python code/train_encoder_decoder.py 0 2000 'south-educpc' 'treated-gans'
python code/train_encoder_decoder.py 1 2000 'south-educpc' 'control'