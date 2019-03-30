#!/bin/bash
#----------------------------------------------------
#SBATCH -J funds_lstm_predict           # Job name
#SBATCH -o funds_lstm_predict.o%j       # Name of stdout output file
#SBATCH -e funds_lstm_predict.e%j       # Name of stderr error file
#SBATCH -p development		    # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 00:15:00        		# Run time (hh:mm:ss)

python code/predict_lstm.py 1 'results/lstm/south-educpc/treated-gans/weights.320-0.084.hdf5' 'south-educpc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/south-educpc/control/weights.1040-0.771.hdf5' 'south-educpc' 'control'

python code/predict_lstm.py 1 'results/lstm/west-educpc/treated-gans/weights.140-0.122.hdf5' 'west-educpc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/west-educpc/control/weights.1680-0.910.hdf5' 'west-educpc' 'control'

python code/predict_lstm.py 1 'results/lstm/south-revpc/treated-gans/weights.1740-1.446.hdf5' 'south-revpc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/south-revpc/control/weights.1910-4.987.hdf5' 'south-revpc' 'control'

python code/predict_lstm.py 1 'results/lstm/west-revpc/treated-gans/weights.1970-6.458.hdf5' 'west-revpc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/west-revpc/control/weights.1340-4.422.hdf5' 'west-revpc' 'control'

python code/predict_lstm.py 1 'results/lstm/south-exppc/treated-gans/weights.30-2.550.hdf5' 'south-exppc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/south-exppc/control/weights.1920-18.812.hdf5' 'south-exppc' 'control'

python code/predict_lstm.py 1 'results/lstm/west-exppc/treated-gans/weights.20-35.925.hdf5' 'west-exppc' 'treated-gans'
python code/predict_lstm.py 1 'results/lstm/west-exppc/control/weights.840-2.373.hdf5' 'west-exppc' 'control'