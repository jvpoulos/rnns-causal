#!/bin/bash
#----------------------------------------------------
#SBATCH -J funds_encoder_decoder_predict           # Job name
#SBATCH -o funds_encoder_decoder_predict.o%j       # Name of stdout output file
#SBATCH -e funds_encoder_decoder_predict.e%j       # Name of stderr error file
#SBATCH -p development		    # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 00:15:00        		# Run time (hh:mm:ss)

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-educpc/treated-gans/weights.500-0.750.hdf5' 'south-educpc' 'treated-gans' 
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-educpc/control/weights.920-0.716.hdf5' 'south-educpc' 'control'

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-educpc/treated-gans/weights.20-0.837.hdf5' 'west-educpc' 'treated-gans' 
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-educpc/control/weights.500-0.650.hdf5' 'west-educpc' 'control'

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-revpc/treated-gans/weights.170-5.641.hdf5' 'south-revpc' 'treated-gans' 
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-revpc/control/weights.1960-3.408.hdf5' 'south-revpc' 'control'

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-revpc/treated-gans/weights.20-4.367.hdf5' 'west-revpc' 'treated-gans' 
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-revpc/control/weights.2000-2.352.hdf5' 'west-revpc' 'control'

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-exppc/treated-gans/weights.70-5.644.hdf5' 'south-exppc' 'treated-gans' 
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/south-exppc/control/weights.1140-3.685.hdf5' 'south-exppc' 'control'

python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-exppc/treated-gans/weights.10-4.750.hdf5' 'west-exppc' 'treated-gans'
python code/predict_encoder_decoder.py 1 'results/encoder-decoder/west-exppc/control/weights.1980-2.393.hdf5' 'west-exppc' 'control'