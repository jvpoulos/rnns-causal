###################################
# Main                            #
###################################

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set directories
data.directory <- "~/Dropbox/github/rnns-causal/data/"
code.directory <-"~/Dropbox/github/rnns-causal/code/"
results.directory <-"~/Dropbox/github/rnns-causal/results/"

descriptive <- FALSE
if(descriptive){
source(paste0(code.directory,'descriptive.R')) 
}

# Causal impact estimates: simulated data

source(paste0(code.directory,'encoder-decoder-sim.R'))

source(paste0(code.directory,'lstm-sim.R'))

source(paste0(code.directory,'bsts-sim.R')) 
source(paste0(code.directory,'synth-sim.R')) 

source(paste0(code.directory,'sim-plot.R')) # plot MSPE from all models

# Causal impact estimates: basque country

# First run synth-basque.R in gans-causal/code

source(paste0(code.directory,'encoder-decoder-basque.R'))

source(paste0(code.directory,'lstm-basque.R'))

source(paste0(code.directory,'bsts-basque.R')) 

source(paste0(code.directory,'basque-plot.R')) # plot MSPE from all models

# california

# First run synth-california.R in gans-causal/code

source(paste0(code.directory,'encoder-decoder-california.R'))

source(paste0(code.directory,'lstm-california.R'))

source(paste0(code.directory,'bsts-california.R')) 

source(paste0(code.directory,'california-plot.R')) # plot MSPE from all models

# west germany

# First run synth-germany.R in gans-causal/code

source(paste0(code.directory,'encoder-decoder-germany.R'))

source(paste0(code.directory,'lstm-germany.R'))

source(paste0(code.directory,'bsts-germany.R')) 

source(paste0(code.directory,'germany-plot.R')) # plot MSPE from all models

# Plot benchmarks

source(paste0(code.directory,'plot-benchmarks.R')) 

# Causal impact estimates: mayoral elections

# Assemble elections data
source(paste0(code.directory,'elections.R')) 

source(paste0(code.directory,'encoder-decoder-elections.R'))# run elections.R first

source(paste0(code.directory,'lstm-elections.R'))

source(paste0(code.directory,'bsts-elections.R')) # run elections.R first
source(paste0(code.directory,'synth-elections.R')) # run elections.R first

# GBR estimates
source(paste0(code.directory,'gbr-elections.R')) 
