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

source(paste0(code.directory,'impact-plots-sim.R'))

source(paste0(code.directory,'bsts-sim.R')) 
source(paste0(code.directory,'synth-sim.R')) 

source(paste0(code.directory,'sim-plot.R')) # plot MSPE from all models

# Causal impact estimates: basque country

source(paste0(code.directory,'synth-basque.R')) # run first

source(paste0(code.directory,'impact-plots-basque.R'))

source(paste0(code.directory,'bsts-basque.R')) 

source(paste0(code.directory,'basque-plot.R')) # plot MSPE from all models

# Causal impact estimates: mayoral elections

# Assemble elections data
source(paste0(code.directory,'elections.R')) 

source(paste0(code.directory,'impact-plots-elections.R')) # run elections.R first

source(paste0(code.directory,'bsts-elections.R')) # run elections.R first
source(paste0(code.directory,'synth-elections.R')) # run elections.R first

# GBR estimates
source(paste0(code.directory,'gbr-elections.R')) 
source(paste0(code.directory,'gbr-elections-pred.R'))  # using predictions 
