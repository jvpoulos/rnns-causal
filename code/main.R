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

# Assemble elections data
source(paste0(code.directory,'elections.R')) 
#source(paste0(code.directory,'descriptive.R')) 

# Causal impact estimates

source(paste0(code.directory,'attention-plot-elections.R')) 
source(paste0(code.directory,'impact-plots-elections.R')) 

source(paste0(code.directory,'bsts-elections.R')) 

# DD estimates
source(paste0(code.directory,'dd-elections.R')) 

# Experimental estimates
source(paste0(code.directory,'exp-elections.R')) 
