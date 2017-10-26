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

# Descriptive plots
source(paste0(code.directory,'descriptive.R')) 

# Experimental estimates replicating P&G 2008
source(paste0(code.directory,'pgrep.R')) 
