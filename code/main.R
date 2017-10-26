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

# State groups

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.land.states <- state.abb[!state.abb %in% pub.states] # 20 state land states
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states
southern.state <- c("GA","NC","SC","TN","TX","VA") # 6 southern state land states

source(paste0(code.directory,'census-county-state.R')) # get census data

# Causal estimates on state capacity

source(paste0(code.directory,'capacity-state.R'))  # load and export capacity data [need to run census-county-state.R before]

source(paste0(code.directory,'impact-plots-rev-exp.R')) # effects of 1866 SHA/1889 HSA restrictions on revenue and expenditure

source(paste0(code.directory,'impact-plots-ed-exp.R')) # effects 866 SHA/1889 HSA restrictions treated (south) education

source(paste0(code.directory,'attention-plot-capacity.R')) # Attention heatmap

source(paste0(code.directory,'dd-capacity.R')) 

# Causal estimates on land patents

source(paste0(code.directory,'patents.R')) 

source(paste0(code.directory,'impact-plots-patents.R')) 

source(paste0(code.directory,'dd-patents.R')) 

# DD estimates on inequality and tenancy

source(paste0(code.directory,'dd-census.R')) 

# DD estimates on railroads

source(paste0(code.directory,'railroads.R')) 

source(paste0(code.directory,'rr-impact.R')) 

source(paste0(code.directory,'impact-plots.rr.R')) 

source(paste0(code.directory,'dd-railroads.R')) 

# DD estimates on Rhode and Strumpf tax data

source(paste0(code.directory,'taxes-revenues.R')) 

source(paste0(code.directory,'dd-taxes.R')) 

# Descriptive plots, attention heatmaps, scatter plots

source(paste0(code.directory,'descriptive.R')) # Descriptive plots

source(paste0(code.directory,'ineq-capacity.R')) # inequality vs. state capacity 

source(paste0(code.directory,'ineq-taxes.R')) # inequality vs. state capacity 