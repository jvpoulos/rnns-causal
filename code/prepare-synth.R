basque <- read.csv(paste0(data.directory,"basque/treated/basque-x.csv"))
germany <- read.csv(paste0(data.directory,"germany/treated/germany-x.csv"))
california <- read.csv(paste0(data.directory,"california/treated/california-x.csv"))

basque.yz <- read.csv(paste0(data.directory,"basque/treated/basque-yz.csv"))
basque.xz <- readRDS(paste0(data.directory,"basque/treated/basque-xz.rds"))

germany.yz <- read.csv(paste0(data.directory,"germany/treated/germany-yz.csv"))
germany.xz <- readRDS(paste0(data.directory,"germany/treated/germany-xz.rds"))

california.yz <- read.csv(paste0(data.directory,"california/treated/california-yz.csv"))
california.xz <- readRDS(paste0(data.directory,"california/treated/california-xz.rds"))

control.outcomes <- list("basque"=basque,"germany"=germany, "california"=california)
treat.covars <- list("basque.yz"=basque.yz, "germany.yz"=germany.yz, "california.yz"=california.yz)
control.covars <- list("basque.xz"=basque.xz, "germany.xz"=germany.xz, "california.xz"=california.xz)

synth.control.outcomes <- lapply(control.outcomes, function(d) {

  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d))
  d.M[is.nan(d.M )] <- NA
  
  # Masked matrix which is 0 for control units and treated units before treatment and 1 for treated units after treatment.
  
  d.mask <- matrix(0, nrow = nrow(d.M), 
                   ncol= ncol(d.M),
                   dimnames = list(rownames(d.M), colnames(d.M)))
  
  return(list("M"=d.M, "mask"=d.mask))
})

synth.treat.covars <- lapply(treat.covars, function(d) {
  
  # Matrix containing time-related covariates (T x N)
  d.Z <- as.matrix(d)
  d.Z[is.nan(d.Z)] <- NA
  
  return(d.Z)
})

synth.control.covars <- lapply(control.covars, function(d) {
  lapply(1:length(d), function(x) {

    # Matrix containing time-related covariates (T x N)
    d.Z <- as.matrix(d[[x]])
    d.Z[is.nan(d.Z)] <- NA
    return(d.Z)
  })
})

saveRDS(synth.control.outcomes, paste0(data.directory,"synth-control-outcomes.rds"))
saveRDS(synth.treat.covars, paste0(data.directory,"synth-treat-covars.rds"))
saveRDS(synth.control.covars, paste0(data.directory,"synth-control-covars.rds"))