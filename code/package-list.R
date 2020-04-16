packages <- c("reldist","dplyr","data.table","reshape","reshape2","stringr","caret","zoo","tidyr","readr","readxl","scales","ggplot2","wesanderson","plm",
              "glmnet","ggplot2","latex2exp","missMDA","bcv","doParallel","boot","tidyverse","mgcv","keras","reticulate","InformationValue","foreign","caret",
              "readstata13","devtools","gtools")

weights <- c("cluster","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights))

library(devtools) 
install_github("susanathey/MCPanel")
install_github("lcallot/lassovar")
install_github("SteffenMoritz/imputeTS")