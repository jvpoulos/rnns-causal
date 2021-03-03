packages <- c("reldist","dplyr","data.table","reshape","reshape2","stringr","caret","zoo","tidyr","readr","readxl","scales","ggplot2","wesanderson","plm",
              "glmnet","ggplot2","latex2exp","missMDA","bcv","doParallel","boot","tidyverse","mgcv","keras","reticulate","InformationValue","foreign","caret",
              "readstata13","devtools","gtools","ggpubr")

weights <- c("cluster","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights))

library(devtools) 
install_github("jvpoulos/MCPanel") # propensity weighting of objective fn.; ADH clips gradients
install_github("jvpoulos/lassovar") # standardization option