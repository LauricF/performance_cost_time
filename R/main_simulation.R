library(here)
source(here("R/main_load.R"))
# load data and funcitons
source("~/Desktop/T1Dprediction_trialnet/R/main_load.R")

# compute model is the whole dataset.
# source("~/Desktop/T1Dprediction_trialnet/R/Cox_models_all.R")

source(paste0(path_code,"train and test models/Stage0.R"))
source(paste0(path_code,"train and test models/Stage1.R"))
source(paste0(path_code,"train and test models/Stage2.R"))

# pvalues ROC AUC at 3 and 5 years 
# # best versus other models
source("~/Desktop/T1Dprediction_trialnet/R/train and test models/pvalue_ROC_COX.R")
source("~/Desktop/T1Dprediction_trialnet/R/train and test models/pvalue_ROC_RF.R")
# between modeld
source("~/Desktop/T1Dprediction_trialnet/R/train and test models/pvalue_ROC_models.R")
