# main

# machine learning
library(caret)
library(My.stepwise)

# handling data
library(dplyr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(naniar)
# ROC analyses
library(pROC)
library(plotROC)
library(timeROC)

# plot tools
library(plotly)
library(ggplot2)
library(cowplot)
library(GGally)
library(ggpubr)
library(corrplot)
library(ggrepel)

# export to word office
library(readxl)
library(officer)
library(rvg)
library(flextable)

# survival model/ survival analysis
library(survival)
library(survey)
library(survminer)
library(rms)
library(pec)
library(riskRegression)
library(randomForestSRC)
library(SurvMetrics)
library(polspline)


# utilitary
library(reshape2)
library(haven)
library(readr)
library(finalfit)
library(pracma) # to compute AUC (integrale with trapeze)
library(RColorBrewer)
library(here)
library(foreach) #parallel
library(parallel) #parallel
library(doParallel)
library(doFuture)
library(imager)
library(gtools) # combinations functions
library(table1)
######### function to compute AUC despite missing values

path_code <- here("R/")
path_data <- here("data/")
path_figures <-  here("figures/")
path_tables <-  here("tables/")

# create new directory to save models and figures.
dir.create(path_figures, recursive = TRUE)
dir.create(path_tables, recursive = TRUE)

source(paste0(path_code, "load_data.R"))


source(paste0(path_code, "prediction_for_each_class.R"))
source(paste0(path_code, "create_multiple_formula.R"))
source(paste0(path_code, "fit_models.R"))
source(paste0(path_code,"train and test models/estimate_cost_models.R"))
source(paste0(path_code, "f_model_performance.R"))
source(paste0(path_code, "presence_in_formula.R"))
source(paste0(path_code, "train and test models/parameters_by_stage.R"))
source(paste0(path_code, "train and test models/plot_performances_f.R"))
source(paste0(path_code,"train and test models/estimate_cost_models.R"))
source(paste0(path_code,"train and test models/compare_TimeROC_2models.R"))
source("~/Desktop/T1Dprediction_trialnet/R/train and test models/estimate_time.R")
source("~/Desktop/T1Dprediction_trialnet/R/compare_TimeROC_l.R")
source("~/Desktop/T1Dprediction_trialnet/R/clean_formula.R")
source("~/Desktop/T1Dprediction_trialnet/R/compute_TimeROC.R")

       