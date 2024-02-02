# load data
library(readr)
data_demographics <- read_csv("/Users/laf225/Dropbox/ErinPhDclass/trialnet/data/saved_datasets/june_2023/data_demographics_LF.csv")

data_demographics_ogtt <- read_csv("/Users/laf225/Dropbox/ErinPhDclass/trialnet/data/saved_datasets/june_2023/data_demographics_OGTT_LF.csv")
data_demographics_ogtt <- data_demographics_ogtt %>% filter(AB_group != "ICA positive") %>% # remove the few observations who are ICA positive only
                          filter(!is.na(GRS2) & !is.na(BMI) & !is.na(Index60) & !is.infinite(C_peptide_index_30))

data_euglycemia_trialnet <- data_demographics_ogtt %>% filter(Glycemia_trialnet == "Euglycemia")
data_dysglycemia_trialnet <- data_demographics_ogtt %>% filter(Glycemia_trialnet == "Dysglycemia")

