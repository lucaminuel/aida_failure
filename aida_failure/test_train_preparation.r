library(plyr)
library(dplyr)
library(fitdistrplus)
library(plugdensity)
library(ks)
library(glue)
library(boot)
library(car)
library(penalized)
library(glmnet)
library(caret)
library(tictoc)
library(ROCR)
# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("useful_functions.R")
load(file="aida_finale.RData")

# delete NaN
aida_finale <- na.omit(aida_finale)
df_failed_company <- filter(aida_finale, aida_finale$'Legal status' == 'Bankruptcy' | aida_finale$'Legal status' == 'Dissolved (bankruptcy)')
df_active_company <- filter(aida_finale, aida_finale$'Legal status' != 'Bankruptcy' & aida_finale$'Legal status' != 'Dissolved (bankruptcy)')
rm(aida_finale)
# underampling df_active_company
index<- sample(seq_len(nrow(df_active_company)), size = 12000, replace = FALSE)
df_active_company <- df_active_company[index,]
df_failed_company$`Legal status` <- 1
df_active_company$`Legal status` <- 0
aida_data_model <- rbind(df_failed_company, df_active_company)
rm(df_failed_company)
rm(df_active_company)
aida_data_model$'Legal status' <- factor(aida_data_model$`Legal status`, levels = c(0, 1), labels = c('0', '1'))
# split in train and test 
aida_train <- filter(aida_data_model, aida_data_model$`Last accounting closing date`< 2018)
aida_test <- filter(aida_data_model, aida_data_model$`Last accounting closing date`>= 2018)
rm(aida_data_model)
save(aida_test, file = 'aida_train.RData')
save(aida_train, file = 'aida_test.RData')
