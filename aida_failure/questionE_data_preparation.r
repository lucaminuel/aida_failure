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
library(ROSE)
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
df_failed_company$`Legal status` <- 1
df_active_company$`Legal status` <- 0
aida_data_model <- rbind(df_failed_company, df_active_company)
rm(df_failed_company)
rm(df_active_company)
aida_data_model$'Legal status' <- factor(aida_data_model$`Legal status`, levels = c(0, 1), labels = c('0', '1'))
table(aida_data_model$`Legal status`)# Several unbalanced
colon_name_original <- colnames(aida_data_model)
# Change colon name for ROSE 
new_col <- c()
for (x in (1:length(aida_data_model))){
  tmp <- paste0('a',x)
  new_col <- c(new_col,tmp)
}
colnames(aida_data_model) <- new_col
aida_data_model$a1 <- as.factor(aida_data_model$a1) 
#Applying ROSE, resize the dataset to N = 23817 (same lenght of dataset question D)
aida_data_model_rose <- ROSE(a14 ~ ., data = aida_data_model, N= 23817,seed = 42)$data
table(aida_data_model_rose$a14)
colnames(aida_data_model_rose)<- colon_name_original
aida_train <- filter(aida_data_model_rose, aida_data_model_rose$`Last accounting closing date`< 2018)
aida_test <- filter(aida_data_model_rose, aida_data_model_rose$`Last accounting closing date`>= 2018)
#Balanced!
table(aida_train$`Legal status`)
table(aida_test$`Legal status`)
save(aida_test, file='aida_test_e.RData')
save(aida_train, file = 'aida_train_e.RData')
