library(dplyr)
library(plyr)
library(fitdistrplus)
library(plugdensity)
library(ks)
library(glue)
# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('aida.RData')
source("useful_functions.R")



# We consider failed company as 'Bankruptcy' or 'Dissolved (bankruptcy)'
df_failed_company <- filter(aida, aida$'Legal status' == 'Bankruptcy' | aida$'Legal status' == 'Dissolved (bankruptcy)')
df_active_company <- filter(aida, aida$'Legal status' != 'Bankruptcy' & aida$'Legal status' != 'Dissolved (bankruptcy)')
rm(aida)
# We delete useless columns for our tasks
to.remove <- c('Company name','File','Province','Registered office address - Commune ISTAT code','Tax code number')



`%ni%` <- Negate(`%in%`)
df_failed_company <- subset(df_failed_company,select = names(df_failed_company) %ni% to.remove)
df_active_company <- subset(df_active_company,select = names(df_active_company) %ni% to.remove)



################
#FAILED COMPANY#
###############
nms <- names(df_failed_company)
data_meno_uno <- df_failed_company[grepl("(- 1)$", nms)] #columns' name ending with -1
data_meno_due <- df_failed_company[grepl("(- 2)$", nms)] #columns' name ending with -2
variabili_comuni <- df_failed_company[grepl("[^( - 1 & - 2)]$", nms)] # columns without -1 & -2
nomi_var_comuni <- colnames(variabili_comuni)



# columns without yr
without_yr <- rematch2::re_match(nomi_var_comuni,".*(?<!yr)$") 
without_yr_wout_na <- without_yr$.match
without_yr_wout_na <- na.omit(without_yr_wout_na)
colonne_univoche <- variabili_comuni[,without_yr_wout_na]
nomi_var_colonne_univoche <- colnames(colonne_univoche)



# renames columns -1&-2
to.remove <- nomi_var_colonne_univoche
`%ni%` <- Negate(`%in%`)
rename_data_col <- subset(variabili_comuni,select = names(variabili_comuni) %ni% to.remove)
nomi_rename_data <- colnames(rename_data_col)
colnames(data_meno_uno) <- nomi_rename_data
colnames(data_meno_due) <- nomi_rename_data



data_meno_uno_star <- cbind(data_meno_uno, colonne_univoche)
data_meno_uno_star$`Last accounting closing date` <- data_meno_uno_star$`Last accounting closing date`- 1
data_meno_uno_star$`Legal status` <- 'Active'



data_meno_due_star <- cbind(data_meno_due, colonne_univoche)
data_meno_due_star$`Last accounting closing date` <- data_meno_due_star$`Last accounting closing date`- 2
data_meno_due_star$`Legal status` <- 'Active'



nuovi_data_active_from_data_failed <- rbind(data_meno_uno_star, data_meno_due_star)


################
#ACTIVE COMPANY#
################
nms_2 <- names(df_active_company)
data_meno_uno_2 <- df_active_company[grepl("(- 1)$", nms_2)] #dataset con solo -1
data_meno_due_2 <- df_active_company[grepl("(- 2)$", nms_2)] #dataset con solo -2



variabili_comuni_2 <- df_active_company[grepl("[^( - 1 & - 2)]$", nms_2)] # variabili senza -1 & -2
nomi_var_comuni_2 <- colnames(variabili_comuni_2)



# columns without yr
without_yr_2 <- rematch2::re_match(nomi_var_comuni_2,".*(?<!yr)$") 
without_yr_wout_na_2 <- without_yr_2$.match
without_yr_wout_na_2 <- na.omit(without_yr_wout_na_2)
colonne_univoche_2 <- variabili_comuni_2[,without_yr_wout_na_2]
nomi_var_colonne_univoche_2 <- colnames(colonne_univoche_2)



## renames columns -1&-2
to.remove_2 <- nomi_var_colonne_univoche_2



`%ni%` <- Negate(`%in%`)
rename_data_col_2 <- subset(variabili_comuni_2,select = names(variabili_comuni_2) %ni% to.remove_2)
nomi_rename_data_2 <- colnames(rename_data_col_2)
colnames(data_meno_uno_2) <- nomi_rename_data_2
colnames(data_meno_due_2) <- nomi_rename_data_2



data_meno_uno_star_2 <- cbind(data_meno_uno_2, colonne_univoche_2)
data_meno_uno_star_2$`Last accounting closing date` <- data_meno_uno_star_2$`Last accounting closing date`- 1



data_meno_due_star_2 <- cbind(data_meno_due_2, colonne_univoche_2)
data_meno_due_star_2$`Last accounting closing date` <- data_meno_due_star_2$`Last accounting closing date`- 2



data_active_finale <- rbind(variabili_comuni_2, data_meno_uno_star_2, data_meno_due_star_2, 
                            nuovi_data_active_from_data_failed)



data_failed_finale <- variabili_comuni



# bind active and failed, rename ATECO and adding Age
aida_finale <- rbind(data_active_finale,data_failed_finale)
aida_finale$Age <-aida_finale$`Last accounting closing date`- aida_finale$`Incorporation year`
aida_finale$`Incorporation year`<- NULL
ateco <- aida_finale$`ATECO 2007code`
aida_finale$`ATECO 2007code` <- sapply(ateco,function_sub_ateco)
save(aida_finale, file = 'aida_finale.RData')
aida_failed_finale <- filter(aida_finale, aida_finale$'Legal status' == 'Bankruptcy' | aida_finale$'Legal status' == 'Dissolved (bankruptcy)')
aida_active_finale <- filter(aida_finale, aida_finale$'Legal status' != 'Bankruptcy' & aida_finale$'Legal status' != 'Dissolved (bankruptcy)')
save(aida_failed_finale, file = 'aida_failed_finale.RData')
save(aida_active_finale, file = 'aida_active_finale.RData')
rm(aida)
