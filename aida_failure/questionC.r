# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
library(tidyverse)
source("useful_functions.R")
load(file = "aida_failed_finale.RData")
load(file = "aida_active_finale.RData")




##############
#QUESTION (C)#
##############
#############################
#COMPARE SIZE (TYPE COMPANY)#
#############################
#########
#GENERAL#
#########
year <- 2016
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year = year) #failed firm at year given feature
size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Total assetsth EURLast avail. yr",year = year)) #total firm at 2016 with variable
size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
sizebin <- 0.1 #we set size bin analyzing hist(variable$failed)
variable <- list(failed = log(size_failed_at_year), total = log(size_total_at_year))
id2bin_failed <- bin_assignment(variable$failed,sizebin,max = max(variable$failed), min = min(variable$failed))
id2bin_total <- bin_assignment(variable$total,sizebin,max = max(variable$failed), min = min(variable$failed))
pB <- table(id2bin_total)/length(variable$total)
pAeB <- table(id2bin_failed)/length(variable$total)
prob_failed_give_size <- pAeB/pB
prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
prob_failed_give_size<- prob_failed_give_size/(sum(prob_failed_give_size))
plot(pAeB)
plot(pB)
plot(prob_failed_give_size,ylab = 'p(failed|size)',  xlab = '(Bin Log Total Asset)', 
     main = 'Probability of failures conditional to size')
# Conditional distribution General given size
list_gen<- list(prob = prob_failed_give_size)
names(list_gen)[1]<- 'General'




########
#REGION#
########
############################
#COMPARE FIRM BETWEEN N-C-S#
############################
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
nord <- c("Valle d'Aosta/Vallée d'Aoste", "Piemonte", "Lombardia", "Trentino-Alto Adige", "Liguria", 
          "Friuli-Venezia Giulia", "Veneto", "Emilia-Romagna")
centro <- c("Lazio", "Marche", "Toscana", "Umbria")
sud <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia")
isola <- c("Sicilia", "Sardegna")
data_nord <- prob_condition_size_region(failedyear = failedyear,activeyear = activeyear, region = nord, sizebin = sizebin)
data_centro <- prob_condition_size_region(failedyear = failedyear,activeyear = activeyear, region = centro, sizebin = sizebin)
data_sud <- prob_condition_size_region(failedyear = failedyear,activeyear = activeyear, region = sud, sizebin = sizebin)
data_isola <- prob_condition_size_region(failedyear = failedyear,activeyear = activeyear, region = isola, sizebin = sizebin)
#chi test: use correction alpha for independent
chi_nord <- chi_test_result_comparison(data_nord, value = 1-(1-0.05)^(length(data_nord)-1))
chi_centro <- chi_test_result_comparison(data_centro, value = 1-(1-0.05)^(length(data_centro)-1))
chi_sud <- chi_test_result_comparison(data_sud,value = 1-(1-0.05)^(length(data_sud)-1))
chi_isola <- chi_test_result_comparison(data_isola, value = 1-(1-0.05)^(length(data_isola)-1))

#######################
#COMPARE FIRM IN N-C-S#
#######################

failedyear_nord <- filter(failedyear, failedyear$`Registered office address - Region` == nord)
levels(failedyear_nord$`Registered office address - Region`) <-  list('nord' = nord)

activeyear_nord <- filter(activeyear, activeyear$`Registered office address - Region` == nord)
levels(activeyear_nord$`Registered office address - Region`) <-  list('nord' = nord)

failedyear_centro <- filter(failedyear, failedyear$`Registered office address - Region` == centro)
levels(failedyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

activeyear_centro <- filter(activeyear, activeyear$`Registered office address - Region` == centro)
levels(activeyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

failedyear_sud<- filter(failedyear, failedyear$`Registered office address - Region` == sud)
levels(failedyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

activeyear_sud <- filter(activeyear, activeyear$`Registered office address - Region` == sud)
levels(activeyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

failedyear2 <- rbind(failedyear_nord, failedyear_centro, failedyear_sud)
activeyear2 <- rbind(activeyear_nord, activeyear_centro, activeyear_sud)
tot <- c('nord', 'centro', 'sud')
data_tot <- prob_condition_size_region(failedyear2, activeyear2, tot)
chi_tot <- chi_test_result_comparison(data_tot, value = 1-(1-0.05)^(length(data_tot)-1))

#############################
#COMPARE SIZE (TYPE COMPANY)#
#############################
#######
#ATECO#
#######
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
table(failedyear$`ATECO 2007code`)
most_frequent_ateco <- c("C", "F", "G")
data_ateco <- prob_condition_size_ateco(failedyear = failedyear,activeyear = activeyear, ateco = most_frequent_ateco,sizebin = sizebin)
#Compare with general
data_ateco_comparison <- append(data_ateco, list_gen)
#Bonferroni correction
chi_ateco_comparison <- chi_test_result_comparison(data_ateco_comparison, value= 0.05*(length(data_ateco_comparison)-1))

#############################
#COMPARE SIZE (TYPE COMPANY)#
#############################
#########################
#(SPECIFIC COMPANY FORM)#
#########################
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
table(failedyear$`Legal form`)
most_frequent_company <- c("S.R.L.")
data_company <- prob_condition_size_company(failedyear = failedyear,activeyear = activeyear, company = most_frequent_company,sizebin = sizebin)
#Compare with general
data_company_comparison <- append(data_company, list_gen)
chi_company <- chi_test_result_comparison(data_company_comparison, value = 0.05 )


###################
#COMPARE LIQUIDITY#
###################
#########
#GENERAL#
#########
year <- 2016
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
failedyear <- failedyear[which(is.na(failedyear$`Liquidity ratioLast avail. yr`) == FALSE),]
activeyear <- activeyear[which(is.na(activeyear$`Liquidity ratioLast avail. yr`) == FALSE),]
liquidity_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year = year) #failed firm at year given feature
liquidity_total_at_year <-  c(liquidity_failed_at_year,select_feature(activeyear,"Liquidity ratioLast avail. yr",year = year)) #total firm at 2016 with variable
sizebin <- 0.05 #we set liquidity bin analyzing hist(variable$failed)
variable <- list(failed = (liquidity_failed_at_year), total = (liquidity_total_at_year))
id2bin_failed <- bin_assignment(variable$failed,sizebin,max = max(variable$failed), min = min(variable$failed))
id2bin_total <- bin_assignment(variable$total,sizebin,max = max(variable$failed), min = min(variable$failed))
pB <- table(id2bin_total)/length(variable$total)
pAeB <- table(id2bin_failed)/length(variable$total)
prob_failed_give_liquidity <- pAeB/pB
prob_failed_give_liquidity[which(is.na(prob_failed_give_liquidity) == TRUE)] = 0
prob_failed_give_liquidity<- prob_failed_give_liquidity/(sum(prob_failed_give_liquidity))
plot(pAeB)
plot(pB)
plot(prob_failed_give_liquidity,ylab = 'p(failed|liquidity)',  xlab = '(bin Liquidity ratio)', 
     main = 'Probability of failures conditional to liquidity')
list_gen<- list(prob = prob_failed_give_liquidity)
names(list_gen)[1]<- 'General'


##################################
#COMPARE LIQUIDITY (TYPE COMPANY)#
##################################
########
#REGION#
########
############################
#COMPARE FIRM BETWEEN N-C-S#
############################
nord <- c("Valle d'Aosta/Vallée d'Aoste", "Piemonte", "Lombardia", "Trentino-Alto Adige", "Liguria", 
          "Friuli-Venezia Giulia", "Veneto", "Emilia-Romagna")
centro <- c("Lazio", "Marche", "Toscana", "Umbria")
sud <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia")
isola <- c("Sicilia", "Sardegna")


data_nord <- prob_condition_liquidity_region(failedyear = failedyear,activeyear = activeyear,  region = nord, sizebin = sizebin)
data_centro <- prob_condition_liquidity_region(failedyear = failedyear,activeyear = activeyear, region = centro, sizebin = sizebin)
data_sud <- prob_condition_liquidity_region(failedyear = failedyear,activeyear = activeyear, region = sud, sizebin = sizebin)
data_isola <- prob_condition_liquidity_region(failedyear = failedyear,activeyear = activeyear, region = isola, sizebin = sizebin)

Sidak_nord =1-(1-0.05)^(length(nord)-1)
Sidak_centro =1-(1-0.05)^(length(centro)-1)
Sidak_sud =1-(1-0.05)^(length(sud)-1)
Sidak_isola =1-(1-0.05)^(length(isola)-1)

chi_nord <- chi_test_result_comparison(data_nord, value = Sidak_nord)
chi_centro <- chi_test_result_comparison(data_centro, value = Sidak_centro)
chi_sud <- chi_test_result_comparison(data_sud, value = Sidak_sud)
chi_isola <- chi_test_result_comparison(data_isola, value = Sidak_isola)

#######################
#COMPARE FIRM IN N-C-S#
#######################

failedyear_nord <- filter(failedyear, failedyear$`Registered office address - Region` == nord)
levels(failedyear_nord$`Registered office address - Region`) <-  list('nord' = nord)

activeyear_nord <- filter(activeyear, activeyear$`Registered office address - Region` == nord)
levels(activeyear_nord$`Registered office address - Region`) <-  list('nord' = nord)

failedyear_centro <- filter(failedyear, failedyear$`Registered office address - Region` == centro)
levels(failedyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

activeyear_centro <- filter(activeyear, activeyear$`Registered office address - Region` == centro)
levels(activeyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

failedyear_sud<- filter(failedyear, failedyear$`Registered office address - Region` == sud)
levels(failedyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

activeyear_sud <- filter(activeyear, activeyear$`Registered office address - Region` == sud)
levels(activeyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

failedyear2 <- rbind(failedyear_nord, failedyear_centro, failedyear_sud)
activeyear2 <- rbind(activeyear_nord, activeyear_centro, activeyear_sud)
tot <- c('nord', 'centro', 'sud')
data_tot <- prob_condition_liquidity_region(failedyear2, activeyear2, tot)
chi_tot <- chi_test_result_comparison(data_tot, value = 1-(1-0.05)^(length(tot)-1))



###################
#COMPARE LIQUIDITY#
###################
#######
#ATECO#
#######
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
failedyear <- failedyear[which(is.na(failedyear$`Liquidity ratioLast avail. yr`) == FALSE),]
activeyear <- activeyear[which(is.na(activeyear$`Liquidity ratioLast avail. yr`) == FALSE),]
table(failedyear$`ATECO 2007code`)
most_frequent_ateco <- c("C", "F", "G")
data_ateco <- prob_condition_liquidity_ateco(failedyear = failedyear,activeyear = activeyear, ateco = most_frequent_ateco, sizebin=sizebin)
data_ateco_comparison <- append(data_ateco, list_gen)
#Bonferroni correction
chi_ateco_comparison <- chi_test_result_comparison(data_ateco_comparison, value= 0.05*(length(data_ateco_comparison)-1))

###################
#COMPARE LIQUIDITY#
###################
#########################
#(SPECIFIC COMPANY FORM)#
#########################
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
failedyear <- failedyear[which(is.na(failedyear$`Liquidity ratioLast avail. yr`) == FALSE),]
activeyear <- activeyear[which(is.na(activeyear$`Liquidity ratioLast avail. yr`) == FALSE),]
table(failedyear$`Legal form`)
most_frequent_company <- c("S.R.L.")
data_company <- prob_condition_liquidity_company(failedyear = failedyear,activeyear = activeyear, company = most_frequent_company, sizebin = sizebin)
data_company_comparison <- append(data_company, list_gen)
chi_company <- chi_test_result_comparison(data_company_comparison, value = 0.05)

##############
#COMPARE AGE #
##############
#########
#GENERAL#
#########
year <- 2016
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
age_failed_at_year <- select_feature(failedyear,"Age",year = year) #failed firm at year given feature
age_total_at_year <-  c(age_failed_at_year,select_feature(activeyear,"Age",year = year)) #total firm at 2016 with variable
age_failed_at_year <- age_failed_at_year[which(age_failed_at_year > 0)]
age_total_at_year <- age_total_at_year[which(age_total_at_year > 0)]
minbin <- min(failedyear$Age)
maxbin <- max(failedyear$Age)
sizebin <- 1 #we set age bin analyzing hist(variable$failed)
variable <- list(failed = (age_failed_at_year), total = (age_total_at_year))
id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
pB <- table(id2bin_total)/length(variable$total)
pAeB <- table(id2bin_failed)/length(variable$total)
prob_failed_give_age <- pAeB/pB
prob_failed_give_age[which(is.na(prob_failed_give_age) == TRUE)] = 0
prob_failed_give_age<- prob_failed_give_age/(sum(prob_failed_give_age))
plot(pAeB)
plot(pB)
plot(prob_failed_give_age,ylab = 'p(failed|age)',  xlab = '(Bin Age)', 
     main = 'Probability of failures conditional to age')
list_gen<- list(prob = prob_failed_give_age)
names(list_gen)[1]<- 'General'



#############
#COMPARE AGE#
#############
########
#REGION#
########
############################
#COMPARE FIRM BETWEEN N-C-S#
############################
nord <- c("Valle d'Aosta/Vallée d'Aoste", "Piemonte", "Lombardia", "Trentino-Alto Adige", "Liguria", 
          "Friuli-Venezia Giulia", "Veneto", "Emilia-Romagna")
centro <- c("Lazio", "Marche", "Toscana", "Umbria")
sud <- c("Abruzzo", "Basilicata", "Calabria", "Campania", "Molise", "Puglia")
isola <- c("Sicilia", "Sardegna")
data_nord <- prob_condition_age_region(failedyear = failedyear,activeyear = activeyear, region = nord,sizebin = sizebin)
data_centro <- prob_condition_age_region(failedyear = failedyear,activeyear = activeyear, region = centro, sizebin = sizebin)
data_sud <- prob_condition_age_region(failedyear = failedyear,activeyear = activeyear, region = sud, sizebin = sizebin)
data_isola <- prob_condition_age_region(failedyear = failedyear,activeyear = activeyear, region = isola, sizebin = sizebin)

Sidak_nord =1-(1-0.05)^(length(nord)-1)
Sidak_centro =1-(1-0.05)^(length(centro)-1)
Sidak_sud =1-(1-0.05)^(length(sud)-1)
Sidak_isola =1-(1-0.05)^(length(isola)-1)


chi_nord <- chi_test_result_comparison(data_nord, value = Sidak_nord)
chi_centro <- chi_test_result_comparison(data_centro, value = Sidak_centro)
chi_sud <- chi_test_result_comparison(data_sud, value = Sidak_sud)
chi_isola <- chi_test_result_comparison(data_isola, value = Sidak_isola)

#######################
#COMPARE FIRM IN N-C-S#
#######################
failedyear_nord <- filter(failedyear, failedyear$`Registered office address - Region` == nord)
levels(failedyear_nord$`Registered office address - Region`) <-  list('nord' = nord)

activeyear_nord <- filter(activeyear, activeyear$`Registered office address - Region` == nord)
levels(activeyear_nord$`Registered office address - Region`) <-  list('nord' = nord)


failedyear_centro <- filter(failedyear, failedyear$`Registered office address - Region` == centro)
levels(failedyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

activeyear_centro <- filter(activeyear, activeyear$`Registered office address - Region` == centro)
levels(activeyear_centro$`Registered office address - Region`) <-  list('centro' = centro)

failedyear_sud<- filter(failedyear, failedyear$`Registered office address - Region` == sud)
levels(failedyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

activeyear_sud <- filter(activeyear, activeyear$`Registered office address - Region` == sud)
levels(activeyear_sud$`Registered office address - Region`) <-  list('sud' = sud)

failedyear2 <- rbind(failedyear_nord, failedyear_centro, failedyear_sud)
activeyear2 <- rbind(activeyear_nord, activeyear_centro, activeyear_sud)
tot <- c('nord', 'centro', 'sud')
data_tot <- prob_condition_age_region(failedyear2, activeyear2, tot,sizebin = 1)
chi_tot <- chi_test_result_comparison(data_tot, value = 1-(1-0.05)^(length(tot)-1))



#############
#COMPARE AGE#
#############
#######
#ATECO#
#######
table(failedyear$`ATECO 2007code`)
most_frequent_ateco <- c("C", "F", "G")
data_ateco <- prob_condition_age_ateco(failedyear = failedyear,activeyear = activeyear, ateco = most_frequent_ateco,sizebin = sizebin)
data_ateco_comparison <- append(data_ateco, list_gen)
chi_tot <- chi_test_result_comparison(data_ateco_comparison, value= 0.05*(length(data_ateco_comparison)-1))


#############
#COMPARE AGE#
#############
#########################
#(SPECIFIC COMPANY FORM)#
#########################
table(failedyear$`Legal form`)
most_frequent_company <- c("S.R.L.")
data_company <- prob_condition_age_company(failedyear = failedyear,activeyear = activeyear, company = most_frequent_company, sizebin = sizebin)
data_company_comparison <- append(data_company, list_gen)
chi_tot <- chi_test_result_comparison(data_company_comparison, value = 0.05 )
