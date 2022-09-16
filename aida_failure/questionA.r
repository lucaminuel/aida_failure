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
library(fitdistrplus)
library(igraph)
library(poweRlaw)
source("useful_functions.R")
load('aida_failed_finale.RData')
load('aida_active_finale.RData')
##############
#QUESTION (A)#
##############
#Let's consider firm in year 2011
year  = 2011
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)
#############################
#COMPARE SIZE (TYPE COMPANY)#
#############################
#########
#GENERAL#
#########
size_active_at_year <- select_feature(activeyear,"Total assetsth EURLast avail. yr",year = year)
size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year = year)
size_active_at_year <- size_active_at_year[which(size_active_at_year > 0)]
size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
hist_active_year <- hist(log(size_active_at_year))
hist_failed_year<- hist(log(size_failed_at_year))
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Log Total Asset', 
     main = 'Distribution of size between failed and active Companies',
     xlim= (c(min(log(size_active_at_year),log(size_failed_at_year)),max(log(size_active_at_year),log(size_failed_at_year)))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))
plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)
#Cullen and Frey graph
descdist(log(size_active_at_year), boot=50) #from this test, active companies look like logistic distribution
descdist(log(size_failed_at_year), boot=50) #no information about failed
#ks.test to comapre the 2 distribution
ks.test(log(size_failed_at_year),log(size_active_at_year)) # p-value< 0.05 -> we have to reject 0 hypotesis: same distribution
ks.test(log(size_active_at_year), plogis, mean(log(size_active_at_year)), scale_plogis(log(size_active_at_year))) #p-value < 0.5 ->  have to reject 0 hypotesis: logistic distribution
#t-test
t.test(log(size_failed_at_year),log(size_active_at_year)) # p<0.05 -> we have to reject 0 hypotesis: same mean



######################################
#COMPARE SIZE (SPECIFIC COMPANY FORM)#
######################################
#Exclude S.R.L., S.R.L. one-person and S.R.L. simplified (most numerous one)
type_society = levels(activeyear$`Legal form`)
type_society = type_society[-c(15,16,17)]
size_active_at_year <- NULL
size_failed_at_year <- NULL
#if we have to merge more society
for( i in type_society){
  size_active_at_yeari <- select_feature(activeyear,"Total assetsth EURLast avail. yr",year,society = i)
  size_active_at_year <- c(size_active_at_year, size_active_at_yeari)
  size_failed_at_yeari <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year,society = i)
  size_failed_at_year <- c(size_failed_at_year, size_failed_at_yeari)
  rm(size_active_at_yeari)
  rm(size_failed_at_yeari)
}
rm(i)
size_active_at_year <- size_active_at_year[which(size_active_at_year > 0)]
size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
hist_active_year <- hist(log(size_active_at_year))
hist_failed_year<- hist(log(size_failed_at_year))
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Log Total Asset', 
     main = 'Distribution of size between failed and active Companies',
     xlim= (c(min(log(size_active_at_year),log(size_failed_at_year)),max(log(size_active_at_year),log(size_failed_at_year)))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))
plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)
#Cullen and Frey graph
descdist(log(size_active_at_year), boot=100)#from this test, no information about theoretical distribution
descdist(log(size_failed_at_year), boot=50)#from this test, no information about theoretical distribution
#KS Test
ks.test(log(size_active_at_year),log(size_failed_at_year)) # p-value< 0.05 -> we have to reject 0 hypotesis: same distribution
#t test
t.test(log(size_failed_at_year),log(size_active_at_year)) # p<0.05 -> we have to reject 0 hypotesis: same mean

###########################
#COMPARE SIZE (TYPE ATECO)#
###########################
type_ateco = "C"
size_active_at_year <- select_feature(activeyear,"Total assetsth EURLast avail. yr",year,ateco = type_ateco)
size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year,ateco = type_ateco)
size_active_at_year <- size_active_at_year[which(size_active_at_year > 0)]
hist_active_year <- hist(log(size_active_at_year))
hist_failed_year<- hist(log(size_failed_at_year))
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Log Total Asset', 
     main = 'Distribution of size between failed and active Companies',
     xlim= (c(min(log(size_active_at_year),log(size_failed_at_year)),max(log(size_active_at_year),log(size_failed_at_year)))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density)))
)
plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)
#Cullen and Frey graph
descdist(log(size_active_at_year), boot=500)#from this test, no information about theoretical distribution
descdist(log(size_failed_at_year), boot=500)#from this test,look like logistic
#KS Test
ks.test(log(size_failed_at_year), plogis, mean(log(size_failed_at_year)), scale_plogis(log(size_failed_at_year))) #p-value > 0.5 -> can not reject 0 hypotesis: logistic distribution

ks.test(log(size_failed_at_year),log(size_active_at_year))  #p-value< 0.05 -> we have to reject 0 hypotesis: same distribution
#t-test
t.test(log(size_failed_at_year),log(size_active_at_year)) # p<0.05 -> we have to reject 0 hypotesis: same mean



##############
#COMPARE  AGE#
##############
#########
#GENERAL#
#########
age_active_at_year <- select_feature(activeyear,"Age",year = year)
age_failed_at_year <- select_feature(failedyear,"Age",year = year)
# consider age >=0
age_active_at_year <- age_active_at_year[which(age_active_at_year > 0)]
age_failed_at_year <- age_failed_at_year[which(age_failed_at_year > 0)]

hist_active_year <- hist(age_active_at_year)
hist_failed_year<- hist(age_failed_at_year)
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Age', 
     main = 'Distribution of age between failed and active Companies',
     xlim= (c(min(age_active_at_year,age_failed_at_year),max(age_active_at_year,age_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))
plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)


# chi-test 
power_law_chi_test(age_active_at_year) #p >0.05 -> can not reject 0 hypotesis: discrete-power-law
power_law_chi_test(age_failed_at_year) #p < 0.05 -> have to reject 0 hypotesis: discrete-power-law
chi_test_function(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution
t.test(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-mean


####################################
#COMPARE AGE(SPECIFIC COMPANY FORM)#
####################################
type_society = levels(activeyear$`Legal form`)
type_society = type_society[-c(15,16,17)]

age_active_at_year <- NULL
age_failed_at_year <- NULL
#if we have to merge more society
for( i in type_society){
  age_active_at_yeari <- select_feature(activeyear,"Age",year,society = i)
  age_active_at_year <- c(age_active_at_year, age_active_at_yeari)
  age_failed_at_yeari <- select_feature(failedyear,"Age",year,society = i)
  age_failed_at_year <- c(age_failed_at_year, age_failed_at_yeari)
  rm(age_active_at_yeari)
  rm(age_failed_at_yeari)
}

rm(i)
age_active_at_year <- age_active_at_year[which(age_active_at_year > 0)]
age_failed_at_year <- age_failed_at_year[which(age_failed_at_year >  0)]
hist_active_year <- hist(age_active_at_year)
hist_failed_year<- hist(age_failed_at_year)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Age', 
     main = 'Distribution of age between failed and active Companies',
     xlim= (c(min(age_active_at_year,age_failed_at_year),max(age_active_at_year,age_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))

plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)
# chi-test
power_law_chi_test(age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: power-law
power_law_chi_test(age_failed_at_year) #p < 0.05 -> have to  reject 0 hypotesis: power-law
chi_test_function(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution
t.test(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-mean

##########################
#COMPARE AGE (TYPE ATECO)#
##########################
type_ateco = "C"
age_active_at_year <- select_feature(activeyear,"Age",year,ateco = type_ateco)
age_failed_at_year <- select_feature(failedyear,"Age",year,ateco = type_ateco)
age_active_at_year <- age_active_at_year[which(age_active_at_year > 0)]
age_failed_at_year <- age_failed_at_year[which(age_failed_at_year > 0)]


hist_active_year <- hist(age_active_at_year)
hist_failed_year<- hist(age_failed_at_year)
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)

plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'Age', 
     main = 'Distribution of age between failed and active Companies, ATECO C',
     xlim= (c(min(age_active_at_year,age_failed_at_year),max(age_active_at_year,age_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density)))
)

plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)
# chi-test
power_law_chi_test(age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: power-law
power_law_chi_test(age_failed_at_year) #p < 0.05 -> have to  reject 0 hypotesis: power-law
chi_test_function(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution
t.test(age_failed_at_year,age_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-mean


####################
#COMPARE  LIQUIDITY#
####################
#########
#GENERAL#
#########
failedyear <- function_select_year(aida_failed_finale, year)
activeyear <- function_select_year(aida_active_finale,year)

liquidity_active_at_year <- select_feature(activeyear,"Liquidity ratioLast avail. yr",year = year)
liquidity_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year = year)
liquidity_active_at_year <- na.omit(liquidity_active_at_year)
liquidity_failed_at_year <- na.omit(liquidity_failed_at_year)


hist_active_year <- hist(liquidity_active_at_year, breaks = 50)
hist_failed_year<- hist(liquidity_failed_at_year, breaks = 50)
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'liquidity', 
     main = 'Distribution of liquidity between failed and active Companies',
     xlim= (c(min(liquidity_active_at_year,liquidity_failed_at_year),max(liquidity_active_at_year,liquidity_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))
plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)


#Cullen and Frey graph
descdist(liquidity_active_at_year, boot=50) #from this test, active companies look like logistic distribution
descdist(liquidity_failed_at_year, boot=50) #no information about failed

#beta
ks.test(liquidity_active_at_year, pbeta, alpha(liquidity_active_at_year), beta(liquidity_active_at_year))

# Are the two distributions equal? 
ks.test(liquidity_failed_at_year,liquidity_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution

t.test(liquidity_active_at_year, liquidity_failed_at_year)#p < 0.05 -> have to reject 0 hypotesis: same mean



###########################################
#COMPARE LIQUIDITY (SPECIFIC COMPANY FORM)#
###########################################
type_society = levels(activeyear$`Legal form`)
type_society = type_society[-c(15,16,17)]

liquidity_active_at_year <- NULL
liquidity_failed_at_year <- NULL
#if we have to merge more society
for( i in type_society){
  liquidity_active_at_yeari <- select_feature(activeyear,"Liquidity ratioLast avail. yr",year,society = i)
  liquidity_active_at_year <- c(liquidity_active_at_year, liquidity_active_at_yeari)
  liquidity_failed_at_yeari <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year,society = i)
  liquidity_failed_at_year <- c(liquidity_failed_at_year, liquidity_failed_at_yeari)
  rm(liquidity_active_at_yeari)
  rm(liquidity_failed_at_yeari)
}
rm(i)
hist_active_year <- hist(liquidity_active_at_year, breaks = 50)
hist_failed_year<- hist(liquidity_failed_at_year, breaks = 50)
plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'liquidity', 
     main = 'Distribution of liquidity between failed and active Companies',
     xlim= (c(min(liquidity_active_at_year,liquidity_failed_at_year),max(liquidity_active_at_year,liquidity_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density))))

plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)


#Cullen and Frey graph
descdist(liquidity_active_at_year, boot=50) #from this test, active companies look like logistic distribution
descdist(liquidity_failed_at_year, boot=100) #no information about failed


#beta
ks.test(liquidity_active_at_year, pbeta, alpha(liquidity_active_at_year), beta(liquidity_active_at_year))
ks.test(liquidity_failed_at_year,liquidity_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution
t.test(liquidity_active_at_year, liquidity_failed_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-mean

################################
#COMPARE LIQUIDITY (TYPE ATECO)#
################################
type_ateco = "C"
liquidity_active_at_year <- select_feature(activeyear,"Liquidity ratioLast avail. yr",year,ateco = type_ateco)
liquidity_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year,ateco = type_ateco)


hist_active_year <- hist(liquidity_active_at_year, breaks = 50)
hist_failed_year<- hist(liquidity_failed_at_year, breaks = 50)
hist_active_year$density = hist_active_year$counts/sum(hist_active_year$counts)
hist_failed_year$density = hist_failed_year$counts/sum(hist_failed_year$counts)

plot(hist_failed_year,freq = FALSE,  col=rgb(0,0,1,0.5),ylab = 'Density',  xlab = 'liquidity', 
     main = 'Distribution of liquidity between failed and active Companies, ATECO C',
     xlim= (c(min(liquidity_active_at_year,liquidity_failed_at_year),max(liquidity_active_at_year,liquidity_failed_at_year))),
     ylim = (c(min(hist_active_year$density,hist_failed_year$density), max(hist_active_year$density,hist_failed_year$density)))
)

plot(hist_active_year, freq= FALSE, add = TRUE, col=rgb(1,0,0,0.5))
legend("topright", legend=c("Active Companies","Failed Companies"), col=c(rgb(1,0,0,0.5), 
                                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15)

#Cullen and Frey graph
descdist(liquidity_active_at_year, boot=50) #from this test, active companies look like logistic distribution
descdist(liquidity_failed_at_year, boot=50) #no information about failed

#beta
ks.test(liquidity_active_at_year, pbeta, alpha(liquidity_active_at_year), beta(liquidity_active_at_year))
ks.test(liquidity_failed_at_year,liquidity_active_at_year) #p < 0.05 -> have to reject 0 hypotesis: same-distribution
t.test(liquidity_active_at_year, liquidity_failed_at_year) #p < 0.05 -> have to reject 0 hypotesis: same mean
