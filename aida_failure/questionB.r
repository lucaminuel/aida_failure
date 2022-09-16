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
source("useful_functions.R")
load(file = "aida_failed_finale.RData")



##############
#QUESTION (B)#
##############
#Let's consider firm in range year 2006-2010
start_year <- 2006
len <- 5
failedyear <- vector(mode = "list", length = len)
for (i in (1:len)){
  failedyear[[i]] <- function_select_year(aida_failed_finale, year = start_year + (i-1))
}
#############################
#COMPARE SIZE (TYPE COMPANY)#
#############################
#########
#GENERAL#
#########
size_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:len)){
  size_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Total assetsth EURLast avail. yr",
                                          year = start_year + (i-1))
  size_failed_year[[i]] <- na.omit(size_failed_year[[i]])
  size_failed_year[[i]] <- size_failed_year[[i]][which(size_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density(log(size_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Log Total Asset', 
         main = 'Distribution of size between failed company at different years', ylim = c(0, 0.6))
  }
  else {
    lines(density(log(size_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)
#Qualitative Test
#Cullen and Frey graph
descdist(log(size_failed_year[[1]]), boot=50)#from this test, active companies look like logistic
descdist(log(size_failed_year[[2]]), boot=50)
descdist(log(size_failed_year[[3]]), boot=50)
descdist(log(size_failed_year[[4]]), boot=50)
descdist(log(size_failed_year[[5]]), boot=50)
#KS Test
ks.test(log(size_failed_year[[1]]), plogis, mean(log(size_failed_year[[1]])),
        scale_plogis(log(size_failed_year[[1]]))) #p-value > 0.05 ->  can not reject 0 hypotesis: logistic distribution

ks_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- ks.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution
#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean


######################################
#COMPARE SIZE (SPECIFIC COMPANY FORM)#
######################################
type_society = 'S.R.L.'
size_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:len)){
  size_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Total assetsth EURLast avail. yr",
                                          year = start_year + (i-1) , society = type_society)
  size_failed_year[[i]] <- na.omit(size_failed_year[[i]])
  size_failed_year[[i]] <- size_failed_year[[i]][which(size_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density(log(size_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Log Total Asset', 
         main = 'Distribution of size between failed company at different years', ylim = c(0, 0.6))
  }
  else {
    lines(density(log(size_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)
#Qualitative Test
#Cullen and Frey graph
descdist(log(size_failed_year[[1]]), boot=500)#from this test, active companies look like logistic
descdist(log(size_failed_year[[2]]), boot=500)
descdist(log(size_failed_year[[3]]), boot=500)#from this test, active companies look like logistic
descdist(log(size_failed_year[[4]]), boot=500)
descdist(log(size_failed_year[[5]]), boot=500)
#KS Test
ks.test(log(size_failed_year[[1]]), plogis, mean(log(size_failed_year[[1]])),
        scale_plogis(log(size_failed_year[[1]])))#p-value > 0.5 ->  can not reject 0 hypotesis: logistic distribution
ks.test(log(size_failed_year[[3]]), plogis, mean(log(size_failed_year[[3]])),
        scale_plogis(log(size_failed_year[[3]])))#p-value > 0.5 ->  can not reject 0 hypotesis: logistic distribution
ks_result<- list()
for (i in (1:len)){
  for (j in (1:len) ){
    if (i < j) {
      a <- ks.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution
#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean


#######################
#COMPARE SIZE (REGION)#
#######################
region = "Lombardia"
size_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:5)){
  size_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Total assetsth EURLast avail. yr",
                                          year = start_year + (i-1) , region = region)
  size_failed_year[[i]] <- na.omit(size_failed_year[[i]])
  size_failed_year[[i]] <- size_failed_year[[i]][which(size_failed_year[[i]] > 0)]
  #hist_failed_year[[i]]<- hist(log(size_failed_year[[i]]))
  #hist_failed_year[[i]]$density = hist_failed_year[[i]]$counts/sum(hist_failed_year[[i]]$counts)
  if (i ==1){
    plot(density(log(size_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Log Total Asset', 
         main = 'Distribution of size between failed company at different years', ylim = c(0, 0.6))
  }
  else {
    lines(density(log(size_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)
#Qualitative Test
#Cullen and Frey graph
descdist(log(size_failed_year[[1]]), boot=500) #from this test, companies look like logistic
descdist(log(size_failed_year[[2]]), boot=500) #from this test, companies look like logistic
descdist(log(size_failed_year[[3]]), boot=500) #from this test, companies look like logistic
descdist(log(size_failed_year[[4]]), boot=500) #from this test, companies look like logistic
descdist(log(size_failed_year[[5]]), boot=500) #from this test, companies look like logistic
#KS Test
ks.test(log(size_failed_year[[1]]), plogis, mean(log(size_failed_year[[1]])),
        scale_plogis(log(size_failed_year[[1]]))) #p-value > 0.05 ->  can not reject 0 hypotesis: logistic distribution
ks.test(log(size_failed_year[[2]]), plogis, mean(log(size_failed_year[[2]])),
        scale_plogis(log(size_failed_year[[2]]))) #p-value > 0.05 ->  can not reject 0 hypotesis: logistic distribution
ks.test(log(size_failed_year[[3]]), plogis, mean(log(size_failed_year[[3]])),
        scale_plogis(log(size_failed_year[[3]]))) #p-value > 0.05 ->  can not reject 0 hypotesis: logistic distribution
ks.test(log(size_failed_year[[4]]), plogis, mean(log(size_failed_year[[4]])),
        scale_plogis(log(size_failed_year[[4]]))) #p-value > 0.05 ->  we can not reject 0 hypotesis: logistic distribution
ks.test(log(size_failed_year[[5]]), plogis, mean(log(size_failed_year[[5]])),
        scale_plogis(log(size_failed_year[[5]]))) #p-value > 0.05 ->  we can not reject 0 hypotesis: logistic distribution

ks_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- ks.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}

t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(log(size_failed_year[[i]]),log(size_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean

#############
#COMPARE AGE#
#############
#########
#GENERAL#
#########
age_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:5)){
  age_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Age",
                                          year = start_year + (i-1))
  age_failed_year[[i]] <- na.omit(age_failed_year[[i]])
  age_failed_year[[i]] <- age_failed_year[[i]][which(age_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density(age_failed_year[[i]]),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Age', 
         main = 'Distribution of age between failed company at different years', ylim = c(0, 0.1))
  }
  else {
    lines(density(age_failed_year[[i]]),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)

#Chi Test
power_law_test<- list()
for (i in (1:5)){
      a <- power_law_chi_test(age_failed_year[[i]])
      if (a$p.value > 0.05) {
        a$data.name = glue('anno{start_year + (i-1)}')
        power_law_test <- append(power_law_test, a)
      }
    }#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution

chi_test_results<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- chi_test_function(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        chi_test_results <- append(chi_test_results, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution
#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean

#####################################
#COMPARE AGE (SPECIFIC COMPANY FORM)#
#####################################
type_society = 'S.R.L.' #take only S.R.L. some other type_society do not exit in 2006-2010
age_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:5)){
  age_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Age",
                                         year = start_year + (i-1), society = type_society)
  age_failed_year[[i]] <- na.omit(age_failed_year[[i]])
  age_failed_year[[i]] <- age_failed_year[[i]][which(age_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density(age_failed_year[[i]]),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Age', 
         main = 'Distribution of age between failed company at different years', ylim = c(0, 0.1))
  }
  else {
    lines(density(age_failed_year[[i]]),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)

#Chi Test
power_law_test<- list()
for (i in (1:5)){
  a <- power_law_chi_test(age_failed_year[[i]])
  if (a$p.value > 0.05) {
    a$data.name = glue('anno{start_year + (i-1)}')
    power_law_test <- append(power_law_test, a)
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution

chi_test_results<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- chi_test_function(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        chi_test_results <- append(chi_test_results, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution
#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean


#####################
#COMPARE AGE(REGION)#
#####################
region = "Lombardia"
age_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:5)){
  age_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Age",
                                         year = start_year + (i-1), region = region)
  age_failed_year[[i]] <- na.omit(age_failed_year[[i]])
  age_failed_year[[i]] <- age_failed_year[[i]][which(age_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density(age_failed_year[[i]]),lwd = 4, col = color[i],ylab = 'Density',  xlab = 'Age', 
         main = 'Distribution of age between failed company at different years', ylim = c(0, 0.1))
  }
  else {
    lines(density(age_failed_year[[i]]),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)

#Chi Test
power_law_test<- list()
for (i in (1:5)){
  a <- power_law_chi_test(age_failed_year[[i]])
  if (a$p.value > 0.05) {
    a$data.name = glue('anno{start_year + (i-1)}')
    power_law_test <- append(power_law_test, a)
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution

chi_test_results<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- chi_test_function(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        chi_test_results <- append(chi_test_results, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution
#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test(age_failed_year[[i]],age_failed_year[[j]])
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean



##################################
#COMPARE LIQUIDITY (TYPE COMPANY)#
##################################
#########
#GENERAL#
#########
liquidity_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')
for (i in (1:len)){
  liquidity_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Liquidity ratioLast avail. yr",
                                               year = start_year + (i-1))
  liquidity_failed_year[[i]] <- na.omit(liquidity_failed_year[[i]])
  liquidity_failed_year[[i]] <- liquidity_failed_year[[i]][which(liquidity_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = ' Liquidity ratio', 
         main = 'Distribution of liquidity between failed company at different years', ylim = c(0, 1))
  }
  else {
    lines(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)



#Qualitative Test
#Cullen and Frey graph
descdist((liquidity_failed_year[[1]]), boot=50)#from this test, active companies look like gamma
descdist((liquidity_failed_year[[2]]), boot=50)# gamma
descdist((liquidity_failed_year[[3]]), boot=50)# gamma
descdist((liquidity_failed_year[[4]]), boot=50)# gamma
descdist((liquidity_failed_year[[5]]), boot=50)# gamma 



#KS Test ( we can reject)
ks.test(liquidity_failed_year[[1]], pgamma, shape = shape_pgamma(liquidity_failed_year[[1]]), 
        scale =scale_pgamma(liquidity_failed_year[[1]]))

ks.test(liquidity_failed_year[[2]], pgamma, shape =shape_pgamma(liquidity_failed_year[[2]]), 
        scale =scale_pgamma(liquidity_failed_year[[2]]))

ks.test(liquidity_failed_year[[3]], pgamma,shape = shape_pgamma(liquidity_failed_year[[3]]), 
        scale = scale_pgamma(liquidity_failed_year[[3]]))

ks.test(liquidity_failed_year[[4]], pgamma, shape =shape_pgamma(liquidity_failed_year[[4]]), 
        scale = scale_pgamma(liquidity_failed_year[[4]]))

ks.test(liquidity_failed_year[[5]], pgamma, shape =shape_pgamma(liquidity_failed_year[[5]]), 
        scale = scale_pgamma(liquidity_failed_year[[5]]))

ks_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- ks.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution    2009-2010



#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}
#p-value > 0.05 -> we can not reject 0 hypotesis: same mean 



###########################################
#COMPARE LIQUIDITY (SPECIFIC COMPANY FORM)#
###########################################
type_society = 'S.R.L.'
liquidity_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')


for (i in (1:len)){
  liquidity_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Liquidity ratioLast avail. yr",
                                               year = start_year + (i-1) , society = type_society)
  liquidity_failed_year[[i]] <- na.omit(liquidity_failed_year[[i]])
  liquidity_failed_year[[i]] <- liquidity_failed_year[[i]][which(liquidity_failed_year[[i]] > 0)]
  if (i ==1){
    plot(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = ' Total Asset', 
         main = 'Distribution of liquidity between failed company at different years', ylim = c(0, 1))
  }
  else {
    lines(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)



#Qualitative Test
#Cullen and Frey graph
descdist((liquidity_failed_year[[1]]), boot=500)#from this test,looks like gamma
descdist((liquidity_failed_year[[2]]), boot=500)#from this test,looks like gamma
descdist((liquidity_failed_year[[3]]), boot=500)#from this test,looks like gamma
descdist((liquidity_failed_year[[4]]), boot=500)#from this test,looks like gamma
descdist((liquidity_failed_year[[5]]), boot=500)#from this test,looks like gamma

#KS Test
#KS Test ( we can reject)
ks.test(liquidity_failed_year[[1]], pgamma, shape = shape_pgamma(liquidity_failed_year[[1]]), 
        scale = scale_pgamma(liquidity_failed_year[[1]]))

ks.test(liquidity_failed_year[[2]], pgamma, shape = shape_pgamma(liquidity_failed_year[[2]]), 
        scale = scale_pgamma(liquidity_failed_year[[2]])) 

ks.test(liquidity_failed_year[[3]], pgamma, shape = shape_pgamma(liquidity_failed_year[[3]]), 
        scale = scale_pgamma(liquidity_failed_year[[3]]))

ks.test(liquidity_failed_year[[4]], pgamma, shape = shape_pgamma(liquidity_failed_year[[4]]), 
        scale = scale_pgamma(liquidity_failed_year[[4]]))

ks.test(liquidity_failed_year[[5]], pgamma, shape = shape_pgamma(liquidity_failed_year[[5]]), 
        scale = scale_pgamma(liquidity_failed_year[[5]]))



ks_result<- list()
for (i in (1:len)){
  for (j in (1:len) ){
    if (i < j) {
      a <- ks.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}
#p-value > 0.05 -> we can not reject 0 hypotesis: same distribution   06-07   06-08  09-10



#t-test
t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean




############################
#COMPARE LIQUIDITY (REGION)#
############################
region = "Lombardia"
liquidity_failed_year <- vector(mode = "list", length = len)
hist_failed_year <- vector(mode = "list", length = len)
color <- c('blue', 'green', 'red', 'black', 'deeppink')


for (i in (1:5)){
  liquidity_failed_year[[i]] <- select_feature(failedyear[[i]], name_feature = "Liquidity ratioLast avail. yr",
                                               year = start_year + (i-1) , region = region)
  liquidity_failed_year[[i]] <- na.omit(liquidity_failed_year[[i]])
  liquidity_failed_year[[i]] <- liquidity_failed_year[[i]][which(liquidity_failed_year[[i]] > 0)]
  #hist_failed_year[[i]]<- hist((liquidity_failed_year[[i]]))
  #hist_failed_year[[i]]$density = hist_failed_year[[i]]$counts/sum(hist_failed_year[[i]]$counts)
  if (i ==1){
    plot(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i],ylab = 'Density',  xlab = ' Total Asset', 
         main = 'Distribution of liquidity between failed company at different years', ylim = c(0, 1))
  }
  else {
    lines(density((liquidity_failed_year[[i]])),lwd = 4, col = color[i])
  }
  
}
legend("topright", legend=c(as.character(start_year),as.character(start_year +1),as.character(start_year+2),
                            as.character(start_year+3),as.character(start_year+4)),
       col= c(color[1],color[2],color[3],color[4],color[5]), pt.cex=2, pch=15)
#Qualitative Test
#Cullen and Frey graph
descdist((liquidity_failed_year[[1]]), boot=500) #from this test, companies look like gamma
descdist((liquidity_failed_year[[2]]), boot=500) #from this test, companies look like gamma/beta
descdist((liquidity_failed_year[[3]]), boot=500) #from this test, companies look like gamma
descdist((liquidity_failed_year[[4]]), boot=500) #from this test, companies look like gamma
descdist((liquidity_failed_year[[5]]), boot=500) #from this test, companies look like gamma


#KS Test
#KS Test ( we can reject)
ks.test(liquidity_failed_year[[1]], pgamma, shape = shape_pgamma(liquidity_failed_year[[1]]), 
        scale = scale_pgamma(liquidity_failed_year[[1]]))

ks.test(liquidity_failed_year[[2]], pgamma, shape = shape_pgamma(liquidity_failed_year[[2]]), 
        scale = scale_pgamma(liquidity_failed_year[[2]]))

ks.test(liquidity_failed_year[[2]], pbeta, alpha(liquidity_failed_year[[2]]), 
        beta(liquidity_failed_year[[2]]))

ks.test(liquidity_failed_year[[3]], pgamma, shape = shape_pgamma(liquidity_failed_year[[3]]), 
        scale = scale_pgamma(liquidity_failed_year[[3]]))

ks.test(liquidity_failed_year[[4]], pgamma, shape = shape_pgamma(liquidity_failed_year[[4]]), 
        scale = scale_pgamma(liquidity_failed_year[[4]]))

ks.test(liquidity_failed_year[[5]], pgamma, shape = shape_pgamma(liquidity_failed_year[[5]]), 
        scale = scale_pgamma(liquidity_failed_year[[5]]))




ks_result<- list()  # we can not reject  06-07   07-08  09-10
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- ks.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        ks_result <- append(ks_result, a)
      }
    }
  }
}



t_result<- list()
for (i in (1:5)){
  for (j in (1:5) ){
    if (i < j) {
      a <- t.test((liquidity_failed_year[[i]]),(liquidity_failed_year[[j]]))
      if (a$p.value > 1-(1-0.05)^(len-1)) {
        a$data.name = glue('anno{start_year + (i-1)} - anno{start_year + (j-1)}')
        t_result <- append(t_result, a)
      }
    }
  }
}#p-value > 0.05 -> we can not reject 0 hypotesis: same mean
