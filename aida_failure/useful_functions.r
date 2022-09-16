######
#Useful function
######
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


#This function take aida dataset, extract the first 2 digit of ATECO 2007 code for each firm
#and sunstitute them to ATECO CODE letter (see: https://www.fondoforte.it/wp-content/uploads/2018/10/CODICI-ATECO-RIASSUNTIVA-DI-TUTTI-I-COMPARTI.pdf)
function_sub_ateco <- function (list_ateco){
  list <- substr(list_ateco,1,2)
  if (is.na(list) == TRUE) return (list)
  if (list >='00' && list <'05' ) {
    list = 'A'
  }
  if (list >='05' && list <'10' ) {
    list = 'B'
  }
  if (list >='10' && list <'35' ) {
    list = 'C'
  }
  if (list >='35' && list <'36' ) {
    list = 'D'
  }
  if (list >='36' && list <'41' ) {
    list = 'E'
  }
  if (list >='41' && list <'45' ) {
    list = 'F'
  }
  if (list >='45' && list <'49' ) {
    list = 'G'
  }
  if (list >='49' && list <'55' ) {
    list = 'H'
  }
  if (list >='55' && list <'58' ) {
    list = 'I'
  }
  if (list >='58' && list <'64' ) {
    list = 'J'
  }
  if (list >='64' && list <'68' ) {
    list = 'K'
  }
  if (list >='68' && list <'69' ) {
    list = 'L'
  }
  if (list >='69' && list <'77' ) {
    list = 'M'
  }
  if (list >='77' && list <'84' ) {
    list = 'N'
  }
  if (list >='84' && list <'85' ) {
    list = 'O'
  }
  if (list >='85' && list <'86' ) {
    list = 'P'
  }
  if (list >='86' && list <'90' ) {
    list = 'Q'
  }
  if (list >='90' && list <'94' ) {
    list = 'R'
  }
  if (list >='94' && list <'97' ) {
    list = 'S'
  }
  if (list >='97' && list <'99' ) {
    list = 'T'
  } 
  if (list == '99'){
    list = 'U'
  }
  return (list)
}


#In this function, we choose the year and from original dataset we select only the firm with
#'Last accounting closing date' (LACD) == year
function_select_year <- function(data , year){
  data_current_year <- filter(data, data$'Last accounting closing date' == year)
  return (data_current_year)
  
}


#In this function we select a feature for a given year.
select_feature <- function(data, name_feature, year, society = 0, ateco = 0, region = 0){
  if (society == 0 & ateco == 0 & region == 0){
    data_at_year <- filter (data, data$"Last accounting closing date"==year)
    index <- grep(name_feature,colnames(data))
    data_at_year <- data_at_year[,index]
    data_at_year<- unlist(data_at_year) 
    data_at_year <- c(na.omit(data_at_year))
  }
  else if (society != 0) {
    data_at_year <- filter (data, data$"Last accounting closing date"==year & data$"Legal form" == society)
    index <- grep(name_feature,colnames(data))
    data_at_year <- data_at_year[,index]
    data_at_year<- unlist(data_at_year) 
    data_at_year <- c(na.omit(data_at_year))
    
  }
  else  if (ateco != 0){
    data_at_year <- filter (data, data$"Last accounting closing date"==year & data$"ATECO 2007code" == ateco)
    index <- grep(name_feature,colnames(data))
    data_at_year <- data_at_year[,index]
    data_at_year<- unlist(data_at_year) 
    data_at_year <- c(na.omit(data_at_year))
  }
  else if (region != 0){
    data_at_year <- filter (data, data$"Last accounting closing date"==year & data$"Registered office address - Region" == region)
    index <- grep(name_feature,colnames(data))
    data_at_year <- data_at_year[,index]
    data_at_year<- unlist(data_at_year) 
    data_at_year <- c(na.omit(data_at_year))
    
  }
  return (data_at_year)
  
}


#In this function we define chi test function 
chi_test_function <- function(data1, data2){
  bins = sort(unique(c(data1, data2)))
  N1i = sapply(bins, function(i) sum(data1==i))
  N2i = sapply(bins, function(i) sum(data2==i))
  mat = cbind(N1i, N2i)
  return (chisq.test(mat, correct=FALSE))
}


#In this function we evaulate the scale of logistic distrubution given the variance
scale_plogis <- function(data){
  scale <- sqrt(3*var(data))/pi
  return (scale)
}


#This function evaluate scale and shape of gamma distribution given variance and mean 
scale_pgamma <- function (data){
  scale = var(data)/mean(data)
  return (scale)
}
shape_pgamma <- function(data){
  shape = mean(data)^2/(var(data))
  return (shape)
}


#This function evaluate meanlog and sdlog of log normal distribution given variance and mean 
mean_plnorm <- function(data){
  mean <- log(mean(data)^2/(sqrt(var(data)+mean(data)^2)))
  return (mean)
}
sd_plnorm <- function(data){
  sd <- sqrt(log(var(data)/mean(data)^2+1))
  return (sd)
}
#This function evaluate meanbeta and sdbeta of beta distribution given variance and mean

alpha <- function(data){
  alpha <- (((mean(data)*(1-mean(data)))/var(data))-1)*mean(data)
  return (alpha)
}
beta <- function(data){
  beta <- (((mean(data)*(1-mean(data)))/var(data))-1)*(1-mean(data))
  return (beta)
}


#This fuction returns how many data are lcoated in each bin
bin_assignment <- function(data, sizebin,min,max){
  nbin <- (max -min)/sizebin
  bins = seq(from = min +sizebin, to=max, by=sizebin)
  bins
  id2bin = cut(data, breaks=c(0,bins), labels=1:nbin)
  return (id2bin)
}


#This function delete from dataset features having vif > 0.5
vf_evaluate <- function(x, data){
  tic("Eliminate vif")
  # A first run of vif
  fit = glm(x~ ., data=data, family=binomial("logit"))
  vf <- vif(fit) 
  index = 0
  #This cycle delete features which estimators have vf higher than 5 
  while (index < length(data) && max(vf) > 5){
    fit = glm(x~ ., data=data, family=binomial("logit"))
    vf <- vif(fit)
    data[which(vf == max(vf))] = NULL
    writeLines(paste("Eliminata colonna:", names(which(vf == max(vf))) ))
    index = index +1
  }
  toc()
  return (data)
}

# This function fit data with "power-law" and return chitest
power_law_chi_test<- function(data){
  # loglog-plot: it could be a discrete power law
  n = length(data)
  epmf = table(data)/n #empirical prob mass func)
  plot(names(epmf), log(epmf), xlab = 'Age') 
  # suppose  using as model y = c+ a*x , y = log(age_active(false)_at_year), x = age
  y <- log(epmf)
  data_for_fit <- data.frame(y)
  active_power_fit <- lm(data_for_fit$Freq~data_for_fit$data)
  #chi-test
  data_fit <- predict(active_power_fit,data_for_fit$data)
  chi <- chi_test_function(data_for_fit$Freq, data_fit) 
  return (chi)
  }



#These functions give conditional probability for each feature selected
prob_condition_size_region <- function(failedyear = failedyear,activeyear = activeyear, region, sizebin = 0.05){
  mylist <- list()
  for (region in region){
    size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year = year, region = region)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Total assetsth EURLast avail. yr",year = year, region = region)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(log(failedyear$`Total assetsth EURLast avail. yr`))
    maxbin <- max(log(failedyear$`Total assetsth EURLast avail. yr`))
    variable <- list(failed = log(size_failed_at_year), total = log(size_total_at_year))
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- region
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", region))
  }
  return (mylist)
}

prob_condition_size_ateco <- function(failedyear = failedyear,activeyear = activeyear, ateco, sizebin = 0.05){
  mylist <- list()
  for (ateco in ateco){
    size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year = year, ateco = ateco)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Total assetsth EURLast avail. yr",year = year, ateco = ateco)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(log(failedyear$`Total assetsth EURLast avail. yr`))
    maxbin <- max(log(failedyear$`Total assetsth EURLast avail. yr`))
    variable <- list(failed = log(size_failed_at_year), total = log(size_total_at_year))
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- ateco
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", ateco))
  }
  return (mylist)
}
prob_condition_size_company <- function(failedyear = failedyear,activeyear = activeyear, company, sizebin = 0.05){
  mylist <- list()
  for (company in company){
    size_failed_at_year <- select_feature(failedyear,"Total assetsth EURLast avail. yr",year = year, society = company)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Total assetsth EURLast avail. yr",year = year, society = company)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(log(failedyear$`Total assetsth EURLast avail. yr`))
    maxbin <- max(log(failedyear$`Total assetsth EURLast avail. yr`))
    variable <- list(failed = log(size_failed_at_year), total = log(size_total_at_year))
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- company
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", company))
  }
  return (mylist)
}

prob_condition_liquidity_region <- function(failedyear = failedyear,activeyear = activeyear, region, sizebin = 0.05){
  mylist <- list()
  for (region in region){
    size_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year = year, region = region)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Liquidity ratioLast avail. yr",year = year, region = region)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$`Liquidity ratioLast avail. yr`)
    maxbin <- max(failedyear$`Liquidity ratioLast avail. yr`)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- region
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", region))
  }
  return (mylist)
}

prob_condition_liquidity_ateco <- function(failedyear = failedyear,activeyear = activeyear, ateco, sizebin = 0.05){
  mylist <- list()
  for (ateco in ateco){
    size_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year = year, ateco = ateco)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Liquidity ratioLast avail. yr",year = year, ateco = ateco)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$`Liquidity ratioLast avail. yr`)
    maxbin <- max(failedyear$`Liquidity ratioLast avail. yr`)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- ateco
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", ateco))
  }
  return (mylist)
}


prob_condition_liquidity_company <- function(failedyear = failedyear,activeyear = activeyear, company, sizebin = 0.05){
  mylist <- list()
  for (company in company){
    size_failed_at_year <- select_feature(failedyear,"Liquidity ratioLast avail. yr",year = year, society = company)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Liquidity ratioLast avail. yr",year = year, society = company)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$`Liquidity ratioLast avail. yr`)
    maxbin <- max(failedyear$`Liquidity ratioLast avail. yr`)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- company
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", company))
  }
  return (mylist)
}


prob_condition_age_region <- function(failedyear = failedyear,activeyear = activeyear, region, sizebin = 0.05){
  mylist <- list()
  for (region in region){
    size_failed_at_year <- select_feature(failedyear,"Age",year = year, region = region)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Age",year = year, region = region)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$Age)
    maxbin <- max(failedyear$Age)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- region
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", region))
  }
  return (mylist)
}


prob_condition_age_ateco <- function(failedyear = failedyear,activeyear = activeyear, ateco, sizebin = 0.05){
  mylist <- list()
  for (ateco in ateco){
    size_failed_at_year <- select_feature(failedyear,"Age",year = year, ateco = ateco)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Age",year = year, ateco = ateco)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$Age)
    maxbin <- max(failedyear$Age)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- ateco
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", ateco))
  }
  return (mylist)
}

prob_condition_age_company <- function(failedyear = failedyear,activeyear = activeyear, company, sizebin = 0.05){
  mylist <- list()
  for (company in company){
    size_failed_at_year <- select_feature(failedyear,"Age",year = year, society = company)
    size_total_at_year <-  c(size_failed_at_year,select_feature(activeyear,"Age",year = year, society = company)) 
    size_failed_at_year <- size_failed_at_year[which(size_failed_at_year > 0)]
    sizebin <- sizebin
    size_total_at_year <- size_total_at_year[which(size_total_at_year > 0)]
    minbin <- min(failedyear$Age)
    maxbin <- max(failedyear$Age)
    variable <- list(failed = size_failed_at_year, total = size_total_at_year)
    id2bin_failed <- bin_assignment(variable$failed,sizebin,max = maxbin, min = minbin)
    id2bin_total <- bin_assignment(variable$total,sizebin,max = maxbin, min = minbin)
    pB <- table(id2bin_total)/length(variable$total)
    pAeB <- table(id2bin_failed)/length(variable$total)
    prob_failed_give_size <- pAeB/pB
    prob_failed_give_size[which(is.na(prob_failed_give_size) == TRUE)] = 0
    prob_failed_give_size <- prob_failed_give_size/sum(prob_failed_give_size)
    list_data<- list(prob = prob_failed_give_size)
    names(list_data)[1]<- company
    mylist <- append(mylist,list_data)
    writeLines(paste("Done:", company))
  }
  return (mylist)
}

# This function performs ks_test for a set of data and return a list of k-test results having p-values > value
ks_result_comparison <- function(data, value = 0.05){
  mylist<- list()
  for (i in (1:length(data))){
    for (j in (1:length(data)) ){
      if (i < j) {
        a <- ks.test(data[[i]],data[[j]])
        if (a$p.value > value) {
          a$data.name = glue('{names(data[i])} - {names(data[j])}')
          mylist <- append(mylist, a)
        }
      }
    }
  }
  return (mylist)
}

#This chi-test doesn't considere bin if it's 0 for both data
chi_test_function_C <- function(data1, data2){
  deletebin <- c()
  for (index in (1:length(data1))){
    if (data1[[index]] ==0 & data2[[index]] ==0){
      a <- index
      deletebin <- c(deletebin,a)
    }
  }
  data1= data1[-deletebin]
  data2 = data2[-deletebin]
  bins = sort(unique(c(data1, data2)))
  N1i = sapply(bins, function(i) sum(data1==i))
  N2i = sapply(bins, function(i) sum(data2==i))
  mat = cbind(N1i, N2i)
  return (chisq.test(mat, correct=FALSE))
}

#Multiple chi-test comparison
chi_test_result_comparison <- function(data, value = 0.05){
  mylist<- list()
  for (i in (1:length(data))){
    for (j in (1:length(data)) ){
      if (i < j) {
        a <- chi_test_function_C(data[[i]],data[[j]])
        if (a$p.value > value) {
          a$data.name = glue('{names(data[i])} - {names(data[j])}')
          mylist <- append(mylist, a)
        }
      }
    }
  }
  return (mylist)
}

#Calibration plot
cal_plot <- function(y_test, lr.pconf1, class = '1'){
  # Plot quality measures, calibration and metrics
  cal_data1 = calibration(y_test~ lr.pconf1, class= class)
  plot(cal_data1)
  # manual computation of calibration plot
  nbins=11
  bins = seq(1/nbins, 1, 1/nbins)
  id2bin = cut(lr.pconf1, breaks=c(0,bins), labels=1:nbins)
  bin.total = c(table(id2bin)) # c() to transform to vector
  bin.pos = tapply(as.numeric(y_test)-1, id2bin, sum)
  y = bin.pos/bin.total
  x = ( c(0,bins[-nbins])+bins)/2 # midpoints
  plot(x*100, y*100, type='o', xlim=c(0,100), ylim=c(0,100), 
       col="blue", xlab="Prediction Confidence", ylab="Perc. of Positives")
  abline(coef = c(0,1),col="grey")
  # add confidence interval
  lines(cal_data1$data$midpoint, cal_data1$data$Lower, col="red")
  lines(cal_data1$data$midpoint, cal_data1$data$Upper, col="red")
  # binary-ECE
  s_b = tapply(lr.pconf1, id2bin, mean)
  y_b = y
  binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
  binECE
  return (binECE)
}

#This function returns the ratings given to our test/train-set by our model (chose by quantile)
rating_quantile_function <- function(aida_test_rating,quant,prob_train){
  rating <- list(A = c() , B= c(), C=c(), D=c(), E=c() )
  quant <- quantile(prob_train, probs = quant)
  rating$A <- quant[2]
  rating$B <- quant[3]
  rating$C <- quant[4]
  rating$D <- quant[5]
  rating$E <- 1
  list_rate <- c()
  for (i in (1:nrow(aida_test_rating))){
    if (aida_test_rating$prob[i]<= rating$A) list_rate <- c(list_rate,'A')
    if (aida_test_rating$prob[i] > rating$A && aida_test_rating$prob[i]<=rating$B) list_rate <- c(list_rate,'B')
    if (aida_test_rating$prob[i] > rating$B && aida_test_rating$prob[i]<=rating$C) list_rate <- c(list_rate,'C')
    if (aida_test_rating$prob[i] > rating$C && aida_test_rating$prob[i]<=rating$D) list_rate <- c(list_rate,'D')
    if (aida_test_rating$prob[i]>rating$D) list_rate <- c(list_rate,'E')
  }
  aida_test_rating$Rating <- list_rate
  return (aida_test_rating)
}
#This function returns the ratings given to our test/train-set by our model (chose by probability)
rating_prob_function <- function(aida_test_rating,prob,prob_train){
  rating <- list(A = c() , B= c(), C=c(), D=c(), E=c() )
  rating$A <- prob[2]
  rating$B <- prob[3]
  rating$C <- prob[4]
  rating$D <- prob[5]
  rating$E <- 1
  list_rate <- c()
  for (i in (1:nrow(aida_test_rating))){
    if (aida_test_rating$prob[i]<= rating$A) list_rate <- c(list_rate,'A')
    if (aida_test_rating$prob[i] > rating$A && aida_test_rating$prob[i]<=rating$B) list_rate <- c(list_rate,'B')
    if (aida_test_rating$prob[i] > rating$B && aida_test_rating$prob[i]<=rating$C) list_rate <- c(list_rate,'C')
    if (aida_test_rating$prob[i] > rating$C && aida_test_rating$prob[i]<=rating$D) list_rate <- c(list_rate,'D')
    if (aida_test_rating$prob[i]>rating$D) list_rate <- c(list_rate,'E')
  }
  aida_test_rating$Rating <- list_rate
  return (aida_test_rating)
}

# This function plot rating barplot (normalized to each bin)
barplot_rating <- function(data_rating){
  ok <- c(nrow(filter(data_rating,data_rating$legal_status2=="class1" & data_rating$Rating=='A'))
          /nrow(filter(data_rating,data_rating$Rating=='A')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class0" & data_rating$Rating=='A'))
          /nrow(filter(data_rating,data_rating$Rating=='A')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class1" & data_rating$Rating=='B'))
          /nrow(filter(data_rating,data_rating$Rating=='B')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class0" & data_rating$Rating=='B'))
          /nrow(filter(data_rating,data_rating$Rating=='B')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class1" & data_rating$Rating=='C'))
          /nrow(filter(data_rating,data_rating$Rating=='C')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class0" & data_rating$Rating=='C'))
          /nrow(filter(data_rating,data_rating$Rating=='C')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class1" & data_rating$Rating=='D'))
          /nrow(filter(data_rating,data_rating$Rating=='D')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class0" & data_rating$Rating=='D'))
          /nrow(filter(data_rating,data_rating$Rating=='D')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class1" & data_rating$Rating=='E'))
          /nrow(filter(data_rating,data_rating$Rating=='E')))
  ok <- c(ok,nrow(filter(data_rating,data_rating$legal_status2=="class0" & data_rating$Rating=='E'))
          /nrow(filter(data_rating,data_rating$Rating=='E')))
  # create a dataset
  Class <- c(rep("A" , 2) , rep("B" , 2) , rep("C" , 2) , rep("D" , 2),rep("E" , 2) )
  condition <- rep(c("Failed" , "Active"), 5)
  value <- ok
  data <- data.frame(Class,condition, value)
  # Stacked
  myplot <- ggplot(data, aes(fill=condition, y=value, x=Class)) + 
    geom_bar(position="stack", stat="identity")
  myplot$labels$fill <- "Legal Status"
  print(myplot + labs(y = "%", x = "Class"))
}
