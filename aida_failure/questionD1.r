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
library(BSDA)
library(ggplot2)
library(multcomp)
library(pROC)


# clean all3
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("useful_functions.R")
load(file = "aida_train.RData")
load(file = "aida_test.RData")

################
#BUILD MODEL GM#
################

#Saving categorical feature to perform ViF
legal_status <- aida_train$`Legal status`
ateco <- aida_train$`ATECO 2007code`
legal_form <- aida_train$`Legal form`
registered <- aida_train$`Registered office address - Region`

#Multicollinearity
categorical <- c('ATECO 2007code', 'Legal form' , 'Registered office address - Region', 'Legal status') 
aida_train[categorical] = NULL  #vif apply only to numeric
aida_train <- vf_evaluate(legal_status, aida_train)#vif
aida2train <- aida_train
rm(aida_train)
aida2train <- cbind(aida2train, ateco, legal_form,registered, legal_status )#this is our train
rm(ateco,legal_form,registered,legal_status)

#Done with multicollinearity, next step: variable selection
fit <- glm(aida2train$legal_status~ ., data=aida2train, family=binomial("logit"))
step <- stepAIC(fit, direction="backward")
save(step, file = "step_AIC.RData")
#load('step_AIC.RData')
#See columns remained after stepAIC
formula_AIC <- step[["formula"]]

#Rebalance test dataset with same train's feature
col_remain <- colnames(aida2train)
legal_status <- aida_test$`Legal status`
ateco <- aida_test$`ATECO 2007code`
legal_form <- aida_test$`Legal form`
registered <- aida_test$`Registered office address - Region`
categorical <- c('ATECO 2007code', 'Legal form' , 'Registered office address - Region', 'Legal status') 
aida2test <- aida_test
aida2test[categorical] = NULL 
rm(aida_test)
aida2test <- cbind(aida2test, ateco,legal_form, registered, legal_status)
rm(ateco,legal_form, registered, legal_status)
aida2test <- aida2test[col_remain]#this is our test

#Train the model
formula_AIC

# Subset train and test using variable for our model(formula_AIC variables)
#we delete also 'Last accounting closing date'

column_model1 <- c( 'Banks/turnover%Last avail. yr','Cost of debit (%)%Last avail. yr' , 'Current liabilities/Tot ass.%Last avail. yr' , 
                    'Current ratioLast avail. yr' ,'Debt/EBITDA ratio%Last avail. yr','Debt/equity ratio%Last avail. yr',
                    'EBITDA/Vendite%Last avail. yr' , 
                    'EBITDAth EURLast avail. yr',
                    'LeverageLast avail. yr' , 'Liquidity ratioLast avail. yr' , 
                    'Net financial positionth EURLast avail. yr',
                    'Return on equity (ROE)%Last avail. yr' ,
                    'Return on investment (ROI) (%)%Last avail. yr' , 
                    'Solvency ratio (%)%Last avail. yr',
                    'Total assets turnover (times)Last avail. yr' ,'Age' , 'ateco' , 
                    'legal_form' , 'registered','legal_status')


#FINALLY
aida_train_model1 <- aida2train[column_model1] #train after stepAIC
aida_test_model1 <- aida2test[column_model1] #test after stepAIC

#First train: no resampling
noresampling = trainControl(method="none")
lr.fit1 <- train(legal_status ~ ., data=aida_train_model1, family=binomial("logit"), 
                 method = 'glm', trControl=noresampling)
lr.pred1 <- predict(lr.fit1, newdata = aida_test_model1)


# using ROC as metric
# need to use class names that are valid variable identifiers
lev <- c("class0", "class1")
aida_train_model1$legal_status2 <- factor(aida_train_model1$legal_status); levels(aida_train_model1$legal_status2) <- lev
aida_test_model1$legal_status2 <- factor(aida_test_model1$legal_status); levels(aida_test_model1$legal_status2) <- lev
lr.pred2 <- as.factor(lr.pred1); levels(lr.pred2) <- lev
aida_train_model1$legal_status <- NULL
aida_test_model1$legal_status <- NULL
# custom summary
custSummary <-  function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
# repeated cross validation with custom summary function
# setting repeated cross-validation
rcv <- trainControl(method="repeatedcv", repeats=6, number=12, 
                    classProbs = TRUE, summaryFunction=custSummary)
set.seed(42) # reproducibility of folds
lr2.fit <- train(legal_status2~ ., data =aida_train_model1, 
                 method = "glm", trControl=rcv, metric="AUC", # custom metric
                 # pass-trough options
                 family=binomial(logit))

save(lr2.fit, file = 'lr2fit.RData')
#load('lr2fit.RData')
summary(lr2.fit$finalModel)
par(mfrow = c(2,1))
plot(lr2.fit$finalModel)
par(mfrow = c(1,1))
lr2.fit$resample # details over 5x10 folds
lr2.folds <- lr2.fit$resample[['AUC']]
save(lr2.folds, file='lr2folds.RData')
#load('lr2folds.RData')
mean(lr2.folds) # reported is mean performance
shapiro.test(lr2.folds)
z.test(lr2.folds, mu = mean(lr2.folds), sigma.x= sd(lr2.folds)) # accuracy confidence interval
lr.prob2 <- predict(lr2.fit, newdata = aida_test_model1, type = 'prob')
lr.pred2 <- predict(lr2.fit, newdata = aida_test_model1)
lr.pconf2 <- lr.prob2[,2]
conf2 <- confusionMatrix(lr.pred2, aida_test_model1$legal_status2)


# featurePlot
featurePlot(x=data.frame(lr.pconf2), y=aida_test_model1$legal_status2, plot='density', auto.key = list(columns = 2))
#optimal cutoff in a range of 0.5
cutoff <- 0.5
lr.pred.cutoff2 <- factor(ifelse(lr.pconf2>=cutoff, 'class1', 'class0'), levels=levels(aida_test_model1$legal_status2))
confusionMatrix(lr.pred.cutoff2, aida_test_model1$legal_status2, positive="class1")

# Plot quality measures, calibration and metrics
lr.prediction2 <-  prediction(lr.pconf2, aida_test_model1$legal_status)
par(mfrow= c (3,1))
# acc at cutoff
lr.acc2 = performance(lr.prediction2, "acc"); plot(lr.acc2)
# tpr at cutoff
lr.tpr2 = performance(lr.prediction2, "tpr"); plot(lr.tpr2)
# f1 at cutoff
lr.f12 = performance(lr.prediction2, "f"); plot(lr.f12)
# roc curve
lr.roc2 <- performance(lr.prediction2, "tpr", "fpr")
#save(lr.roc2, file='lrroc2.RData')
par(mfrow = c(2,1))
plot(lr.roc2, colorize=T); abline(a=0, b= 1)
cal_plot(y_test= aida_test_model1$legal_status2,lr.pconf1 = lr.pconf2, class= 'class1')
o2 <- performance(lr.prediction2,"auc")
o2@y.values[[1]]
#CI DeLong
ci.auc(aida_test_model1$legal_status2, lr.pconf2)
# other metrics
precision(lr.pred2, aida_test_model1$legal_status2, relevant="class1") # or PPV
recall(lr.pred2, aida_test_model1$legal_status2, relevant="class1") # or sensitivity
F_meas(lr.pred2, aida_test_model1$legal_status2, relevant="class1")
o2 <- performance(lr.prediction2,"auc")
o2@y.values[[1]]

#Brier
mean((lr.pconf2 - (as.numeric(aida_test_model1$legal_status2)-1))^2)
par(mfrow=c(1,1))

#CONFIDENCE INTERVALS FOR PREDICTION ON THE TEST SET
fit <- glm(formula=aida_train_model1$legal_status2~., family=binomial("logit"), data=aida_train_model1)

# fitted confidence interval for each score
x1 <- seq(1, nrow(aida_test_model1), 1)
#y1 <- (as.numeric(aida_test_model1$legal_status2)-1)

#prediction over the test
test_data <- aida_test_model1
ci <- add_ci(test_data, fit, alpha = 0.05, names = c("lwr", "upr"))
dataplot=data.frame(ci$lwr,ci$pred,ci$upr,test_data$legal_status2)
dataplot=dataplot[order( dataplot[,2] ),]
plot(x1,dataplot$ci.pred,ylim=c(0,1),ylab="Score",xlab="firm",main="Confidence intervals for predictions")
points(x1,dataplot$ci.lwr,col="blue",type="l")
points(x1,dataplot$ci.upr,col="blue",type="l")
points(x1,(as.numeric(dataplot$test_data.legal_status2)-1),col="red")
legend(0.7,0.7, legend=c("score","true class","CI"), col=c("black","red","blue"),cex=0.6, pt.cex=1, pch=15)
xo=sum(ci$pred < min(ci$pred[which(ci$pred>=0.5)]))#firm with score 0.5
abline(v=xo,col="green")
abline(a=0.5,b=0,col="green")

#this should be the minimum binwidth for our rating model (check if quant1 satisfies)
mean(ci$upr-ci$lwr)

# Building rating model from scoring
# Score training
lr.probtrain <- predict(lr2.fit, newdata = aida_train_model1, type = 'prob')
lr.predtrain<- predict(lr2.fit, newdata = aida_train_model1)

hist_prtrain <- hist(lr.probtrain[,2],freq = FALSE, breaks = 10)
# Histogram scoring training
hist_prtrain$density <- hist_prtrain$counts/sum(hist_prtrain$counts)
plot(hist_prtrain,  freq= FALSE)
par(mfrow = c(1,1))
prob_train <- lr.probtrain[,2]
#Choosen quantile binning
quant <- c(0,0.2,0.4,0.6,0.8,1)
# Barplot over the train
aida_train_rating <- aida_train_model1
aida_train_rating$prob <- lr.probtrain[,2]
aida_train_rating <- rating_quantile_function(aida_train_rating, quant, lr.probtrain[,2])
barplot_rating(aida_train_rating)
quant1 <- quantile(prob_train, probs = quant)
quant1 <- quant1*100
quant1
# Prepare test dataframe for rating
aida_test_rating <- aida_test_model1
aida_test_rating$prob <- lr.prob2[,2]
aida_test_rating <- rating_quantile_function(aida_test_rating, quant, lr.probtrain[,2])
barplot_rating(aida_test_rating)

