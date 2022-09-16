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
library(ggplot2)
library(lmtest)
library(BSDA)
library(pROC)
# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("useful_functions.R")
load(file = "aida_train.RData")
load(file = "aida_test.RData")



##################
#BUILD MODEL (RF)#
##################
# First train: no resampling
colnames(aida_train)[which(names(aida_train) == "Legal status")] <- "legal_status"
colnames(aida_test)[which(names(aida_test) == "Legal status")] <- "legal_status"
noresampling = trainControl(method="none")
tunegrid = expand.grid(.mtry=3) # fixed value, no grid search
set.seed(42) # same folds as lr2.fit
rf.fit <- train(legal_status~., data = aida_train, 
                method = "rf",  tuneGrid=tunegrid,trControl=noresampling,
                # pass-trough options
                ntree=15)
rf.fit
rf.pred <- predict(rf.fit, newdata = aida_test)
rf.prob <- predict(rf.fit, newdata = aida_test, type="prob") # as predict_proba in scikit-learn
rf.pconf <- rf.prob[,2] # scores
rf.prediction <-  prediction(rf.pconf, aida_test$legal_status)
rf.roc1 <- performance(rf.prediction, "tpr", "fpr")

# Second train with rcv
# need to use class names that are valid variable identifiers
aida_train2 <- aida_train
aida_test2 <- aida_test
lev <- c("class0", "class1")
aida_train2$legal_status2 <- factor(aida_train2$legal_status); levels(aida_train2$legal_status2) <- lev
aida_test2$legal_status2 <- factor(aida_test2$legal_status); levels(aida_test2$legal_status2) <- lev
rf.pred2 <- as.factor(rf.pred); levels(rf.pred2) <- lev
aida_train2$legal_status <- NULL
aida_test2$legal_status <- NULL
# custom summary
custSummary <-  function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
rcv <- trainControl(method="repeatedcv", repeats=6, number=12, 
                   classProbs = TRUE, summaryFunction=custSummary)
rf.fit2 <- train(legal_status2~., data = aida_train2, 
                          method = "rf",  tuneGrid=tunegrid,trControl=rcv, metric="AUC",
                          # pass-trough options
                          ntree=15)

save(rf.fit2, file = 'rffit2.RData')
#load('rffit2.RData')
rf2.folds <-rf.fit2$resample[['AUC']]
#save(rf2.folds, file = 'rf2folds.RData')
#load('rf2folds.RData')
mean(rf2.folds)
shapiro.test(rf2.folds)
z.test(rf2.folds, mu = mean(rf2.folds), sigma.x= sd(rf2.folds)) # accuracy confidence interval
#performance over the test-set
rf.pred2 <- predict(rf.fit2, newdata = aida_test2)
rf.prob2 <- predict(rf.fit2, newdata = aida_test2, type="prob") # as predict_proba in scikit-learn
rf.pconf2 <- rf.prob2[,2] # scores
# built-in metrics
conf2 <- confusionMatrix(rf.pred2, aida_test2$legal_status2, positive="class1")

#FEATURE PLOT
featurePlot(x=data.frame(rf.pconf2), y=aida_test2$legal_status2, plot='density', auto.key = list(columns = 2))
#we see the interception at 0.3
cutoff <- 0.3
rf.pred.cutoff2 <- factor(ifelse(rf.pconf2>=cutoff, 'class1', 'class0'), levels=levels(aida_test2$legal_status2))
conf22 <-confusionMatrix(rf.pred.cutoff2, aida_test2$legal_status2, positive="class1")
conf22



# other metrics
# Plot quality measures, calibration and metrics
rf.prediction <-  prediction(rf.pconf2, aida_test2$legal_status2)
par(mfrow = c(3,1))
rf.acc1 <- performance(rf.prediction, "acc"); plot(rf.acc1)
# tpr at cutoff
rf.tpr1 <- performance(rf.prediction, "tpr"); plot(rf.tpr1)
# f1 at cutoff
rf.f11 <- performance(rf.prediction, "f"); plot(rf.f11)
par(mfrow = c(1,1))
precision(rf.pred2, aida_test2$legal_status2, relevant="class1") # or PPV
recall(rf.pred2, aida_test2$legal_status2, relevant="class1") # or sensitivity
F_meas(rf.pred2, aida_test2$legal_status2, relevant="class1")
# Brier score
mean((rf.pconf2 - (as.numeric(aida_test2$legal_status2)-1))^2)

# calibration plot: P(Y=1|s(W) \in [a, b])
cal_plot(y_test= aida_test2$legal_status2,lr.pconf1 =rf.pconf2,class='class1')
#previsioni sul test
rf.prediction2 <-  prediction(rf.pconf2, aida_test2$legal_status2)
rf.roc2 <- performance(rf.prediction2, "tpr", "fpr")
plot(rf.roc2, colorize=T, labels = 'rcv',); abline(a=0, b= 1)
o2 <- performance(rf.prediction2,"auc")
o2@y.values[[1]]
ci.auc(aida_test2$legal_status2, rf.pconf2)

#ROC curve rcv and with rcv
plot(rf.roc1, col = "red", labels = 'no rcv', main ='Random Forest ROC'); abline(a=0, b= 1)
plot(rf.roc2, col= "blue", add = TRUE, labels = 'rcv'); abline(a=0, b= 1)
legend("bottomright", legend=c("no rcv","rcv"), col=c('red','blue'), pt.cex=2, pch=15)

#save(rf.roc2, file = 'rfroc2.RData')
#load('rfroc2.RData')

#RATING MODEL

# Building rating model from scoring
# Score training
rf.probtrain <- predict(rf.fit2, newdata = aida_train2, type = 'prob')
rf.predtrain<- predict(rf.fit2, newdata = aida_train2)

hist_prtrain <- hist(rf.probtrain[,2],freq = FALSE, breaks = 10)
# Histogram scoring training
hist_prtrain$density <- hist_prtrain$counts/sum(hist_prtrain$counts)
plot(hist_prtrain,  freq= FALSE)
par(mfrow = c(1,1))
prob_train <- rf.probtrain[,2]
#Choosen quantile binning
perc <- c(0,0.1,0.2,0.4,0.7,1)
# Barplot train (quantile)
aida_train_rating <- aida_train2
aida_train_rating$prob <- rf.probtrain[,2]
aida_train_rating_quant <- rating_quantile_function(aida_test_rating=aida_train_rating, quant=perc, prob_train)
barplot_rating(aida_train_rating_quant)
quantiles1 <- quantile(rf.probtrain[,2], probs = perc)
quantiles1 <- quantiles1*100
quantiles1
# Prepare test dataframe for rating (quantile)
aida_test_rating <- aida_test2 
aida_test_rating$prob <- rf.prob2[,2]
aida_test_rating_quant <- rating_quantile_function(aida_test_rating=aida_test_rating, quant=perc, prob_train=rf.probtrain[,2])
barplot_rating(aida_test_rating_quant)

# Testing a different approach for binning the class
# Barplot train (prob)
aida_train_rating <- aida_train2
aida_train_rating$prob <- rf.probtrain[,2]
aida_train_rating_prob <- rating_prob_function(aida_test_rating=aida_train_rating, prob=perc, prob_train)
barplot_rating(aida_train_rating_prob)
# Prepare test dataframe for rating (prob)
aida_test_rating <- aida_test2 
aida_test_rating$prob <- rf.prob2[,2]
aida_test_rating_prob <- rating_prob_function(aida_test_rating=aida_test_rating, prob=perc, prob_train=rf.probtrain[,2])
barplot_rating(aida_test_rating_prob)

#Confront rcv logistic with rcv Random Forest
load('lr2folds.RData')
shapiro.test(lr2.folds)
shapiro.test(rf2.folds)
#F-test
var.test(rf2.folds,lr2.folds)
boxplot(lr2.folds,rf2.folds)
t.test(lr2.folds,rf2.folds, var.equal = TRUE)