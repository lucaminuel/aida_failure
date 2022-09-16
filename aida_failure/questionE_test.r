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
library(pROC)
library(BSDA)
library(boot)

# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("useful_functions.R")
load(file="aida_test_e.RData")
load(file="aida_train_e.RData")



#####################
#LOGISTIC REGRESSION#
#####################
#Same step question D1-D2
legal_status <- aida_train$`Legal status`
ateco <- aida_train$`ATECO 2007code`
legal_form <- aida_train$`Legal form`
registered <- aida_train$`Registered office address - Region`
#Multicollinearity
categorical <- c('ATECO 2007code', 'Legal form' , 'Registered office address - Region', 'Legal status') 
aida_train[categorical] = NULL #vif apply only to numeric
aida_train <- vf_evaluate(legal_status, aida_train)
aida2train <- aida_train
rm(aida_train)
aida2train <- cbind(aida2train, ateco, legal_form,registered, legal_status )
rm(ateco,legal_form,registered,legal_status)
#Done with multicollinearity, next step: variable selection
fit <- glm(aida2train$legal_status~ ., data=aida2train, family=binomial("logit"))
step <- stepAIC(fit, direction="backward")
#save(step, file = "step_AIC_e.RData")
#load('step_AIC_e.RData')
#See columns remained after stepAIC
formula_AIC <- step[["formula"]] 
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
aida2test <- aida2test[col_remain]
#Train the model
# Subset train and test only variable for our model(formula_AIC variables), we delete also 'Last accounting closing date'
column_model1 <- c( 'Banks/turnover%Last avail. yr', 'Cash Flowth EURLast avail. yr',
                    'Cost of debit (%)%Last avail. yr',
                    'Current ratioLast avail. yr',
                    'Current liabilities/Tot ass.%Last avail. yr',
                    'Debt/equity ratio%Last avail. yr' ,
                    'EBITDA/Vendite%Last avail. yr' ,
                    'Interest/Turnover (%)%Last avail. yr',
                    'LeverageLast avail. yr' ,
                    'Net financial positionth EURLast avail. yr',
                    'Return on asset (ROA)%Last avail. yr' ,
                    'Return on equity (ROE)%Last avail. yr' ,
                    'Return on investment (ROI) (%)%Last avail. yr' , 
                    'Solvency ratio (%)%Last avail. yr',
                    'Total assets turnover (times)Last avail. yr' ,'Age' , 'ateco' , 
                    'legal_form' , 'registered','legal_status')
aida_train_model1 <- aida2train[column_model1]
aida_test_model1 <- aida2test[column_model1]



# First train: no resampling
noresampling = trainControl(method="none")
lr1.fit_e <- train(legal_status ~ ., data=aida_train_model1, family=binomial("logit"), 
                 method = 'glm', trControl=noresampling)
lr.prob1 <- predict(lr1.fit_e, newdata = aida_test_model1, type = 'prob')
lr.pred1 <- predict(lr1.fit_e, newdata = aida_test_model1)
lr.pconf1 <- lr.prob1[,2]
conf1 <- confusionMatrix(lr.pred1, aida_test_model1$legal_status, positive="1")
# featurePlot
featurePlot(x=data.frame(lr.pconf1), y=aida_test_model1$legal_status, plot='density', auto.key = list(columns = 2))
#optimal cutoff in a range of 0.5
cutoff <- 0.5
lr.pred.cutoff1 <- factor(ifelse(lr.pconf1>=cutoff, 1, 0), levels=levels(aida_test_model1$legal_status))
confusionMatrix(lr.pred.cutoff1, aida_test_model1$legal_status, positive="1")

# Plot quality measures, calibration and metrics
lr.prediction1 <-  prediction(lr.pconf1, aida_test_model1$legal_status)
cal_plot(y_test= aida_test_model1$legal_status,lr.pconf =lr.pconf1)
# acc at cutoff
lr.acc1 <- performance(lr.prediction1, "acc"); plot(lr.acc1)
# tpr at cutoff
lr.tpr1 <- performance(lr.prediction1, "tpr"); plot(lr.tpr1)
# f1 at cutoff
lr.f11 <- performance(lr.prediction1, "f"); plot(lr.f11)
# roc curve
lr.roc1 <- performance(lr.prediction1, "tpr", "fpr")
plot(lr.roc1, colorize=T); abline(a=0, b= 1)
o1 <- performance(lr.prediction1,"auc")
o1@y.values[[1]]
ci.auc(aida_test_model1$legal_status, lr.pconf1)
# other metrics
precision(lr.pred1, aida_test_model1$legal_status, relevant="1") # or PPV
recall(lr.pred1, aida_test_model1$legal_status, relevant="1") # or sensitivity
F_meas(lr.pred1, aida_test_model1$legal_status, relevant="1")

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
lr2.fit_e <- train(legal_status2~ ., data =aida_train_model1, 
                 method = "glm", trControl=rcv, metric="AUC", # custom metric
                 # pass-trough options
                 family=binomial(logit))
#saveRDS(lr2.fit_e, file= 'lr2fit_e.rds')
#Diagnostic Plot
summary(lr2.fit_e$finalModel)
par(mfrow=c(2,1))
plot(lr2.fit_e$finalModel)
par(mfrow=c(1,1))
lr2.fit_e<-loadRDS('lr2fit_e.rds')
lr2.fit_e$resample # details over 5x10 folds
lr2.folds_e <- lr2.fit_e$resample[['AUC']]
mean(lr2.folds_e) # reported is mean performance
#save(lr2.folds_e, file='lr2folds_e.RData')
laod('lr2folds_e.RData')
shapiro.test(lr2.folds_e)
z.test(lr2.folds_e, mu = mean(lr2.folds_e), sigma.x = sd(lr2.folds_e))
lr.prob2 <- predict(lr2.fit_e, newdata = aida_test_model1, type = 'prob')
lr.pred2 <- predict(lr2.fit_e, newdata = aida_test_model1)
lr.pconf2 <- lr.prob2[,2]
conf2 <- confusionMatrix(lr.pred2, aida_test_model1$legal_status2)
#Feature Plot
featurePlot(x=data.frame(lr.pconf2), y=aida_test_model1$legal_status2, plot='density', auto.key = list(columns = 2))
# Plot quality measures, calibration and metrics
lr.prediction2 <-  prediction(lr.pconf2, aida_test_model1$legal_status2)
par(mfrow = c(2,1))
cal_plot(y_test= aida_test_model1$legal_status2,lr.pconf =lr.pconf2,class='class1')
lr.roc2 <- performance(lr.prediction2, "tpr", "fpr")
plot(lr.roc2, colorize=T); abline(a=0, b= 1)
# acc at cutoff
par(mfrow = c(3,1))
lr.acc1 <- performance(lr.prediction1, "acc"); plot(lr.acc1)
# tpr at cutoff
lr.tpr1 <- performance(lr.prediction1, "tpr"); plot(lr.tpr1)
# f1 at cutoff
lr.f11 <- performance(lr.prediction1, "f"); plot(lr.f11)
par(mfrow = c(1,1))
o2 <- performance(lr.prediction1,"auc")
o2@y.values[[1]]
ci.auc(aida_test_model1$legal_status2, lr.pconf2)
# other metrics
precision(lr.pred2, aida_test_model1$legal_status2, relevant="class1") # or PPV
recall(lr.pred2, aida_test_model1$legal_status2, relevant="class1") # or sensitivity
F_meas(lr.pred2, aida_test_model1$legal_status2, relevant="class1")
mean((lr.pconf2 - (as.numeric(aida_test_model1$legal_status2)-1))^2)




###############
#RANDOM FOREST#
###############
load(file="aida_test_e.RData")
load(file="aida_train_e.RData")
colnames(aida_train)[which(names(aida_train) == "Legal status")] <- "legal_status"
colnames(aida_test)[which(names(aida_test) == "Legal status")] <- "legal_status"
noresampling = trainControl(method="none")
tunegrid = expand.grid(.mtry=3) # fixed value, no grid search
set.seed(42) # same folds as lr2.fit
rf.fit_e <- train(legal_status~., data = aida_train, 
                method = "rf",  tuneGrid=tunegrid,trControl=noresampling,
                # pass-trough options
                ntree=15)
rf.pred <- predict(rf.fit_e, newdata = aida_test)
rf.prob <- predict(rf.fit_e, newdata = aida_test, type="prob") # as predict_proba in scikit-learn
rf.pconf <- rf.prob[,2] # scores
# built-in metrics
conf1rf <- confusionMatrix(rf.pred, aida_test$legal_status, positive="1")
rf.prediction <-  prediction(rf.pconf, aida_test$legal_status)

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
rf.fit2_e <- train(legal_status2~., data = aida_train2, 
                 method = "rf",  tuneGrid=tunegrid,trControl=rcv, metric="AUC",
                 # pass-trough options
                 ntree=15)

#save(rf.fit2_e, file = 'rffit2_e.RData')
load('rffit2_E.RData')
rf2.folds_e <-rf.fit2_e$resample[['AUC']]
#save(rf2.folds_e, file = 'rf2folds.RData')
load('rf2folds.RData')
shapiro.test(rf2.folds_e)
t.test(rf2.folds_e)
mean(rf2.folds_e)
#performance over the test-set
rf.pred2 <- predict(rf.fit2_e, newdata = aida_test2)
rf.prob2 <- predict(rf.fit2_e, newdata = aida_test2, type="prob") # as predict_proba in scikit-learn
rf.pconf2 <- rf.prob2[,2] # scores
# built-in metrics
conf2 <- confusionMatrix(rf.pred2, aida_test2$legal_status2, positive="class1")
#FEATURE PLOT
par(mfrow = c(1,1))
featurePlot(x=data.frame(rf.pconf2), y=aida_test2$legal_status2, plot='density', auto.key = list(columns = 2))
# other metrics
# Plot quality measures, calibration and metrics
par(mfrow=c(3,1))
rf.prediction <-  prediction(rf.pconf2, aida_test2$legal_status2)
rf.acc1 <- performance(rf.prediction, "acc"); plot(rf.acc1)
# tpr at cutoff
rf.tpr1 <- performance(rf.prediction, "tpr"); plot(rf.tpr1)
# f1 at cutoff
rf.f11 <- performance(rf.prediction, "f"); plot(rf.f11)

precision(rf.pred2, aida_test2$legal_status2, relevant="class1") # or PPV
recall(rf.pred2, aida_test2$legal_status2, relevant="class1") # or sensitivity
F_meas(rf.pred2, aida_test2$legal_status2, relevant="class1")
# Brier score
mean((rf.pconf2 - (as.numeric(aida_test2$legal_status2)-1))^2)

# calibration plot: P(Y=1|s(W) \in [a, b])
#previsioni sul test
par(mfrow = c(2,1))
rf.prediction2 <-  prediction(rf.pconf2, aida_test2$legal_status2)
cal_plot(y_test= aida_test2$legal_status2,lr.pconf1 =rf.pconf2,class='class1')
rf.roc2 <- performance(rf.prediction2, "tpr", "fpr")
plot(rf.roc2, colorize=T, labels = 'rcv',); abline(a=0, b= 1)
o2 <- performance(rf.prediction2,"auc")
o2@y.values[[1]]
ci.auc(aida_test2$legal_status2, rf.pconf2)
