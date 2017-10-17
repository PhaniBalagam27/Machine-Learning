
library(caret)
library(plyr)
library(dplyr)
library(C50)
library(kernlab)
#Load input data 

bank_full <- read.csv("'E:/AML - BUAN 6341' ",sep = ";", header = TRUE)
str(bank_full)

class(bank_full)
nomvars <- c(2:10,15,21)
str(nomvars)
library(mlbench)
# how many missing values.
sum(is.na(bank_full))
# Lets check correlations , see - nomvars to exclude nominal variables for dimensionality reduction,
# not looking for factors but only nominal attributes
corrMatrix <- cor(bank_full[,-nomvars], use = "pairwise.complete.obs")

?cor
# Are there NA's ?
table(is.na(corrMatrix))
# replace NA's
corrMatrix <- replace(corrMatrix, which(is.na(corrMatrix)), 0)
corrMatrix
# Column indexes to remove
#where corelation is >0.9
rmcols <- findCorrelation(corrMatrix, cutoff = 0.9, verbose = FALSE)


# Column names to remove
colnames(corrMatrix[,rmcols])

# New dataset with columns removed
#bank_full2 <- bank_full[,-which(names(bank_full) %in% colnames(corrMatrix[,rmcols]))]
bank_full2 <- bank_full
# Factor levels-- we can do it in carrot much more easily
faclen <- bank_full2 %>% Filter(f = is.factor) %>% sapply(levels) %>% sapply(length) %>% (function(x) x[x<2])# these are all factors of length 1
facnames <-names(bank_full2) %in% names(faclen)
bank_full3 <- bank_full2[!facnames]

# Zero variance
# nearzerovar is from carrot package, it gives me data which has variance near to 0
nzv <- nearZeroVar(bank_full3)
nzv
bank_full4 <- bank_full3[,-nzv]
bank_full4
View(bank_full4)

TrainingDataIndex <- createDataPartition(bank_full4$y, p=0.70, list = FALSE)

# Create Training Data as subset 
trainingData <- bank_full4[TrainingDataIndex,]

# Everything else not in training is test data.
testData <- bank_full4[-TrainingDataIndex,]

# setting training control parameters
NoTrainingParameters <- trainControl(method = "none")# cross validation or boot straping or repeatedcv
TrainingParameters <- trainControl(method = "cv", number = 10)

##################SVM##########################
library(e1071)

bank_full_linear.tune<-tune(svm,y~.,data=trainingData,probability=TRUE,kernel = "linear", ranges=list(cost=c(0.01,0.1,1,5,10,100)))

bank_full_ploynomial.tune<-tune(svm,y~.,data=trainingData,probability=TRUE,kernel = "polynomial",ranges=list(cost=c(0.01,0.1,1,5,10,100)))

bank_full_radial.tune<-tune(svm,y~.,data=trainingData,probability=TRUE,kernel = "radial",ranges=list(cost=c(0.01,0.1,1,5,10,100)))

######################## metrics##################

metrics <- function(best.model){
  bestmodal.tune<- best.model
  SVMPredictions <-predict(bestmodal.tune, testData)
  predicted_prob <- predict(bestmodal.tune, newdata=testData, type = "prob")
  pred<-predict(bestmodal.tune,testData,probability=TRUE)
  cmSVM <-confusionMatrix(SVMPredictions, testData$y)
  cmSVM
  a <- cmSVM$overall
  b <- cmSVM$byClass
  list1 <- list(cmSVM,a,b)
  return(list1)
}


svm_linear <- metrics(bank_full_linear.tune$best.model)
svm_ploy <- metrics(bank_full_ploynomial.tune$best.model)
svm_radial <- metrics(bank_full_radial.tune$best.model)
svm_linear
svm_ploy
svm_radial

###################################### plot########################

library(ROCR)
library(ggplot2)
library(pROC)

plotroc <- function(model1){
  bestmodal.tune<- model1
  pred.svm1<-predict(bestmodal.tune,testData,probability=TRUE)
  pred.svm2 <- attr(pred.svm1,"probabilities")
  new_pred <- pred.svm2[,2]
  pred1 <- prediction(predictions = new_pred,labels = testData$y)
  perf <- performance(pred1, measure = "tpr", x.measure = "fpr")
  fpr.svm <- perf@x.values[[1]]
  tpr.svm <- perf@y.values[[1]]
  list2 <- list(fpr.svm,tpr.svm)
  return(list2)
}


perf.auc <- performance(pred, measure = "auc")

str(perf.auc)

perf.auc@y.values


linearpl <- plotroc(bank_full_linear.tune$best.model)
polypl <- plotroc(bank_full_ploynomial.tune$best.model)
radilpl <- plotroc(bank_full_radial.tune$best.model)

linearpl <- as.data.frame(linearpl)
colnames(linearpl)[1] <- "FalsepositiveRate"
colnames(linearpl)[2] <- "TruepositiveRate"

polypl <- as.data.frame(polypl)
colnames(polypl)[1] <- "FalsepositiveRate"
colnames(polypl)[2] <- "TruepositiveRate"




df <- data.frame(tpr=radilpl[2], fpr=radilpl[1],
                 Method=c(rep("SVM Radial",each=length(radilpl[2]))))
ggplot(aes(x=FalsepositiveRate, y=TruepositiveRate, color=Method), data=df) + geom_line() + xlim(0.00, 1.0) + geom_point() + ggtitle("        ROC Curve for Radial SVM")

df <- data.frame(tpr=polypl[2], fpr=polypl[1],
                 Method=c(rep("SVM Polynomial",each=length(polypl[2]))))
ggplot(aes(x=FalsepositiveRate, y=TruepositiveRate, color=Method), data=df) + geom_line() + xlim(0.00, 1.0) + geom_point() + ggtitle("        ROC Curve for Radial SVM")


#####################################################

plot(perf, main = "ROC curve for Desicion Tree",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

perf.auc <- performance(pred, measure = "auc")

str(perf.auc)



library(pROC)
library(ROCR)
?predict
f1 = roc(y ~ CreditHistory.Critical, data=training) 

?roc
plot(ROC) 

######################### Descision Tree ##########################
DecTreeModel <- train(y ~., data=trainingData, 
                      method = "C5.0",# is the decision tree
                      trControl= TrainingParameters,
                      na.action = na.omit# if missing values omit them
)
summary(DecTreeModel)

DecTreeModel$bestTune
#Predict
DTPredictions <-predict(DecTreeModel, testData, na.action = na.pass)
# See predictions
DTPredictions
predicted_prob_DT <- predict(DecTreeModel, testData, type = "prob")
head(predicted_prob_DT)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(stats)
fancyRpartPlot(DecTreeModel)
library(ROCR)
pred_DT <- prediction(predictions = predicted_prob_DT$yes,
                      labels = testData$y)
perf_DT <- performance(pred_DT, measure = "tpr", x.measure = "fpr")
perf.auc <- performance(pred_DT, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
library(caret)
# Create confusion matrix
cm <-confusionMatrix(DTPredictions, testData$y)
cm
cm$overall
cm$byClass

fpr.DT <- perf_DT@x.values[[1]]
tpr.DT <- perf_DT@y.values[[1]]

library(ggplot2)
df <- data.frame(tpr=c(tpr.DT), fpr=c(fpr.DT),
                 Method=c(rep("Decision Tree",each=length(tpr.DT))))
ggplot(aes(x=fpr, y=tpr, color=Method), data=df) + geom_line() + xlim(0.00, 1.0) + geom_point() +  ggtitle("        ROC  curve for Decision Tre")

###################################################

install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
library(caret)
tree<-rpart(y~., data=testData, method="class")
rpart.plot(tree,type=4,extra=101)
tpred=predict(tree,testData,type="class")
plot(tpred)
plotcp(tree)
tcm<-confusionMatrix(tpred,testData$y)
tcm
#Pruning
pruned=prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
tpred2=predict(pruned,testData,type="class")
Purned_Tree<-confusionMatrix(tpred2,testData$y)
Purned_Tree
Purned_Tree$overall
Purned_Tree$byClass

fpr.DT <- tcm2@x.values[[1]]
tpr.DT <- tcm2@y.values[[1]]

library(ggplot2)
df <- data.frame(tpr=c(tpr.DT), fpr=c(fpr.DT),
                 Method=c(rep("Decision Tree",each=length(tpr.DT))))
ggplot(aes(x=fpr, y=tpr, color=Method), data=df) + geom_line() + xlim(0.00, 1.0) + geom_point() +  ggtitle("        ROC  curve for Decision Tre")

predicted_prob_prun <- predict(pruned, testData, type = "prob")
predicted_prob_prun
library(ROCR)


################# Boosting################
library(adabag);
adaboost<-boosting(y~., data=trainingData, boos=TRUE, mfinal=20,coeflearn='Breiman',method = "AdaBoost.M1")
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,trainingData)
predada <- predict(adaboost,testData)
predada$prob
library(ROCR)
pred <- prediction(predictions = predada$prob[,2],
                   labels = testData$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

plot(perf, main = "ROC curve for  boosting",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
predada$confusion
predada$error

t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)

Accuracy <- 1-predada$error
Accuracy
#############################Prune ###########################

adaboost.prune <- boosting.cv(y~., data=trainingData, v = 10, boos = TRUE, mfinal = 100,
                              coeflearn = "Breiman")

summary(adaboost.prune)
adaboost.prune$trees
adaboost.prune$weights
adaboost.prune$importance
errorevol(adaboost.prune,trainingData)
predada <- predict(adaboost.prune,testData)
predada$prob
library(ROCR)
pred <- prediction(predictions = predada$prob[,2],
                   labels = testData$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

plot(perf, main = "ROC curve for Pruned boosting",
     col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)
predada$confusion
predada$error

t1<-adaboost.prune$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)


