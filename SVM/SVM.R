
#svm
#for linear kernel
pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE)
#install.packages("fortunes")
library(dplyr)
#install.packages("e1071")
library("e1071")

pima_norm<-pima %>% mutate_each_(funs(scale),vars=c("V1","V2","V3","V4","V5","V6","V7","V8") ) 

Accuracy_kernel_linear<-numeric(10)
for (l in 1:10) {
  smp_size <- floor(0.90 * nrow(pima))
  train_ind <- sample(seq_len(nrow(pima)), size = smp_size)
  train <- pima[train_ind, ]
  test <- pima[-train_ind, ]
  train$V9 <- as.factor(train$V9)
  test$V9 <- as.factor(test$V9)
  svmmodel <- svm(V9 ~ ., data = train,kernel="linear", cost = 4, gamma = 0.5)
  #mytunedsvm <- tune.svm(V9 ~ ., data = train,kernel="linear", gamma = 2^(-1:1), cost = 2^(2:4)) 
  #summary(mytunedsvm)
  z2<-predict(svmmodel, test)
  Acc<-table(test[,9],z2)
  Accuracy_kernel_linear[l]<- sum(diag(Acc))/sum(Acc)*100 
}
a<-sum(Accuracy_kernel_linear)/10
a
#---------------------------

#for polynomial kernel


Accuracy_kernel_poly<-numeric(10)
for (l in 1:10) {
  smp_size1 <- floor(0.90 * nrow(pima_norm))
  train_ind <- sample(seq_len(nrow(pima_norm)), size = smp_size1)
  train1 <- pima_norm[train_ind, ]
  test1 <- pima_norm[-train_ind, ]
  train1$V9 <- as.factor(train$V9)
  test1$V9 <- as.factor(test$V9)
  svmmodel <- svm(V9 ~ ., data = train1,kernel="polynomial", cost = 4, gamma = 0.5)
  #mytunedsvm <- tune.svm(V9 ~ ., data = train,kernel="polynomial", gamma = 2^(-1:3), cost = 2^(2:4)) 
  #summary(mytunedsvm)
  z2<-predict(svmmodel, test1)
  Acc<-table(test1[,9],z2)
  Accuracy_kernel_poly[l]<- sum(diag(Acc))/sum(Acc)*100 
}
b<-sum(Accuracy_kernel_poly)/10
b
#for radial basis kernel

Accuracy_kernel_rad<-numeric(10)
for (l in 1:10) {
  smp_size2 <- floor(0.90 * nrow(pima_norm))
  train_ind <- sample(seq_len(nrow(pima_norm)), size = smp_size2)
  train2 <- pima_norm[train_ind, ]
  test2 <- pima_norm[-train_ind, ]
  train$V9 <- as.factor(train$V9)
  test$V9 <- as.factor(test$V9)
  svmmodel <- svm(V9 ~ ., data = train2,kernel="radial", cost = 4, gamma = 0.5)
  #mytunedsvm <- tune.svm(V9 ~ ., data = train,kernel="radial", gamma = 2^(-1:3), cost = 2^(2:4)) 
  #summary(mytunedsvm)
  z2<-predict(svmmodel, test2)
  Acc<-table(test2[,9],z2)
  Accuracy_kernel_rad[l]<- sum(diag(Acc))/sum(Acc)*100 
}
c<-sum(Accuracy_kernel_rad)/10
c
#-------------------------------------------
#for sigmoid basis kernel

Accuracy_kernel_sig<-numeric(10)
for (l in 1:10) {
  smp_size3 <- floor(0.90 * nrow(pima_norm))
  train_ind <- sample(seq_len(nrow(pima_norm)), size = smp_size3)
  train3 <- pima_norm[train_ind, ]
  test3 <- pima_norm[-train_ind, ]
  train$V9 <- as.factor(train$V9)
  test$V9 <- as.factor(test$V9)
  svmmodel <- svm(V9 ~ ., data = train3,kernel="sigmoid", cost = 4, gamma = 0.5)
  #mytunedsvm <- tune.svm(V9 ~ ., data = train,kernel="radial", gamma = 2^(-1:3), cost = 2^(2:4)) 
  #summary(mytunedsvm)
  z2<-predict(svmmodel, test3)
  Acc<-table(test3[,9],z2)
  Accuracy_kernel_sig[l]<- sum(diag(Acc))/sum(Acc)*100 
}
d<-sum(Accuracy_kernel_sig)/10
d
Av<-c(a,b,c,d)
Kernel<-c("linear","polynomial","radial basis","sigmoid")
Table_SVM<-data.frame(Kernel,Av)
Table_SVM
