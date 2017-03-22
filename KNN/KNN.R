pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE)k<-c(3, 5, 7, 9, 11) 
pima$V9 <- as.factor(pima$V9)
#install.packages("e1071")
library("e1071") 
require(class)
k<-c(3, 5, 7, 9, 11) 
pima$V9 <- as.factor(pima$V9)

require(class)
Average_acc_knn<-numeric(5)
Accuracy_knn<-numeric(10)
for (i in 1:5){
  for(j in 1:10){
    smp_size <- floor(0.90 * nrow(pima))
    train_ind <- sample(seq_len(nrow(pima)), size = smp_size)
    trainset <- pima[train_ind, ]
    train_target <- pima[train_ind,9 ]
    testset <- pima[-train_ind, ]
    test_target <- pima[-train_ind, 9]  
    m1<-knn(train=trainset,test=testset,cl=train_target,k[i])
    q<-table(test_target,m1)
    #print(q)
    Accuracy_knn[j]<- sum(diag(q))/sum(q)*100
    #print(Accuracy_knn[j])
  }
  Average_acc_knn[i] <-sum(Accuracy_knn)/10
}



Average_acuuracy_of_10_experiments<-Average_acc_knn
Table_SVM<-data.frame(k,Average_acuuracy_of_10_experiments)
Table_SVM
