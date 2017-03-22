library("e1071") 
Accuracy_in_percentage<-numeric(10)
for (l in 1:10) {
  smp_size <- floor(0.90 * nrow(pima))
  train_ind <- sample(seq_len(nrow(pima)), size = smp_size)
  train <- pima[train_ind, ]
  test <- pima[-train_ind, ]
  train$V9 <- as.factor(train$V9)
  test$V9 <- as.factor(test$V9)
  nb <- naiveBayes(V9~., data = train)
  z<-predict(nb, test)
  z
  Acc<-table(test[,9],z)
  Accuracy_in_percentage[l] <- sum(diag(Acc))/sum(Acc)*100 
}
Experiments<-c(1:10)
Table_NAIVE <- data.frame(Experiments,Accuracy_in_percentage)
Table_NAIVE
Average<-sum(Accuracy_in_percentage)/10
Average
