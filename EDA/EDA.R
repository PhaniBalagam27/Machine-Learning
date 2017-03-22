pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header = FALSE)
str(pima)
summary(pima)
par(mfrow = c(3,3))
colnames <- dimnames(pima)[[2]]
for(i in 1:8){
  hist(pima[,i], main = colnames[i], freq = FALSE, col="blue", border = "white")
}

BarplotF<- function(x){
  colName<-colnames(x)
  for (i in 1:8){
    barplot(x[,i],main=colName[i])
  }}

BarplotF(pima)


corrwithclass <- function(x){
  correlationwithclass <- numeric(8)
  for (i in 1:8) {
    correlationwithclass[i] <- cor(x[,i],x[,9])
  }
  print(correlationwithclass)
  print(max(correlationwithclass))
}
corrwithclass(pima)

corr_var<- function(x){
  d<-x[,1:8]
  dmat<-abs(cor(d))
  for (i in 1:8) {
    for (j in 1:8) {
      if (i>=j) dmat[i,j] <- 0
    }}
  print(dmat)
  max(dmat)
  which(dmat == max(dmat), arr.ind = TRUE)
}
corr_var(pima)
