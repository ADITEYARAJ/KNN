library(ggplot2)
zoo <- read.csv(file.choose())
View(zoo)
table(zoo$animal.name)
table(zoo$type)
zoo1 <- zoo[,-1]
View(zoo1)
zoo1$type <- factor(x=zoo1$type,levels = c("1","2","3","4","5","6","7"),labels = c("A","B","C","D","E","F","G"))
zoo1_train <- zoo1[1:80,-17]
zoo1_test <- zoo1[81:101,-17]
train_labels <- zoo1[1:80,17]
test_labels <- zoo1[81:101,17]
train_acc <- NULL
test_acc <- NULL
library(class)
for (i in seq(1,50,1)) {
  train_predict <- knn(train = zoo1_train,test = zoo1_train,cl=train_labels,k=i)
  train_acc <- c(train_acc,mean(train_predict==train_labels))
  test_predict <- knn(train = zoo1_train,test = zoo1_test,cl=train_labels,k=i)
  test_acc <- c(test_acc,mean(test_predict==test_labels))
  
}
acc_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,50,1)))
acc_df
ggplot(acc_df,aes(x=neigh))+geom_line(aes(y=train_acc,color="train_acc"),lwd=1.5)+geom_line(aes(y=test_acc[],color="test_acc"),lwd=1.5)+
scale_fill_manual(" ",breaks =c("train_acc","test_acc"),values=c("train_acc"="green","test_acc"="red"))
zoo_predict <- knn(train = zoo1_train,test=zoo1_test,cl=train_labels,k=3)
a <- test_labels==zoo_predict
a
table(a)
prop.table(a)
1-9/50
