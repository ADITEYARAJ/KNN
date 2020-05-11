glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
glass1 <- glass[sample(nrow(glass)),]#To shuffle the data by row for getting unbaised dsts for train and test set.
View(glass1)
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
glass2 <- as.data.frame(lapply(glass1[,1:9],norm))
View(glass2)
train_glass <- glass2[1:160,]
test_glass <- glass2[161:214,]
train_lables <- glass1[1:160,10]
test_lables <- glass1[161:214,10]
train_acc <- NULL
test_acc <- NULL
library(class)
for (i in seq(1,100,1)) {
  train_pred <- knn(train=train_glass,test=train_glass,cl=train_lables,k=i)
  train_acc <- c(train_acc,mean(train_pred==train_lables))
  test_pred <- knn(train=train_glass,test=test_glass,cl=train_lables,k=i)
  test_acc <- c(test_acc,mean(test_pred==test_lables))
  
}
df_glass <- data.frame(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,100,1))
df_glass
ggplot(df_glass,aes(x=neigh))+
  geom_line(aes(y=train_acc,color="train_acc"))+
geom_line(aes(y=test_acc,color="test_acc"))+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
dist <- -test_acc+train_acc
dist  
min(dist)#giving 9 as the minimum distance between test_acc and train_acc
glass_pred <- knn(train = train_glass,test = test_glass,cl=train_lables,k=9)
glass_pred
test_lables
acc <- test_lables==glass_pred
acc
table(acc)
