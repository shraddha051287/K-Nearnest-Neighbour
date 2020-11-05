mydata<-read.csv(file.choose())
View(mydata)
mydata$animal.name
#table or proportions with more information labels 
round(prop.table(table(mydata$animal.name))*100,digits = 1)
#summarize numeric features
summary(mydata)
##create normalization function
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
## test normalization function - result should be identical 
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
#Normalize the mydata 
mydata_n<-as.data.frame(lapply(mydata[2:18],normalize))
#confirm that noralization worked
summary(mydata_n$legs)
#create training and test data
mydata_train<-mydata_n[1:50, ]
mydata_test<-mydata_n[51:101, ]
#create lables for training  and test data
mydata_train_labels<-mydata[1:50,1] 
mydata_test_labels<-mydata[51:101,1]
#load the "class" library
install.packages("class")
library(class)
mydata_test_pred<-knn(train = mydata_train,test = mydata_test,cl=mydata_train_labels,k=21)
##Evaluating model performance 
#load the "gmodels" library
install.packages("gmodels")
library(gmodels)
#create the cross tabulation of predicted vs actual
CrossTable(x=mydata_test_labels,y=mydata_test_pred,prop.chisq = FALSE)
wss=NULL
twss<-NULL
for (i  in 1:18 ) {twss<-c(twss,knn(mydata_n,i)$tot.withinss)}
twss
plot(1:20,twss,type="o")
k_3<-knn(mydata_n,3)  
wss=(nrow(mydata_n)-1)*sum(apply(mydata_n,2,var))

plot(1:18,twss,type = "b",xlab = "No.of animals",ylab = "frequency")
title(sub = "knn")
