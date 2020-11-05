mydata<-read.csv(file.choose())
View(mydata)
mydata$RI
table(mydata$RI)
#table or proportions with more informative labels
round(prop.table(table(mydata$RI))*100,digits = 1)
#summarize three numeric features
summary(mydata[c("RI_mean","NA_mean","MG_mean")])
summary(mydata)
#create normalization function 
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
View()
#test normalization function - result should be identical
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))
#normalize the mydata
mydata_n<-as.data.frame(lapply(mydata[1:10],normalize))
#confirm that normalization worked
summary(mydata_n$Si)
## create training and test data 
mydata_train<-mydata_n[1:107, ]
mydata_test<-mydata_n[108:214, ]
##create labels for training & test data 
mydata_train_labels<-mydata[1:107,1]
mydata_test_labels<-mydata[108:214,1]
#load the "class" library 
install.packages("class")
library(class)
mydata_test_pred<-knn(train = mydata_train,test = mydata_test,cl=mydata_train_labels,k=11)

mydata_test_pred<-knn(train=mydata_train,test = mydata_test,cl=mydata_train_labels,k=10)

mydata_test_pred
mydata_test_labels

table(mydata_test_pred,mydata_test_labels)
##Evaluating model performance 
#load the "gmodels"library
install.packages("gmodels")
library(gmodels)
##Create the cross tabulation of predicted vs actual
CrossTable(x=mydata_test_labels,y=mydata_test_pred,prop.chisq = FALSE)



wss=NULL
twss<-NULL
for (i  in 1:20 ) {twss<-c(twss,knn(mydata_n,i)$tot.withinss)}
twss
plot(1:20,twss,type="o")
k_3<-knn(mydata_n,3)  

wss=(nrow(mydata_n)-1)*sum(apply(mydata_n,2,var))

plot(2:15,twss,type = "b",xlab = "No.of animals",ylab = "frequency")
title(sub = "knn")
