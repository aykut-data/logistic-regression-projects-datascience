purchase_data1<-read.csv("Purchase Data 1.csv",header=T)
purchase_data2<-read.csv("Purchase Data 2.csv",header=T)
response_data<-read.csv("Response Data.csv",header=T)
library(dplyr)

masterdata<-full_join(response_data,purchase_data1,by="Custid")
masterdata<-full_join(masterdata,purchase_data2,by="Custid")

str(masterdata)
masterdata$Age<-as.factor(masterdata$Age)
masterdata$Gender<-as.factor(masterdata$Gender)
masterdata$MS<-as.factor(masterdata$MS)
masterdata$Pre_Month<-as.factor(masterdata$Pre_Month)
str(masterdata)
head(masterdata)

library(ggplot2)

response<-data.frame(table(masterdata$Response))
names(response)<-c("Response","Count")
response$Perc<-paste(round((response$Count/sum(response$Count))*100,2),"%",sep = "")

ggplot(response, aes(x = "", y = Count, fill = Response)) +
  geom_col(color = "black") +
  geom_text(aes(label = Perc),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#BE2A3E", "#3C8D53"))+
  theme_void()

ggplot(masterdata, aes(x=BillAmt_1, y=as.factor(Response),fill=as.factor(Response))) +
  ggtitle("Box plot of Bill Amount1 by Response")+
  geom_boxplot()+ coord_flip()+theme_classic()

ggplot(masterdata, aes(x=BillAmt_2, y=as.factor(Response),fill=as.factor(Response))) + 
  ggtitle("Box plot of Bill Amount2 by Response")+
  geom_boxplot()+ coord_flip()+theme_classic()

ggplot(masterdata, aes(x=BillAmt_3, y=as.factor(Response),fill=as.factor(Response))) + 
  ggtitle("Box plot of Bill Amount3 by Response")+
  geom_boxplot()+ coord_flip()+theme_classic()

library(caret)
masterdata$Response<-as.factor(masterdata$Response)
index<-createDataPartition(masterdata$Response,p=0.7,list=FALSE)
traindata<-masterdata[index,]
testdata<-masterdata[-index,]
traindata
testdata

blr_model<-glm(Response~Age+Gender+MS+Pre_Month+N_Products+N_Service+BillAmt_1+BillAmt_2+BillAmt_3,
               data=traindata,family = binomial)
summary(blr_model)

#Rerun the model after removing the insignificant variables
blr_model<-glm(Response~N_Products+N_Service+BillAmt_1+BillAmt_2,
               data=traindata,family = binomial)
summary(blr_model)

library(car)
vif(blr_model)

library(ROCR)
traindata$predprob<-fitted(blr_model)
predtrain<-prediction(traindata$predprob,traindata$Response)
perftrain<-performance(predtrain,"tpr","fpr")
plot(perftrain)
abline(0,1)

testdata$predprob<-predict(blr_model,testdata,type='response')
predtest<-prediction(testdata$predprob,testdata$Response)
perftest<-performance(predtest,"tpr","fpr")
plot(perftest)
abline(0,1)

auctrain<-performance(predtrain,"auc")
auctrain@y.values 

auctest<-performance(predtest,"auc")
auctest@y.values

sstrain <- performance(predtrain, "sens", "spec")
best_threshold <- sstrain@alpha.values[[1]][which.max(sstrain@x.values[[1]]+sstrain@y.values[[1]])]
best_threshold

traindata$predY<-as.factor(ifelse(traindata$predprob>best_threshold,1,0))
confusionMatrix(traindata$predY,traindata$Response,positive="1")

testdata$predY<-as.factor(ifelse(testdata$predprob>best_threshold,1,0))
confusionMatrix(testdata$predY,testdata$Response,positive="1")

library(caret)
kfolds<-trainControl(method="cv",number=4)
kmodel<-train(Response~N_Products+N_Service+BillAmt_1+BillAmt_2,data=traindata,method="glm",
              family=binomial,trControl=kfolds)

kmodel

traindata$pred <- kmodel$finalModel$fitted.values
traindata$predY <-as.factor(ifelse(traindata$pred > best_threshold , 1,0))
confusionMatrix(traindata$predY,traindata$Response,positive="1")
