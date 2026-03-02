# Check current working directory
getwd()

# Set working directory (use / or \\ for Windows)
setwd("C:/Users/***/***/***")


library(ggplot2)
library(dplyr)
library(ROCR)
library(car)
library(caret)

masterdata<-read.csv("ICU Mortality.csv",header = T)
head(masterdata)

masterdata$STA<-as.factor(masterdata$STA)
STA<-data.frame(table(masterdata$STA))
names(STA)<-c("Vital Status","N")
STA$Percentage<-round((STA$N/sum(STA$N))*100,2)
STA$`Vital Status`<-ifelse(STA$`Vital Status`==1,"Died","Lived")

ggplot(masterdata, aes(x=STA, y=AGE,fill=STA)) +
  ggtitle("Box plot of Age by Vital Status")+
  geom_boxplot()+theme_classic()

ggplot(masterdata, aes(x=STA, y=SYS,fill=STA)) +
  ggtitle("Box plot of Systolic Blood Pressure by Vital Status")+
  geom_boxplot()+theme_classic()

ggplot(masterdata, aes(x=STA, y=HRA,fill=STA)) +
  ggtitle("Box plot of Heart Rate by Vital Status")+
  geom_boxplot()+theme_classic()

gender<-as.data.frame.matrix(table(masterdata$SEX,masterdata$STA))
names(gender)<-c("Lived","Died")
gender$N<-gender$Lived+gender$Died
gender$Gender<-ifelse(row.names(gender)==0,"Male","Female")
gender$Death_rate<-round((gender$Died/gender$N)*100,2)
gender<-gender %>% select(Gender,N,Death_rate)
gender

typad<-as.data.frame.matrix(table(masterdata$TYP,masterdata$STA))
names(typad)<-c("Lived","Died")
typad$N<-typad$Lived+typad$Died
typad$`Admission Type`<-ifelse(row.names(typad)==0,"Elective","Emergency")
typad$Death_rate<-round((typad$Died/typad$N)*100,2)
typad<-typad %>% select(`Admission Type`,N,Death_rate)
typad

locon<-as.data.frame.matrix(table(masterdata$LOC,masterdata$STA))
names(locon)<-c("Lived","Died")
locon$N<-locon$Lived+locon$Died
locon$`Level of Consciousness`<-ifelse(row.names(locon)==0,"No Coma or Stupor",ifelse(row.names(locon)==1,"Deep Stupor","Coma"))
locon$Death_rate<-round((locon$Died/locon$N)*100,2)
locon<-locon %>% select(`Level of Consciousness`,N,Death_rate)
locon

masterdata$LOC<-ifelse(masterdata$LOC>0,1,0)

blr_model<-glm(STA~.-SR.NO-ID-RACE,data=masterdata,family = binomial)
summary(blr_model)

vif(blr_model)

masterdata$predprob<-fitted(blr_model)
pred<-prediction(masterdata$predprob,masterdata$STA)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)

auc<-performance(pred,"auc")
auc@y.values 

ss <- performance(pred, "sens", "spec")
best_threshold <- ss@alpha.values[[1]][which.max(ss@x.values[[1]]+ss@y.values[[1]])]
paste("Best Threshold is :",round(best_threshold,2))

masterdata$predY<-as.factor(ifelse(masterdata$predprob>best_threshold,1,0))
confusionMatrix(masterdata$predY,masterdata$STA,positive="1")

#The sensitivity and specificity values using optimum threshold are approximately 65% and 89% 
#and the accuracy is approximately 84% indicating good model performance.