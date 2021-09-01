library(e1071)
library(caret)
library(magrittr)
library(dplyr)

isOverfitting<-function(trcfm,tstcfm)
{
  # if the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  tracc=trcfm$overall['Accuracy']
  tstacc=tstcfm$overall['Accuracy']
  tracc > 2*tstacc 
}

isUnderfitting<-function(trcfm)
{
  # if the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  tracc=trcfm$overall['Accuracy']
  0.5 >  tracc
}

rain <- read.csv('C:/Users/Zankar/Desktop/ML/weather/weatherAUS.csv',header=TRUE,sep=',',stringsAsFactors = F)
sapply(rain, function(x) sum(is.na(x)))
rainML <- rain
rainML$Date <- as.Date(as.character(rain$Date))
rainML$Date <- strftime(rainML$Date,"%m")       
str(rainML)

weather <- na.omit(rainML)
sapply(weather,function(x) sum(is.na(x)))

weatherallnum <- weather
weatherallnum$Location <- as.numeric(as.factor(weather$Location))
weatherallnum$WindGustDir <- as.numeric(as.factor(weather$WindGustDir))
weatherallnum$WindDir9am <- as.numeric(as.factor(weather$WindDir9am))
weatherallnum$WindDir3pm <- as.numeric(as.factor(weather$WindDir3pm))
weatherallnum$RainTomorrow <-ifelse(weather$RainTomorrow=='Yes',1,0)
weatherallnum$RainToday <-ifelse(weather$RainToday=='Yes',1,0)
weatherallnum$Date <- as.numeric(as.factor(weatherallnum$Date))


################################
#create balanced df 
table(weatherallnum$RainTomorrow)
only1 <- weatherallnum %>% filter(RainTomorrow > 0)
table(only1$RainTomorrow)
only0 <- weatherallnum %>% filter(RainTomorrow == 0)
table(only0$RainTomorrow)
set.seed(43)
only0ids <- sample(1:nrow(only0),nrow(only1),replace=F)
only0 <- only0[only0ids,]
balweather <- rbind(only0,only1)
table(balweather$RainTomorrow)

################################

apply(weatherallnum,2,table)
catweather<-weatherallnum[,c(1,2,8,10,11,18,19,22,23)]

head(catweather)
dim(catweather)
table(catweather$RainTomorrow)
sum(table(catweather$RainTomorrow))

TBL<-table(catweather$RainTomorrow)
CL<-names(TBL)
print(paste("P(c=",CL[1],")=",TBL[[1]]/sum(TBL),sep=""))
print(paste("P(c=",CL[2],")=",TBL[[2]]/sum(TBL),sep=""))

nb_likelihood<-function(df,label,class,feature,val)
{nrow(df[df[[feature]]==val&df[[label]]==class,])/nrow(df[df[[label]]==class,])}

#
nbtr.model1<-naiveBayes(RainTomorrow~.,data=catweather)
nbtr.model1
nbtr.pred<-predict(nbtr.model1,catweather[,-c(9)],type='raw')
nbtr.class<-unlist(apply(round(nbtr.pred),1,which.max))-1
nbtr.tbl<-table(catweather[[9]], nbtr.class)
cfm<-caret::confusionMatrix(nbtr.trtbl)
cfm

#
set.seed(43)
trdidx<-sample(1:nrow(catweather),0.7*nrow(catweather),replace=F)
trcatweather<-catweather[trdidx,]
tstcatweather<-catweather[-trdidx,]

table(catweather$RainTomorrow)

dim(trcatweather)
table(trcatweather$RainTomorrow)
dim(tstcatweather)
table(tstcatweather$RainTomorrow)

nbtr.model<-naiveBayes(RainTomorrow~.,data=trcatweather)

nbtr.trpred<-predict(nbtr.model,trcatweather[,-c(9)],type='raw')
nbtr.trclass<-unlist(apply(round(nbtr.trpred),1,which.max))-1
nbtr.trtbl<-table(trcatweather[[9]], nbtr.trclass)
tr.cfm<-caret::confusionMatrix(nbtr.trtbl)
tr.cfm

nbtr.tspred<-predict(nbtr.model,tstcatweather[,-c(9)],type='raw')

roc.nbtr.tspred<-nbtr.tspred[,2]
nbtr.tsclass<-unlist(apply(round(nbtr.tspred),1,which.max))-1
nbtr.tstbl<-table(tstcatweather[[9]], nbtr.tsclass)
tst.cfm<-caret::confusionMatrix(nbtr.tstbl)
tst.cfm

ifelse(isUnderfitting(tr.cfm),"model is deficient",paste("There is no underfitting", "model is an effective learner@ [",tr.cfm$overall['Accuracy'],"] accuracy"))
ifelse(isOverfitting(tr.cfm,tst.cfm),"model is overfitting -- too complex",
       paste(" There is no overfitting, model is an effective learner@ [",tr.cfm$overall['Accuracy'],"] training accuracy vs testing accuracy=[",tst.cfm$overall['Accuracy'],']'))

trcatweathertarget_0<-trcatweather[trcatweather$RainTomorrow==0,]
trcatweathertarget_1<-trcatweather[trcatweather$RainTomorrow==1,]

tstcatweathertarget_0<-tstcatweather[tstcatweather$RainTomorrow==0,]
tstcatweathertarget_1<-tstcatweather[tstcatweather$RainTomorrow==1,]

c0freq<-nrow(trcatweathertarget_0)
c1freq<-nrow(trcatweathertarget_1)
p0<-c0freq/(c0freq+c1freq)
p1<-c1freq/(c0freq+c1freq)
c(p0,p1)

classProbabilities<-
  function(dsetc1,dsetc2,prop,val,c1prob,c2prob)
  {
    
    propdsetc1=dsetc1[dsetc1[prop]==val,]
    propdsetc2=dsetc2[dsetc2[prop]==val,]
    c(nrow(propdsetc1)/nrow(dsetc1),nrow(propdsetc2)/nrow(dsetc1))
  }

trcatweathertarget_0[1,]

classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'Date',9,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'Location',16,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'WindGustDir',13,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'WindDir9am',9,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'WindDir3pm',13,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'Cloud9am',1,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'Cloud3pm',1,p0,p1)
classProbabilities(trcatweathertarget_0,trcatweathertarget_1,'RainToday',1,p0,p1)

#

####################### for all values

set.seed(43)
trdidxall<-sample(1:nrow(weatherallnum),0.7*nrow(weatherallnum),replace=F)
trweather<-weatherallnum[trdidxall,]
tstweather<-weatherallnum[-trdidxall,]

table(weatherallnum$RainTomorrow)

dim(trweather)
table(trweather$RainTomorrow)
dim(tstweather)
table(tstweather$RainTomorrow)

nbtr.model2<-naiveBayes(RainTomorrow~.,data=trweather)

nbtr.trpred2<-predict(nbtr.model2,trweather[,-c(23)],type='raw')
nbtr.trclass2<-unlist(apply(round(nbtr.trpred2),1,which.max))-1
nbtr.trtbl2<-table(trweather[[23]], nbtr.trclass2)
tr.cfm2<-caret::confusionMatrix(nbtr.trtbl2)
tr.cfm2

nbtr.tspred2<-predict(nbtr.model2,tstweather[,-c(23)],type='raw')

roc.nbtr.tspred2<-nbtr.tspred2[,2]
nbtr.tsclass2<-unlist(apply(round(nbtr.tspred2),1,which.max))-1
nbtr.tstbl2<-table(tstweather[[23]], nbtr.tsclass2)
tst.cfm2<-caret::confusionMatrix(nbtr.tstbl2)
tst.cfm2

##accuracy increases for all values cont. and cat.
#######################



#######################
#for bal all values

set.seed(43)
trdidxbal<-sample(1:nrow(balweather),0.7*nrow(balweather),replace=F)
trweatherbal<-balweather[trdidxbal,]
tstweatherbal<-balweather[-trdidxbal,]

table(balweather$RainTomorrow)

dim(trweatherbal)
table(trweatherbal$RainTomorrow)
dim(tstweatherbal)
table(tstweatherbal$RainTomorrow)

nbtr.modelbal<-naiveBayes(RainTomorrow~.,data=trweatherbal)

nbtr.trpredbal<-predict(nbtr.modelbal,trweatherbal[,-c(23)],type='raw')
nbtr.trclassbal<-unlist(apply(round(nbtr.trpredbal),1,which.max))-1
nbtr.trtblbal<-table(trweatherbal[[23]], nbtr.trclassbal)
tr.cfmbal<-caret::confusionMatrix(nbtr.trtblbal)
tr.cfmbal

#######################
