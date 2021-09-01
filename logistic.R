if(!require(plotly))install.packages('plotly')
library(plotly)
if(!require(stringr))install.packages('stringr')
library(stringr)
if(!require(car))install.packages('car')
library(car)
if(!require(e1071))install.packages('e1071')
library(e1071)
require(e1071)
if(!require(caret))install.packages('caret')
library(caret)
require(caret)
if(!require(pROC))install.packages('pROC')
library(pROC)
if(!require(ROCR))install.packages('ROCR')
library(ROCR)

rain <- read.csv('C:/Users/Zankar/Desktop/ML/weather/weatherAUS.csv',header=TRUE,sep=',',stringsAsFactors = T)
str(rain)
sapply(rain, function(x) sum(is.na(x)))
rain2 <- rain
rain2$Date <- as.Date(as.character(rain$Date))
rain2$Date <- strftime(rain2$Date,"%m")       
str(rain2)

weather <- na.omit(rain2)
sapply(weather,function(x) sum(is.na(x)))
str(weather)
names(weather)
corweather<-cor(weather[,1:22])

apply(weather,2,table)       
apply(weather,2,class)

weatherallnum <- weather

weatherallnum$Location <- as.numeric(as.factor(weather$Location))
weatherallnum$WindGustDir <- as.numeric(as.factor(weather$WindGustDir))
weatherallnum$WindDir9am <- as.numeric(as.factor(weather$WindDir9am))
weatherallnum$WindDir3pm <- as.numeric(as.factor(weather$WindDir3pm))
weatherallnum$RainTomorrow <-ifelse(weather$RainTomorrow=='Yes',1,0)
weatherallnum$RainToday <-ifelse(weather$RainToday=='Yes',1,0)
weatherallnum$Date <- as.numeric(as.factor(weatherallnum$Date))

apply(weatherallnum, 2, class)
apply(weatherallnum,2,table)

corweather<-cor(weatherallnum[,1:22])
corweather

################################
#create balanced df 
table(weatherallnum$RainTomorrow)
only1 <- weatherallnum %>% filter(RainTomorrow > 0)
table(only1$RainTomorrow)
only0 <- weatherallnum %>% filter(RainTomorrow == 0)
table(only0$RainTomorrow)
set.seed(43)
only0ids <- sample(1:nrow(only0),nrow(only1),replace=F)
only1ids <- sample(1:nrow(only1),0.7*nrow(only1),replace = F)
only0 <- only0[only0ids,]
only1 <- only1[only1ids,]
balweather <- rbind(only0,only1)
table(balweather$RainTomorrow)

################################

isConstant<-function(x) length(names(table(x)))<2
apply(weatherallnum,2,isConstant)
head(weatherallnum)

classLabels<-table(weatherallnum$RainTomorrow)
print(classLabels)
names(classLabels)
length(names(classLabels))
ifelse(length(names(classLabels))==2,"binary classification", "multi-class classification")


#p <- pairs(weatherallnum)

glm_model<-glm(RainTomorrow~.,data=weatherallnum, family='binomial')
glm_model
summary_glm_model<-summary(glm_model)
coef_summary_glm_model<-coef(summary_glm_model)
dim(coef_summary_glm_model)
coef_summary_glm_model[[1]]
row.names(coef_summary_glm_model)

coef_summary_glm_model
coef_summary_glm_model[,4]<0.05
row_names<-row.names(coef_summary_glm_model[coef_summary_glm_model[,4]<0.05,])
row_names

summary_glm_model$aic
summary_glm_model$null.deviance
summary_glm_model$deviance
ifelse(summary_glm_model$deviance<summary_glm_model$null.deviance,"model has improved","model has not helped")

formularhs <- paste(row.names(coef_summary_glm_model[coef_summary_glm_model[,4]<0.05,]),collapse='+')
formularhs
formularhs <- str_extract(formularhs, "Date+(?s)(.*$)")

formulastr<-paste('RainTomorrow~',formularhs,sep='')
formulastr

#paste(row.names(coef_summary_glm_model[coef_summary_glm_model[,4]<0.05,]),collapse='+')

model2<-glm(formulastr,data=weatherallnum,family='binomial')
summ_model2<-summary(model2)
coef_summ_model2<-coef(summ_model2)
coef_summ_model2

summ_model2$aic
summ_model2$null.deviance
summ_model2$deviance


vif_model<-vif(model2)

vif_model
vif_model[vif_model>4]
names(vif_model)
nl<-names(vif_model[vif_model<4])
(newformulastr<-paste('RainTomorrow~',paste(nl,collapse='+')))
newformulastr

Y<-weatherallnum[[23]]
head(Y)
actual_distribution<-table(Y)

newmodel<-glm(newformulastr,data=weatherallnum,family='binomial')
summary(newmodel)
summnewmodel<-summary(newmodel)
(summnewmodel$aic)
(summnewmodel$deviance)
(summnewmodel$null.deviance)

(p_values<-coef(summnewmodel)[,4])

table(p_values<0.005)

predYprob<-predict(newmodel,weatherallnum[,1:22],type='response')
predY<-ifelse(predYprob<0.5,0,1)
table(predY)
table(weatherallnum[[23]])
table(weatherallnum[[23]],predY)

install.packages('caret')
install.packages("e1071")

cfm<-caret::confusionMatrix(table(weatherallnum[[23]],predY))
cfm

ImpMeasure<-data.frame(varImp(newmodel))
ImpMeasure$Vars<-row.names(ImpMeasure)
z<-rownames(ImpMeasure[order(-ImpMeasure$Overall),][1:10,])

z

(newformulastr<-paste('RainTomorrow~',paste(z,collapse='+')))
newformulastr


set.seed(43)
tstidx<-sample(1:nrow(weatherallnum),0.30*nrow(weatherallnum),replace=F)
trdata<-weatherallnum[-tstidx,]
tsdata<-weatherallnum[tstidx,]

glm.trmodel<-glm(newformulastr,data=trdata,family='binomial')
summary(glm.trmodel)
predtr<-predict(glm.trmodel,trdata[,1:22],type='response')

predtrclass<-ifelse(predtr<0.5,0,1)
table(trdata[[23]])
table(predtrclass)
levels(factor(predtrclass))
levels(factor(trdata[[23]]))
length(predtrclass)==length(trdata[[23]])
(trcfm<-caret::confusionMatrix(table(trdata[[23]],predtrclass)))

predts<-predict(glm.trmodel,tsdata[,1:22],type='response')
predtsclass<-ifelse(predts<0.5,0,1)                            
table(predtsclass)
table(tsdata[[23]])
table(tsdata[[23]],predtsclass)
tscfm<-caret::confusionMatrix(table(tsdata[[23]],predtsclass))
tscfm

(precision <- tscfm$byClass['Pos Pred Value'])    
(recall <- tscfm$byClass['Sensitivity'])
(f_measure <- 2 * ((precision * recall) / (precision + recall))) #geometric mean instead of arithmatic mean

###############################
#for balanced dataset
set.seed(43)
tstidx1<-sample(1:nrow(balweather),0.30*nrow(balweather),replace=F)
trdata1<-balweather[-tstidx1,]
tsdata1<-balweather[tstidx1,]


tst.model2<-glm(newformulastr,data=trdata1,family='binomial')
summary(tst.model2)
tr.tst.pred<-predict(tst.model2,trdata1[,1:22],type='response')
tr.pred.class<-ifelse(tr.tst.pred<0.5,0,1)
tr.pred.table<-table(trdata1[[23]],tr.pred.class)
tr.pred.table
(tr.pred.cfm<-confusionMatrix(tr.pred.table))
(precision <- tr.pred.cfm$byClass['Pos Pred Value'])    
(recall <- tr.pred.cfm$byClass['Sensitivity'])
(f_measure <- 2 * ((precision * recall) / (precision + recall)))

tst.pred2<-predict(tst.model2,tsdata1[,1:22],type='response')
tst.pred2.class<-ifelse(tst.pred2<0.5,0,1)
tst.pred2.table<-table(tsdata1[[23]],tst.pred2.class)
tst.pred2.table
(tst.pred2.cfm<-confusionMatrix(tst.pred2.table))                       
(accuracy<-tst.pred2.cfm$overall['Accuracy'])
(precision <- tst.pred2.cfm$byClass['Pos Pred Value'])
(recall <- tst.pred2.cfm$byClass['Sensitivity'])
(f_measure <- 2 * ((precision * recall) / (precision + recall)))

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

par(pty="s")
glmROC <- roc(tsdata[[23]]~ tst.pred2.class,plot=TRUE,
              print.auc=TRUE,col="green",lwd =4,
              legacy.axes=TRUE,main="ROC Curves")


getMetrics<-function(actual_class,predicted_response)
{
  X=list()
  if ( require(ROCR) ) {
    auc_1=prediction(predicted_response,actual_class)
    prf=performance(auc_1, measure="tpr",x.measure="fpr")
    slot_fp=slot(auc_1,"fp")
    slot_tp=slot(auc_1,"tp")
    
    fpr=unlist(slot_fp)/unlist(slot(auc_1,"n.neg"))
    tpr=unlist(slot_tp)/unlist(slot(auc_1,"n.pos"))
    
    auc<-performance(auc_1,"auc")
    AUC<-auc@y.values[[1]]
    X=list(fpr=fpr,tpr=tpr,auc=AUC)
  }
  X
}

L<-getMetrics(tsdata[[23]],tst.pred2)
plot(L$fpr,L$tpr,main=" ROC Plot tpr vs fpr")
print(paste("AUC=",L$auc,sep=''))
text(paste("AUC=",L$auc,sep=''),x=0.6,y=0.30)
