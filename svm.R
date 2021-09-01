library(plotly)
library(e1071)
library(lattice)
library(caret)
library(pROC)

###################################
heart <- read.csv('C:/Users/Zankar/Desktop/ML/heart.csv',header=TRUE,sep=',',stringsAsFactors = F)
head(heart)
heart$target<-ifelse(heart$target>0,1,0)
dim(heart)
table(heart$target)

heart$target <- factor(heart$target)
svm_model <- svm(target~.,data=heart,probability=TRUE)
#
###################################

rain <- read.csv('C:/Users/Zankar/Desktop/ML/weather/weatherAUS.csv',header=TRUE,sep=',',stringsAsFactors = F)
str(rain)
sapply(rain, function(x) sum(is.na(x)))
rainML <- rain
rainML$Date <- as.Date(as.character(rain$Date))
rainML$Date <- strftime(rainML$Date,"%m")       
str(rainML)

weather <- na.omit(rainML)
sapply(weather,function(x) sum(is.na(x)))

names(weather)
corweather<-cor(weather[,1:22])

apply(weather,2,table)       

weatherallnum <- weather
weatherallnum$Location <- as.numeric(as.factor(weather$Location))
weatherallnum$WindGustDir <- as.numeric(as.factor(weather$WindGustDir))
weatherallnum$WindDir9am <- as.numeric(as.factor(weather$WindDir9am))
weatherallnum$WindDir3pm <- as.numeric(as.factor(weather$WindDir3pm))
weatherallnum$RainTomorrow <-ifelse(weather$RainTomorrow=='Yes',1,0)
weatherallnum$RainToday <-ifelse(weather$RainToday=='Yes',1,0)
weatherallnum$Date <- as.numeric(as.factor(weatherallnum$Date))

weather <- weatherallnum

weather$RainTomorrow <- factor(weather$RainTomorrow)
svm_model <- svm(RainTomorrow~.,data=weather,probability=TRUE)
svm_model

svmpredict <- predict(svm_model,weather[,-c(23)],probability = TRUE)
probs <- attr(svmpredict,"prob")[,"1"]
predclass <- ifelse(probs>0.5,1,0)
table(weather$RainTomorrow==predclass)
(cfmx <- caret::confusionMatrix(table(weather$RainTomorrow,predclass)))

set.seed(43)
tstidx <- sample(1:nrow(weather),0.30*nrow(weather),replace=F)
trdata <- weather[-tstidx,]
tsdata <- weather[tstidx,]

svm_train_model <- svm(RainTomorrow~.,data=trdata,probability=TRUE)
svm_train_predict <- predict(svm_train_model,trdata[,-c(23)],probability = TRUE)
train_probs <- attr(svm_train_predict,"prob")[,"1"]
train_predclass <- ifelse(train_probs>0.5,1,0)
tr_tbl <- table(trdata$RainTomorrow,train_predclass)
(tr_cfmx <- caret::confusionMatrix(tr_tbl))

cdf <- data.frame(all=cfmx$byClass,tr=tr_cfmx$byClass)
cdf

svm_tst_predict <- predict(svm_train_model,tsdata[,-c(23)],probability = TRUE)
tst_probs <- attr(svm_tst_predict,"prob")[,"1"]
tst_predclass <- ifelse(tst_probs>0.5,1,0)
tst_tbl <- table(tsdata$RainTomorrow,tst_predclass)
(tst_cfmx <- caret::confusionMatrix(tst_tbl))

cdf <- cbind(cdf,ts=tst_cfmx$byClass)
cdf

#library(car)
#library(usdm)
#vif_model<-vif(trdata)

#vif_model

vif_col <- c(1,2,5,6,7,8,9,10,11,12,13,18,19,22,23)
vif_trdata <- trdata[,vif_col]
vif_tsdata <- tsdata[,vif_col]
head(vif_trdata)

svm_vif_tr <- svm(RainTomorrow~.,data=vif_trdata,probability=TRUE)
svm_vif_tr
svm_vif_tr_predict <- predict(svm_vif_tr,vif_trdata[,-c(15)],probability=TRUE)
vif_train_probs <- attr(svm_vif_tr_predict,"prob")[,"1"]
vif_train_predclass <- ifelse(vif_train_probs>0.5,1,0)
vif_tr_tbl <- table(vif_trdata$RainTomorrow,vif_train_predclass)
(vif_tr_cfmx <- caret::confusionMatrix(vif_tr_tbl))

(cdf<-cbind(cdf,vif_tr=vif_tr_cfmx$byClass))

svm_vif_ts_predict <- predict(svm_vif_tr,vif_tsdata[,-c(15)],probability = TRUE)
svm_vif_ts_probs <- attr(svm_vif_ts_predict,"prob")[,"1"]
vif_ts_predclass <- ifelse(svm_vif_ts_probs>0.5,1,0)
vif_ts_tbl <- table(vif_tsdata$RainTomorrow,vif_ts_predclass)
(vif_ts_cfmx <- caret::confusionMatrix(vif_ts_tbl))

(cdf<-cbind(cdf,vif_ts=vif_ts_cfmx$byClass))#

#svm_roc_obj <- roc(vif_tsdata[,c(15)],svm_vif_ts_probs)
#svm_auc <- auc(svm_roc_obj)

svm_roc_obj <- roc(tsdata[,c(23)],tst_probs)
svm_auc <- auc(svm_roc_obj)

plot.new()
plot.window(xlim = c(1,0),ylim = c(0,1),xaxs='i',yaxs='i')
axis(1,las=1)
axis(2,las=1)
abline(1,-1,col="black",lwd=0.5)
box()
title(main="ROC",xlab = "Specificity",ylab = "Sensitivity")
lines(svm_roc_obj,col="black",las=2)
text(x=0.2,y=0.2,paste("SVM_AUC=",round(svm_auc,6),sep = ''))

