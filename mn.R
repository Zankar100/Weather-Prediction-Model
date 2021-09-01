require(nnet)
library(nnet)
library(caret)

rain <- read.csv('C:/Users/Zankar/Desktop/ML/weather/weatherAUS.csv',header=TRUE,sep=',',stringsAsFactors = F)

rain$Date <- as.Date(as.character(rain$Date))
rain$Date <- strftime(rain$Date,"%m")

weather <- na.omit(rain)

weather$Location <- as.numeric(as.factor(weather$Location))
weather$WindGustDir <- as.numeric(as.factor(weather$WindGustDir))
weather$WindDir9am <- as.numeric(as.factor(weather$WindDir9am))
weather$WindDir3pm <- as.numeric(as.factor(weather$WindDir3pm))
weather$RainTomorrow <-ifelse(weather$RainTomorrow=='Yes',1,0)
weather$RainToday <-ifelse(weather$RainToday=='Yes',1,0)
weather$Date <- as.numeric(as.factor(weather$Date))

multinom_model<-multinom("RainTomorrow~.",data=weather)
multinom_model

multinom_pred<-predict(multinom_model,weather[,-23],type="class")
mncfm<-caret::confusionMatrix(table(weather[,23],multinom_pred))
mncfm

##############Correlation

# devtools::install_github("laresbernardo/lares")
library(lares)
corr_cross(weather, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 15 # display top 10 couples of variables (by correlation coefficient)
)

library(ggcorrplot)
p<-ggcorrplot(cor(weather))
p + theme(axis.text.x = element_text(angle = 90))
