require(randomForest)

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

weather$RainTomorrow<-as.factor(weather$RainTomorrow)
class(weather$RainTomorrow)

weather.rf<-randomForest(RainTomorrow~.,data=weather)
importance(weather.rf)

varImpPlot(weather.rf)

require(caret)
varImp(weather.rf)

