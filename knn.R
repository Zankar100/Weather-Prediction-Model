library(class)
library(ROCR)

euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

knn_predict2 <- function(test_data, train_data, k_value, labelcol){
  print(k_value)
  print(labelcol)
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,-c(labelcol)], train_data[j,-c(labelcol)]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[labelcol]]))
      #print(i,j,as.character(train_data[j,][[labelcol]])))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    # print(k_value)
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    head(eu[1:k_value,])
    tbl.sm.df<-table(eu$eu_char)
    cl_label<-  names(tbl.sm.df)[[as.integer(which.max(tbl.sm.df))]]
    
    pred <- c(pred, cl_label)
  }
  return(pred) #return pred vector
}

accuracy <- function(test_data,labelcol,predcol){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,labelcol] == test_data[i,predcol]){ 
      correct = correct+1
    }
  }
  accu = (correct/nrow(test_data)) * 100  
  return(accu)
}

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

catweather<-weather[,c(1,2,8,10,11,18,19,22,23)]

knn.df<-catweather
knn.ids <- sample(1:nrow(knn.df),20000,replace = F)
knn.df <- knn.df[knn.ids,]
table(knn.df$RainTomorrow)
labelcol <- 9  
predictioncol<-labelcol+1

set.seed(43)
n<-nrow(knn.df)
knn.df<- knn.df[sample(n),]

train.df <- knn.df[1:as.integer(0.7*n),]
test.df <- knn.df[as.integer(0.7*n +1):n,]

table(train.df$RainTomorrow)
table(test.df$RainTomorrow)

K<-7 # number of neighbors to determine the class

predictions <- knn_predict2(test.df, train.df, K,labelcol) #calling knn_predict()

test.df[,predictioncol] <- predictions #
print(paste('accuracy=',accuracy(test.df,labelcol,predictioncol)))

train.df <- knn.df[1:as.integer(0.7*n),]
test.df <- knn.df[as.integer(0.7*n +1):n,]

cl<-factor(train.df[,9])
knnPred<-knn(train.df[,-c(labelcol)],test.df[,-c(labelcol)], cl, k = 7, prob=TRUE)
prob<-attr(knnPred,"prob")
kp<-prediction(prob,test.df[,labelcol])
AUC<-performance(kp,"auc")
#perf <- performance(pred,"tpr","fpr")
#plot(perf,colorize=TRUE)

pred_knn<-performance(kp,"tpr","fpr")

plot(pred_knn,avg="threshold",colorize=T,lwd=3,main="ROCR knn")
text(0.8,0.2,paste("AUC=",round(AUC@y.values[[1]],4),sep=''))
TBL<-table(test.df[,labelcol],knnPred)
print(paste("accuracy=",sum(diag(TBL))/sum(TBL)))
