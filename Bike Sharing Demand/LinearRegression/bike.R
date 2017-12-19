#URL: "https://www.kaggle.com/c/bike-sharing-demand/data"
rm(list=ls())

library(ggplot2)

setwd("C:/Users/Tobal/Google Drive/Otros/Machine Learning/Udemy/R/Linear Regression/Bike Sharing Demand")

bikeData=read.csv("train.csv", header=TRUE)


#Only 1 Month
Month1=bikeData[bikeData$datetime[grep(x=bikeData$datetime, pattern="2011-01-01")],]
ggplot(data=Month1, aes(x=datetime, y=count)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=Month1, aes(x=datetime, y=count)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Dissect
BikeMonth=as.integer(format(as.Date(bikeData$datetime), format="%m"))
BikeDay=as.integer(format(as.Date(bikeData$datetime), format="%d"))
BikeHour=(format(strptime(bikeData$datetime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
bikeData$Month=BikeMonth
bikeData$Day=BikeDay
bikeData$Hour=BikeHour

#Uso por hora por dia. Alta variacion.
ggplot(data=bikeData, aes(x=bikeData$Hour, y=bikeData$count)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Uso por hora por dia. Seasons. Alta variacion.
BikeSeasons=bikeData[bikeData$season==1,]
ggplot(data=BikeSeasons, aes(x=BikeSeasons$Hour, y=BikeSeasons$count)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Uso por hora, separado por temporada
ggplot(data=bikeData, aes(x=bikeData$Hour, y=bikeData$count, shape=factor(bikeData$season))) +
  geom_boxplot(aes(colour=factor(bikeData$season)))

#Uso por hora, separado por holiday
ggplot(data=bikeData, aes(x=bikeData$Hour, y=bikeData$count, shape=factor(bikeData$holiday))) +
  geom_boxplot(aes(colour=factor(bikeData$holiday)))

#Uso por hora, separado por clima
ggplot(data=bikeData, aes(x=bikeData$Hour, y=bikeData$count, shape=factor(bikeData$weather))) +
  geom_boxplot(aes(colour=factor(bikeData$weather)))

#Uso por hora, separado por working day
ggplot(data=bikeData, aes(x=bikeData$Hour, y=bikeData$count, shape=factor(bikeData$workingday))) +
  geom_boxplot(aes(colour=factor(bikeData$workingday)))


####
#Analyze data per hour.
#Data is lineal only in the same hours. The tendencies are similar only in the same hours, not
#between them. There's no clear linearity between the passing of hours.
unique(bikeData$Hour)
bikeDataHour=bikeData[bikeData$Hour == "01:00:00",]
bikeDataHour$datetime=NULL
bikeDataHour$Hour=NULL
head(bikeDataHour)

corrplot(cor(bikeDataHour))
#Holiday makes no difference
ggplot(data=bikeDataHour, aes(x=as.factor(holiday), y=count)) + geom_boxplot()
mean(bikeDataHour[bikeDataHour$holiday==0,]$count)
mean(bikeDataHour[bikeDataHour$holiday==1,]$count)
sd(bikeDataHour[bikeDataHour$holiday==0,]$count)
sd(bikeDataHour[bikeDataHour$holiday==1,]$count)

#Working day makes difference. They rent more on non-working days.
ggplot(data=bikeDataHour, aes(x=as.factor(workingday), y=count)) + geom_boxplot()

#Weather makes difference only when is rainning or worst.
ggplot(data=bikeDataHour, aes(x=as.factor(weather), y=count)) + geom_boxplot()

#Weather makes difference only when is rainning or worst.
ggplot(data=bikeDataHour, aes(x=as.factor(temp), y=count)) + geom_boxplot()

#Rented bikes don't change ant the same hour across days.
ggplot(data=bikeDataHour, aes(x=Day, y=count)) + geom_point()

####
####
#Aparentemente workingday es importante, al igual que wheater y temp
BikeTEMP=bikeData[bikeData$workingday==0 & bikeData$weather==1,]
ggplot(data=BikeTEMP, aes(x=Hour, y=count))  + geom_point(data=BikeTEMP,aes(colour=BikeTEMP$temp)) +
  scale_color_gradient(low="blue", high="red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Linearity respond in working days between:
#05:00:00-13:00:00, rising.
#14:00:00-04:00:00, falling.
#Season dosen't seems to be a relevant factor.

#####################################
#First time window: 05:00:00-11:00:00
TimeWindow1=unique(bikeData$Hour)[6:14]
TimeWindow1

BikeTrainW1=bikeData[bikeData$workingday==0,]
BikeTrainW1=BikeTrainW1[BikeTrainW1$Hour %in% TimeWindow1,]

#Train and test sets. Aprox 80-20%
daysSet=16:19
trainSetW1=BikeTrainW1[!BikeTrainW1$Day %in% daysSet,]
testSetW1=BikeTrainW1[BikeTrainW1$Day %in% daysSet,]

(nrow(BikeTrainW1)*80)/100
nrow(trainSetW1)

#Modeling
colnames(trainSetW1)
ModelW1=lm(count ~ weather + Hour +  temp + atemp + windspeed, data=trainSetW1)
summary(ModelW1)

#Predict
PredW1=predict(ModelW1, newdata=testSetW1)

#Bind predicted values with original ones
EvalModel=cbind(PredW1, testSetW1$count)
colnames(EvalModel)=c("Predicted","Actual")
EvalModel=as.data.frame(EvalModel)

#Getting R2 of the predicted values
SSE= sum((EvalModel$Predicted - EvalModel$Actual)^2)
SST= sum((mean(BikeTrainW1$count) - EvalModel$Actual)^2)
R2=1- SSE/SST
R2
#Recorded Value: 0.7124573


#####################################
#Third time window: 12:00:00-14:00:00
TimeWindow3=c(unique(bikeData$Hour)[15:24],unique(bikeData$Hour)[1:5])
TimeWindow3

BikeTrainW3=bikeData[bikeData$workingday==0,]
BikeTrainW3=BikeTrainW3[BikeTrainW3$Hour %in% TimeWindow3,]

#Train and test sets. Aprox 80-20%
daysSet=16:19
trainSetW3=BikeTrainW3[!BikeTrainW3$Day %in% daysSet,]
testSetW3=BikeTrainW3[BikeTrainW3$Day %in% daysSet,]
(nrow(BikeTrainW3)*80)/100
nrow(trainSetW3)

#Modeling
colnames(trainSetW3)
ModelW3=lm(count ~ weather + Hour  + atemp + windspeed, data=trainSetW3)
summary(ModelW3)

#Predict
PredW3=predict(ModelW3, newdata=testSetW3)

#Bind predicted values with original ones
EvalModel=cbind(PredW3, testSetW3$count)
colnames(EvalModel)=c("Predicted","Actual")
EvalModel=as.data.frame(EvalModel)

#Getting R2 of the predicted values
SSE= sum((EvalModel$Predicted - EvalModel$Actual)^2)
SST= sum((mean(BikeTrainW3$count) - EvalModel$Actual)^2)
R2=1- SSE/SST
R2
#Recorded Value: 0.7334369

#########
###

#More Eploxration of data

#Data IS linear, for range 05:00:00 to 13:00:00
ggplot(data=BikeTrainW1, aes(x=Hour, y=count)) + geom_point()

TimeStampOrder=c("14:00:00","15:00:00","16:00:00","17:00:00","18:00:00","19:00:00","20:00:00",
                 "21:00:00","22:00:00","23:00:00","00:00:00","01:00:00","02:00:00","03:00:00",
                 "04:00:00")
BikeTrainW3$NewHour=factor(BikeTrainW3$Hour, levels=TimeStampOrder)

#Data IS linear, for range 14:00:00 to 04:00:00
ggplot(data=BikeTrainW3, aes(x=NewHour, y=count)) + geom_point() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data=BikeTrainW1, aes(x=Day, y=count)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
colnames(BikeTrainW1)



###########
#    _____       _   _   _                        _ _   _______               _   _               
#   |  __ \     | | | | (_)                 /\   | | | |__   __|             | | | |              
#   | |__) |   _| |_| |_ _ _ __   __ _     /  \  | | |    | | ___   __ _  ___| |_| |__   ___ _ __ 
#   |  ___/ | | | __| __| | '_ \ / _` |   / /\ \ | | |    | |/ _ \ / _` |/ _ \ __| '_ \ / _ \ '__|
#   | |   | |_| | |_| |_| | | | | (_| |  / ____ \| | |    | | (_) | (_| |  __/ |_| | | |  __/ |   
#   |_|    \__,_|\__|\__|_|_| |_|\__, | /_/    \_\_|_|    |_|\___/ \__, |\___|\__|_| |_|\___|_|   
#                                 __/ |                             __/ |                         
#                                |___/                             |___/                          
rm(list=ls())
library(ggplot2)

setwd("C:/Users/Tobal/Google Drive/Otros/Machine Learning/Udemy/R/Linear Regression/Bike Sharing Demand")

bikeData=read.csv("train.csv", header=TRUE)


#Dissect
BikeMonth=as.integer(format(as.Date(bikeData$datetime), format="%m"))
BikeDay=as.integer(format(as.Date(bikeData$datetime), format="%d"))
BikeHour=(format(strptime(bikeData$datetime, format="%Y-%m-%d %H:%M:%S"), format="%H"))
bikeData$Month=BikeMonth
bikeData$Day=BikeDay
bikeData$Hour=BikeHour

#####################################
#First time window: 05:00:00-11:00:00
TimeWindow1=unique(bikeData$Hour)[6:14]
TimeWindow1

BikeTrainW1=bikeData[bikeData$workingday==0,]
BikeTrainW1=BikeTrainW1[BikeTrainW1$Hour %in% TimeWindow1,]

#Train and test sets. Aprox 80-20%
daysSet=16:19
trainSetW1=BikeTrainW1[!BikeTrainW1$Day %in% daysSet,]
testSetW1=BikeTrainW1[BikeTrainW1$Day %in% daysSet,]

(nrow(BikeTrainW1)*80)/100
nrow(trainSetW1)

#Modeling
colnames(trainSetW1)
ModelW1=lm(count ~ weather + Hour +  temp + atemp + windspeed, data=trainSetW1)
summary(ModelW1)

#Predict
PredW1=predict(ModelW1, newdata=testSetW1)

#Bind predicted values with original ones
EvalModel=cbind(PredW1, testSetW1$count)
colnames(EvalModel)=c("Predicted","Actual")
EvalModel=as.data.frame(EvalModel)

#Getting R2 of the predicted values
SSE= sum((EvalModel$Predicted - EvalModel$Actual)^2)
SST= sum((mean(BikeTrainW1$count) - EvalModel$Actual)^2)
R2=1- SSE/SST
R2
#Recorded Value: 0.7124573


#####################################
#Third time window: 12:00:00-14:00:00
TimeWindow3=c(unique(bikeData$Hour)[15:24],unique(bikeData$Hour)[1:5])
TimeWindow3

BikeTrainW3=bikeData[bikeData$workingday==0,]
BikeTrainW3=BikeTrainW3[BikeTrainW3$Hour %in% TimeWindow3,]

#Train and test sets. Aprox 80-20%
daysSet=16:19
trainSetW3=BikeTrainW3[!BikeTrainW3$Day %in% daysSet,]
testSetW3=BikeTrainW3[BikeTrainW3$Day %in% daysSet,]
(nrow(BikeTrainW3)*80)/100
nrow(trainSetW3)

#Modeling
colnames(trainSetW3)
ModelW3=lm(count ~ weather + Hour  + atemp + windspeed, data=trainSetW3)
summary(ModelW3)

#Predict
PredW3=predict(ModelW3, newdata=testSetW3)

#Bind predicted values with original ones
EvalModel=cbind(PredW3, testSetW3$count)
colnames(EvalModel)=c("Predicted","Actual")
EvalModel=as.data.frame(EvalModel)

#Getting R2 of the predicted values
SSE= sum((EvalModel$Predicted - EvalModel$Actual)^2)
SST= sum((mean(BikeTrainW3$count) - EvalModel$Actual)^2)
R2=1- SSE/SST
R2
#Recorded Value: 0.7334369



###
#After a lot of trying I look at the solutions on Udemy course and note that the
#teacher also realice that using linear models with this data is not the best option,
#based on that, I decide to move on.
#Using linear regression I was able to predict values with about 70% accuracy, but only 
#for data without working values=1. The shape of this data was clearly not lineal,
#and with peaks on two ocasions (count vs hours plot). The models didn't perfom well on this
#particular data (less than 40% accuracy).
#I'll be beack with more knowledge to tackle this problem.