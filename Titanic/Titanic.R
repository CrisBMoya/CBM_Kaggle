rm(list=ls())

library(ggplot2)
library(Amelia)
library(nnet)
library(caTools)
library(Boruta)

#Seed - Good practices
set.seed(101)

#Reading data
trainDF=read.table("C:/Users/Tobal/Google Drive/R/Kaggle/Titanic/train.csv", sep=",", header=TRUE,
                   na.strings=c("NA",""," ","<NA>"))
str(trainDF)
#Looking for NAs
missmap(trainDF, legend=FALSE)
#Exploring
#Gender and age: "Women and child first". Age should be an important feature for prediction.
ggplot(data=na.omit(trainDF), aes(y=Survived, x=Age)) + geom_col(aes(colour=factor(Sex)))

#Income? - No difference
ggplot(data=trainDF, aes(y=Survived, x=Pclass)) + geom_col()

#Does the cabin number matters? Are cabins numbers in some sort of order? - Nope
ggplot(data=na.omit(trainDF), aes(y=Survived, x=Cabin)) + geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Feature Engineering
#Age
#Young people dont travel alone, unlikely.
AgeNA=trainDF[is.na(trainDF$Age),]
AgeNA=AgeNA[!AgeNA$SibSp==0,]

#Mean age of people traveling with someone.
mean(na.omit(trainDF[!trainDF$SibSp==0,]$Age))
sd(na.omit(trainDF[!trainDF$SibSp==0,]$Age))

#Train data for age, predicting age
trainAge=na.omit(trainDF[!trainDF$SibSp==0,])
#Prediction
model1=multinom(Age ~ Sex + Survived + SibSp + Parch, data=trainAge)
pred1=predict(model1, type="class", newdata=AgeNA)
#Seems legit
mean(as.numeric(pred1))
sd(as.numeric(pred1))
hist(x=as.numeric(pred1))

#Inputting into original DF
AgeNA$Age=pred1
trainDF[trainDF$PassengerId %in% AgeNA$PassengerId,]$Age=AgeNA$Age

####
####
#Same as before but with older people
AgeNAOlder=trainDF[is.na(trainDF$Age),]
AgeNAOlder=AgeNAOlder[AgeNAOlder$SibSp==0,]

#Train
trainAgeOlder=na.omit(trainDF[trainDF$SibSp==0,])

#Prediction
model2=multinom(Age ~ Sex + Survived + SibSp + Parch, data=trainAgeOlder)
pred2=predict(model2, type="class", newdata=AgeNAOlder)
#Checking, fair enough, maybe
mean(as.numeric(pred2))
sd(as.numeric(pred2))
hist(x=as.numeric(pred2))

#Inputting into original DF
AgeNAOlder$Age=pred2
trainDF[trainDF$PassengerId %in% AgeNAOlder$PassengerId,]$Age=AgeNAOlder$Age

##Mismap
missmap(trainDF, legend=FALSE)

######
######
#Train and test sets
sepdf=sample.split(trainDF$Survived, SplitRatio = 0.7)
trainSetNew=subset(trainDF, sepdf==TRUE)
testSetNew=subset(trainDF, sepdf==FALSE)

#Prediction
str(trainSetNew)
model3=glm(Survived ~ Age + Sex + Fare + Pclass, data=trainSetNew, family=binomial(logit))
pred3=predict(model3, type="response", newdata=testSetNew)

#Comparison
cMat=table(testSetNew$Survived, pred3 > 0.5)
(cMat[1,1]+cMat[2,2])/sum(cMat)
#Recorded Value: 0.8246269

#Applying into real test df
testDF=read.table("C:/Users/Tobal/Google Drive/R/Kaggle/Titanic/test.csv", sep=",", header=TRUE)
str(testDF)

model4=glm(Survived ~ Age + Sex, data=trainDF, family=binomial(logit))
pred4=predict(model4, type="response", newdata=testDF)
pred4[is.na(pred4)]=0
pred4=ifelse(pred4 > 0.5, 1, 0)

Solution=data.frame(PassengerId=testDF$PassengerId, Survived=pred4)
write.csv(x=Solution, file="C:/Users/Tobal/Google Drive/R/Kaggle/Titanic/Solution.csv", 
          row.names=FALSE, quote=FALSE)

#############
