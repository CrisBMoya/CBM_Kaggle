setwd("C:/Users/Tobal/Google Drive/Otros/Machine Learning/Udemy/R/Logistic Regression")

library(dplyr)
library(Amelia)
library(ggplot2)
library(caTools)
#Task: predict wether te income is below or equal to 50k, or above it.
#Load data.
df=read.csv("adult_sal.csv", header=TRUE, na.strings="?")
df=select(df, -X)

head(df, 20)
str(df)
#Finding missing values
findLevels=sapply(df, unique)

#NA is refered as "?"
missmap(df, legend=FALSE)
df=na.omit(df)
missmap(df, legend=FALSE)

#Exploration of data.
colnames(df)
#Income by education. There's a difference between those with education level below 9.
#From 10 to 16 is more common to earn more than 50k
ggplot(df, aes(x=education_num, y=income_num)) + geom_col(aes(colour=factor(income_num))) +
  scale_x_continuous(breaks=1:16)

#Income by Age. People between 37-50 tend to earn more.
ggplot(df, aes(x=age, y=income_num)) + geom_col(aes(colour=factor(income_num))) +
  scale_x_continuous(breaks=seq(min(df$age),max(df$age),by=5))

#Income by type of employer.
ggplot(df, aes(x=type_employer, y=income_num)) + geom_col(aes(colour=factor(income_num))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Income by type of employer.
ggplot(df, aes(x=race, y=income)) + geom_col(aes(colour=factor(income))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###Grouping excesive levels
sapply(findLevels, length)

#type_employer, education, marital, race, country

#type_emplyer
findLevels$type_employer
fun1=function(x){
if(x == "State-gov" | x == "Local-gov" | x == "Federal-gov"){
  return("Gov")
  }else if(x == "Self-emp-not-inc" | x=="Self-emp-inc" | x=="Private"){
    return("Private")
  }else{
    return("Other")}
}

df$type_employer=sapply(df$type_employer, fun1)
unique(df$type_employer)

#Education
findLevels$education
fun2=function(x){
  if(x=="1st-4th" | x=="5th-6th" | x=="7th-8th"){
    return("Primary")
  }else if(x=="9th" | x=="10th" | x=="11th" | x=="12th"){
    return("Secondary")  
  }else{
    return("University")  
    }
}

df$education=sapply(df$education, fun2)
unique(df$education)

#Marital
findLevels$marital
fun3=function(x){
  if(x=="Never-Married" | x=="Divorced" | x=="Separated" | x=="Widowed"){
    return("Not married")
  }else{
    return("Married")  
    }
}

df$marital=sapply(df$marital, fun3)
unique(df$marital)

#Country
findLevels$country
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
    if (ctry %in% Asia){
      return('Asia')
    }else if (ctry %in% North.America){
      return('North.America')
    }else if (ctry %in% Europe){
      return('Europe')
    }else if (ctry %in% Latin.and.South.America){
      return('Latin.and.South.America')
    }else{
      return('Other')      
    }
}

df$country <- sapply(df$country,group_country)
unique(df$country)
##DONE
str(df)
#Turn character columns into factors
df[sapply(df, is.character)]=lapply(df[sapply(df, is.character)], as.factor)


#Train and test sets
sepdf=sample.split(df$income, SplitRatio = 0.7)
trainSet=subset(df, sepdf=TRUE)
testSet=subset(df, sepdf=FALSE)


#Model
colnames(trainSet)
model1=glm(income ~ . -race -education_num -relationship, data=trainSet, family=binomial(logit))

#Predict
pred1=predict(model1, newdata=testSet, type="response")

#Confussion Matrix
cMat=table(testSet$income, pred1 >0.5)

(cMat[1,1]+cMat[2,2])/sum(cMat)
#Recorded value: 0.8217956
