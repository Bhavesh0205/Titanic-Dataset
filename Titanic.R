#####################Predicton of survival (overall)#######################
#setting working Directory
setwd("C:/Users/BHAVESH/Desktop/R_study/project/Titanic")
#loading Training dataset
train<-read.csv("train.csv")
str(train)

#checking for number of survivers
table(train$Survived)

#checking ratio/proportion of survivers to deaths
prop.table(table(train$Survived))

#loading test data
test<-read.csv("test.csv")

#adding new column to test data
test$Survived <- rep(0,418)

#loading iinto submit
submit <- data.frame(PasengerID =test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "theyallperish",,row.names = FALSE)


###########Prediction of Survival based on Gender and age##########
#Total Number of male and female
summary(train$Sex)

#Proportion of survival of each
prop.table(table(train$Sex,train$Survived),1)

#Predicting more women survived compared to men
test$Survived[test$Sex=="female"]<-1

#segregating children
train$child<-0
train$child[test$Age < 18]<-1

###segregation of childen and adult surviors

#survivors
Survivors<-aggregate(Survived ~child +Sex,data =train,FUN = sum)
#Total count
total<-aggregate(Survived ~ child+Sex,data =train,FUN = length)
#Ratio of each
ratio<-aggregate(Survived ~ child+Sex,data = train, FUN = function(x)
+{sum(x)/length(x)})
ratio

##segregation based on class
train$Fare2[train$Fare > 30]<-"30+"
train$Fare2[train$Fare >=20 & train$Fare <=30]<- "20-30"
train$Fare2[train$Fare >=10 & train$Fare <=20]<- "10-20"
train$Fare2[train$Fare <=10] <- "<10"

#final ratio
aggregate(Survived ~ Fare2 + Pclass + Sex,data = train, FUN = function(x)
+ {sum(x/length(x))})

#Predicting the results in test data
test$Survived<-0
test$Survived[test$Sex== "female" ]<-1
test$Survived[test$Sex == "female" & test$Pclass =="3" & test$Fare >=20]<-0

nrow(test[test$Survived ==1,])

##################Using Decision Trees#################

library(rpart)
fit<-rpart(Survived ~ Pclass +Sex + Age + SibSp + Parch + Fare + Embarked,data=train, method = "class")
plot(fit)     
text(fit)
install.packages('rattle')
install.packages('RGtk2')
install.packages('rpart.plot')
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#making Prediction
prediction<-predict(fit,test,type ="class")
submit<-data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit,file ="decisiontree.csv",row.names = F)
