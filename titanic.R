#Titanic Project
#Predict the "Survival" variable
getwd()
setwd("C:/Users/Everett/Desktop/R/titanic")
str(train)
train$Name <- as.factor(train$Name)
train$Sex <- as.factor(train$Sex)
train$Ticket <- as.factor(train$Ticket)
train$Survived #Vector
table(train$Survived) #Number who died/survived

prop.table(table(train$Survived)) #38% survived

test$Survived <- rep(0,418) #Prediction that everyone died, running it on the test set 418 times

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyalldie.csv", row.names = FALSE) #We are right about 62% of the time, pretty bad


summary(train$Sex) #Majority are male passengers
prop.table(table(train$Sex, train$Survived), 1) #% survived based on gender

test$Survived <- 0 #Set whole column to died
test$Survived[test$Sex == 'female'] <- 1 #Then set any place that has Female as survived

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "morefemaleslived.csv", row.names = FALSE) #Improved to 76%

summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1 #Adding the child variable, since we are focusing on women and children
#0 is adult, 1 is child

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)

aggregate(Survived ~ Child + Sex, data = train, FUN = length)

aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x) / length(x)})
#75% of female adults and 69% of female children survived. 16% of male adults and 39% of male children survived.

train$Fare2 <- '30+' #Separating ticket fares into price range categories
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x) / length(x)})
#If you paid more for a ticket/were upper class, more chance of survival
#However, males still don't do as well compared to females

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "ticketpriceandclass.csv", row.names = FALSE) #Improved to about 78%

#Decision Trees!
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
#method = 'anova' would be used for continuous variables like age to run decimal values, 'class' is for 1 and 0 
plot(fit)
text(fit)

install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "decisiontree.csv", row.names = FALSE) #Now at 78.5%

#Removing split limits from the tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit) #oh god what have I done
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "decisiontreegonewrong.csv", row.names = FALSE) #Dropped to 74%
#We overfit the model
