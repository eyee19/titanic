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

#Feature engineering
train$Name[1]

train <- read.csv("train.csv") #rereading dataset in for this part of the exercise, as column number does not match
test <- read.csv("test.csv")

test$Survived <- NA
combi <- rbind(train,test) #combining training and testing sets

combi$Name <- as.character(combi$Name) #setting factor back to string
combi$Name[1]
strsplit(combi$Name[1], split = '[,.]')[[1]][2] #[[1]] removes the [[1]], [2] chooses the 2nd index which is "Mr"
combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title) #removing extra spaces
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle' #combining madame and madmoiselle
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1 #adding 1 is for their own existence

combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,] #splitting data back into train and test

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featureengineering.csv", row.names = FALSE) #Now at 79.4%

#Random forests, bagging
summary(combi$Age) #20% are NAs

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age),], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),]) #now we don't have NAs

summary(combi)
summary(combi$Embarked) #Embarked has 2 blank values
which(combi$Embarked == '') #This tells us where those blank values are
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked) #The blank values are now Southampton
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE) #Replacing the one NA with the median fare

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2) #reducing family ID manually to fit threshold of random forest

install.packages("randomForest")
library(randomForest)
set.seed(415)

train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data = train, importance = TRUE, ntree = 2000)
varImpPlot(fit)
#Accuracy graph tests to see how worse the model performs without each variable, so high decrease in accuracy is normal for easily predictable variables
#Gini one measures how pure the nodes are at the end of the trees, high score means variable was important
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE) #Didn't do better than decision tree

install.packages("party")
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "improvedforest.csv", row.names = FALSE) #81.3% improvement


