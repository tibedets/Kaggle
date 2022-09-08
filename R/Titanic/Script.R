### Set Working Directory ###
setwd("F:/Kaggle/R")
### Read Data Into R ###
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

### Create column for separation ###
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

###Cleaning Test set ###
titanic.test$Survived <- NA

### Combine Data Files ###
titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

###Clean Missing Values Age ###
age.median <- median(titanic.full$Age, na.rm = TRUE)

titanic.full[is.na(titanic.full$Age), "Fare"] <- age.median

###Clean Missing Values Fare ###
fare.median <- median(titanic.full$Fare, na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

### Categorical Casting ###
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

### Split Data Set into test and train ###
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

titanic.model<- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

Survived <- predict(titanic.model, newdata = titanic.test)

