require(rpart)
require(rattle)
require(rpart.plot)
require(reshape2)
require(kernlab)
require(plyr)
require(caret)
require(Hmisc)
require(pROC)
require(e1071)
require(randomForest)
require(party)
require(klaR)


source("lib/functions.r")
source("lib/dataloader.r")

train <- train.raw
test <- test.raw

##### simple decision tree model
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit);
printcp(fit);
plotcp(fit);

Prediction <- predict(fit, test, type = "class");
result <- data.frame(PassengerId = test$PassengerId, Survived = as.numeric(levels(Prediction))[Prediction]);
write.csv(result, file = "decisiontree.csv", row.names = FALSE);

##### tree pruning
pfit <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(pfit, uniform=TRUE, main="Pruned Classification Tree")
printcp(pfit)

Prediction <- predict(pfit, test, type = "class");
result <- data.frame(PassengerId = test$PassengerId, Survived = as.numeric(levels(Prediction))[Prediction]);
write.csv(result, file = "prunedtree.csv", row.names = FALSE);


########## Complex models with feature selection

## Add new feature Title
test$Survived <- NA
comb <- rbind(train, test)
split1 = colsplit(string=as.character(comb$Name), pattern=", ", names=c("Part1","Part2"))
split2 = colsplit(string=as.character(split1$Part2), pattern="\\.", names=c("Part1","Part2"))
comb$Title <- split2$Part1

comb$Title[comb$Title %in% c('Mlle')] <- 'Miss'
comb$Title[comb$Title %in% c('Mme','Ms')] <- 'Mrs'
comb$Title[comb$Title %in% c('Capt', 'Col', 'Don', 'Major')] <- 'Sir'
comb$Title[comb$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

## Add new feature FamSize = SibSp+Parch+1
comb$FamSize <- comb$SibSp + comb$Parch + 1

## Add new feature FamilyID
split1 <- colsplit(string=as.character(comb$Name), pattern=",.", names=c("Part1","Part2"))
comb$Surname <- split1$Part1
comb$FamilyID <- paste(as.character(comb$FamSize), comb$Surname, sep="")
comb$FamilyID[comb$FamSize <= 1] <- 'Single'

id_list <- data.frame(table(comb$FamilyID))
id_list <- id_list[id_list$Freq <= 1,]
comb$FamilyID[comb$FamilyID %in% id_list$Var1] <- 'Single'
comb$FamilyID <- factor(comb$FamilyID)

## Fill in missing age values using a regression tree
age.train = comb[!is.na(comb$Age),]
age.fit <- rpart(Age ~ Title + Sex + SibSp + Parch + Fare + Embarked + FamSize, 
                 data=age.train, method="anova")
summary(age.fit)
comb$Age[is.na(comb$Age)] <- predict(age.fit, comb[is.na(comb$Age),])

## Add new feature AgeCat
comb$AgeCat[comb$Age <= 10.00] <-"child"
comb$AgeCat[comb$Age > 10.00 & comb$Age <=20.00] <-"teen"
comb$AgeCat[comb$Age > 20.00 & comb$Age <= 35.00] <-"young"
comb$AgeCat[comb$Age > 35.00 & comb$Age <= 50.00] <-"adult"
comb$AgeCat[comb$Age > 50.00 & comb$Age <= 60.00] <-"elder"
comb$AgeCat[comb$Age > 60.00] <-"old"

## Add new feature AgeCatSex
comb$AgeCatSex <- paste(comb$AgeCat, comb$Sex, sep="_")


## Fill in missing Fare values
comb$Fare[is.na(comb$Fare)] <- median(comb$Fare, na.rm=TRUE)

## Fill in missing Embarked values with most probable value
comb$Embarked[is.na(comb$Embarked)] <- 'S'
# At this point there are no missing values


## Add new feature FareCat - categorical split of Fare
data.frame(table(comb$Pclass, comb$Fare[order(as.numeric(comb$Fare))])) 
## FareCat
comb$FareCat <- comb$Fare
comb$FareCat[comb$Fare == 0.00] <- 0
comb$FareCat[comb$Fare > 0.00 & comb$Fare <= 7.00] <- 1
comb$FareCat[comb$Fare > 7.00 & comb$Fare <= 7.50] <- 2
comb$FareCat[comb$Fare > 7.50 & comb$Fare <= 8.00] <- 3
comb$FareCat[comb$Fare > 8.00 & comb$Fare <= 14.00] <- 4
comb$FareCat[comb$Fare > 14.00 & comb$Fare <= 30.00] <- 5
comb$FareCat[comb$Fare > 30.00 & comb$Fare <= 60.00] <- 6
comb$FareCat[comb$Fare > 60.00 & comb$Fare <= 150.00] <- 7
comb$FareCat[comb$Fare > 150.00] <- 8
table(comb$FareCat)
## Add new feature FareCatPclass -> 21 categories
comb$FareCatPclass <- comb$FareCat
comb$FareCatPclass <- paste(comb$Pclass, comb$FareCat, sep="")
comb$FareCatPclass[comb$FareCatPclass %in% c('10','20','30')] <- 0
comb$FareCatPclass[comb$FareCatPclass %in% c('11','21','31')] <- 1
comb$FareCatPclass[comb$FareCatPclass %in% c('18','28','38')] <- 8
table(comb$FareCatPclass)

# Split back
train <- comb[1:891,]
test <- comb[892:1309,]

########## Decision Tree ###########

# Build the tree with new features
fit <- rpart(Survived ~ AgeCatSex + FamilyID, data=train, method="class")
fancyRpartPlot(fit)
summary(fit)

Prediction <- predict(fit, test, type = "class");
result <- data.frame(PassengerId = test$PassengerId, 
                     Survived = as.numeric(levels(Prediction))[Prediction]);
write.csv(result, file = "complex-decision-tree.csv", row.names = FALSE);

########## Random forest ############

## Reduce number of Family ID's
comb$FamilyID2 <- as.character(comb$FamilyID)
comb$FamilyID2[comb$FamSize <= 3] <- 'Small'
comb$FamilyID2[comb$FamSize <= 1] <- 'Single'
unique(comb$FamilyID2)
summary(comb)

comb$AgeCat <- as.factor(comb$AgeCat)
comb$AgeCatSex <- as.factor(comb$AgeCatSex)
comb$FamilyID2 <- as.factor(comb$FamilyID2)
comb$Title <- as.factor(comb$Title)

# Split back
train <- comb[1:891,]
test <- comb[892:1309,]

# simple random forest
set.seed(241)
rf <- randomForest(as.factor(Survived) ~ AgeCatSex + Pclass + Age + Sex + FamilyID2 + FamSize 
                   + Title + SibSp + Parch + Embarked + Fare + FareCatPclass, data=train, 
                   importance=TRUE, ntree=2500)
summary(rf)
varImpPlot(rf)

Prediction <- predict(rf, test)
result <- data.frame(PassengerId = test$PassengerId, 
                     Survived = as.numeric(levels(Prediction))[Prediction])
write.csv(result, file = "simple-rforest.csv", row.names = FALSE)

# random forest with conditional inference trees
set.seed(241)
crf <- cforest(as.factor(Survived) ~ AgeCatSex + Pclass + Age + Sex + FamilyID + FamSize 
               + Title + SibSp + Parch + Embarked + Fare, data=train, 
               controls=cforest_unbiased(ntree=200, mtry=3))
summary(crf)

Prediction <- predict(crf, OOB=TRUE, test)
result <- data.frame(PassengerId = test$PassengerId, 
                     Survived = as.numeric(levels(Prediction))[Prediction])
write.csv(result, file = "conditional-rforest.csv", row.names = FALSE)

########### Neural Nets ###########

nnet <- train(Survived ~ AgeCatSex + FamilyID + Title + Embarked + Pclass + FamSize, 
              data=train, method="nnet", 
              tuneLength=7, 
              MaxNWts=1000,
              verbose = FALSE,
              tuneGrid=data.frame(.size=3, .decay=5e-3), 
              maxit=700)
summary(nnet)

Prediction <- predict(nnet, test)
result <- data.frame(PassengerId = test$PassengerId, 
                     Survived = as.numeric(levels(Prediction))[Prediction])
write.csv(result, file = "neural-net.csv", row.names = FALSE)

########## Naive Bayes ############

comb$Title <- as.factor(comb$FareCatPclass)
comb$FamilyID <- as.factor(comb$FamilyID)
comb$Embarked <- as.factor(comb$Embarked)
# Split back
train <- comb[1:891,]
test <- comb[892:1309,]

train.df = train[,c("Survived","AgeCatSex","Embarked","Title","FamilyID","Pclass","FamSize")]
test.df = test[,c("Survived","AgeCatSex","Embarked","Title","FamilyID","Pclass","FamSize")]

nb <- naiveBayes(as.factor(Survived) ~ ., # features should be conditionally independent
                data=train.df,
                trControl=trainControl(method='cv',number=10)) 
summary(nb)

Prediction <- predict(nb, test.df[,-1])

result <- data.frame(PassengerId = test$PassengerId, 
                     Survived = as.numeric(levels(Prediction))[Prediction])
write.csv(result, file = "naive-bayes.csv", row.names = FALSE)