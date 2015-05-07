library(rpart);
library(rattle);
library(rpart.plot);

train = read.csv("train.csv");
test = read.csv("test.csv");

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