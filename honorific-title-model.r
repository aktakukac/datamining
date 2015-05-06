require(plyr)
require(caret)
require(Hmisc)
require(pROC)
require(e1071)
require(randomForest)
require(kernlab)

source("lib/functions.r")
source("lib/dataloader.r")

df.train <- train.raw
df.infer <- test.raw

# Based on https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
# map missing data by provided feature
#require(Amelia)
#missmap(df.train, main="Titanic Training Data - Missings Map",         col=c("yellow", "black"), legend=FALSE)

#mosaicplot(df.train$Pclass ~ df.train$Survived, 
#           main="Passenger Fate by Traveling Class", shade=FALSE, 
#           color=TRUE, xlab="Pclass", ylab="Survived")

df.train$Title <- getTitle(df.train)
df.infer$Title <- getTitle(df.infer)

unique(df.train$Title)
# identify the titles which have at least one record with an age missing, using the bystats function from the Hmisc package.
options(digits=2)
bystats(df.train$Age, df.train$Title, fun=function(x)c(Mean=mean(x),Median=median(x)))

# Manually add these to a list
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# Do we have missing age for a title
# df.train$Age[which(df.train$Title=="Dr")]

df.train$Age <- setToMedian(df.train$Age, df.train$Title, titles.na.train)

# We also have missing Emberked port information
#summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

# Consolidate the honorific titles for example Lady -> Noble
# Also Master title means that it's a boy with age < 15 => high Survived probability
## Title consolidation
selectedNobleTitles <- c("Capt", "Col", "Don", "Dr", 
                         "Jonkheer", "Lady", "Major", 
                         "Rev", "Sir")
df.train$Title <- changeTitles(df.train, 
                               selectedNobleTitles,
                               "Noble")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme", "Ms"), 
                               "Miss")

df.train$Title <- as.factor(df.train$Title)

# Same for infer
df.infer$Title <- changeTitles(df.infer, 
                               selectedNobleTitles,
                               "Noble")
df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme", "Ms"), 
                               "Miss")

df.infer$Title <- as.factor(df.infer$Title)

#infer age
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.infer$Age <- setToMedian(df.infer$Age, df.infer$Title, titles.na.test)

# infer missing fares
df.infer$Fare[ which( df.infer$Fare == 0)] <- NA
df.infer$Fare <- setToMedian(df.infer$Fare, df.infer$Pclass, 
                              as.numeric(levels(df.infer$Pclass)))

# Family Siblings/Spouses(SibSp) and childred (Parch)
df.train$Family <- df.train$SibSp + df.train$Parch
df.infer$Family <- df.infer$SibSp + df.infer$Parch

#add survived model that we have from trai
# What features we should keep
train.keeps <- c("Survived", "Sex", "Age", "Title", "Pclass", "Fare", "Embarked", "Family")
df.train.munged <- df.train[train.keeps]

## split training data into train batch and test batch
set.seed(23)
training.rows <- createDataPartition(df.train.munged$Survived, 
                                     p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows, ]
test.batch <- df.train.munged[-training.rows, ]

#data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- df.infer[test.keeps]

set.seed(35)
## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
# http://www.inside-r.org/packages/cran/caret/docs/trainControl
cv.ctrl <- trainControl(method = "LOOCV", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)


########################### Logistic Regression

# ROC is receiver operating characteristic: metric = "ROC", trControl = cv.ctrl
# Accuracy 0.78
glm.simple.1 <- train(Survived ~ Sex + Pclass + Age + Embarked,
                    data = train.batch,
                    method = "glm")


# I() function, inhibits interpretation & conversion of R objects, to create a new 2-level factor 
# within the model formula. This factor is valued TRUE if a passenger's port of origin was 
# Southampton ("S"), or FALSE otherwise

glm.complex <- train(Survived ~ Pclass + I(Title=="Mr") + I(Title=="Noble") 
                     + Age + Family + I(Embarked=="S") 
                     + I(Title=="Mr" & Pclass==3), 
                     data = train.batch, 
                     method = "glm")


predictAndSave(glm.complex, pred.these, df.infer, "feature-selection-logistic-regression.csv")

###########################  Random Forests
rndforest.grid <- data.frame(.mtry = c(2, 3))
rndforest.simple <- train(Survived ~ Sex + Pclass + Age + Embarked, 
                 data = train.batch,
                 method = "rf",
                 tuneGrid = rndforest.grid)

rndforest.complex <- train(Survived ~ Pclass + I(Title=="Mr") + I(Title=="Noble") 
                           + Age + Family + I(Embarked=="S") 
                           + I(Title=="Mr" & Pclass==3),
                           data = train.batch,
                           method = "rf",
                          tuneGrid = rndforest.grid)

predictAndSave(rndforest.simple, pred.these, df.infer,"feature-selection-random-forests-simple.csv")
predictAndSave(rndforest.complex, pred.these, df.infer,"feature-selection-random-forests-complex.csv")


###########################  Support Vector Machines
svm.complex <- train(Survived ~ Pclass + I(Title=="Mr") + I(Title=="Noble") 
                  + Age + Family + I(Embarked=="S") 
                  + I(Title=="Mr" & Pclass==3), 
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"))

predictAndSave(svm.complex, pred.these, df.infer,"feature-selection-svm-complex.csv")