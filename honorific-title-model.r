readData <- function(path.name, file.name, skip.lines, column.types, missing.types) {
  read.csv( url( paste0(path.name, file.name) ), sep=",",
            colClasses=column.types,
            na.strings=missing.types )
}

readLocal <- function(path.name, file.name, skip.lines, column.types, missing.types) {
  read.csv(paste0(path.name, file.name), 
           header = TRUE, 
           sep=",", 
           dec = ".",
           skip = skip.lines,
           colClasses=column.types,
           na.strings=missing.types )
}

## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  title.comma.end
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

Titanic.path <- "https://raw.githubusercontent.com/nicupavel/datamining/master/dataset/"
Titanic.localpath <- "dataset/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv
#train.raw <- readData(Titanic.path, train.data.file, 0, train.column.types, missing.types)
train.raw <- readLocal(Titanic.localpath, train.data.file, 0, train.column.types, missing.types)
df.train <- train.raw

#test.raw <- readData(Titanic.path, test.data.file, 0, test.column.types, missing.types)
test.raw <- readLocal(Titanic.localpath, test.data.file, 0, test.column.types, missing.types)
df.infer <- test.raw

# Based on https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
# map missing data by provided feature
#require(Amelia)
#missmap(df.train, main="Titanic Training Data - Missings Map",         col=c("yellow", "black"), legend=FALSE)

#mosaicplot(df.train$Pclass ~ df.train$Survived, 
#           main="Passenger Fate by Traveling Class", shade=FALSE, 
#           color=TRUE, xlab="Pclass", ylab="Survived")

df.train$Title <- getTitle(df.train)
#unique(df.train$Title)
# identify the titles which have at least one record with an age missing, using the bystats function from the Hmisc package.
options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, fun=function(x)c(Mean=mean(x),Median=median(x)))

