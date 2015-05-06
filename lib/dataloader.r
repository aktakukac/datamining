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

#train.raw <- readUrl(Titanic.path, train.data.file, 0, train.column.types, missing.types)
train.raw <- readLocal(Titanic.localpath, train.data.file, 0, train.column.types, missing.types)
#test.raw <- readUrl(Titanic.path, test.data.file, 0, test.column.types, missing.types)
test.raw <- readLocal(Titanic.localpath, test.data.file, 0, test.column.types, missing.types)