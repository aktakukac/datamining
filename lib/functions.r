readUrl <- function(path.name, file.name, skip.lines, column.types, missing.types) {
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

## function used to get the median age of an honorific title (used where an id is missing age) and some other places
setToMedian <- function(dest.var, filter.var, var.levels) {
  for (v in var.levels) {
    dest.var[which( filter.var == v)] <- median(dest.var[which(filter.var == v)], na.rm = TRUE)
  }
  return (dest.var)
}

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

## function to convert our predictions and save the result to csv for kaggle submission
predictAndSave <- function(model,test.data, infer.data, file.name) {
  # use the model to generate predictions
  Survived <- predict(model, newdata = test.data)
  
  # reformat predictions to 0 or 1 and link to PassengerId in a data frame
  #Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
  predictions <- as.data.frame(Survived)
  predictions$PassengerId <- infer.data$PassengerId
  
  # write predictions to csv file for submission to Kaggle
  write.csv(predictions[,c("PassengerId", "Survived")], file=file.name, row.names=FALSE, quote=FALSE)

}
