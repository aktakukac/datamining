# DataMining for Titanic Kaggle competition

###honorific-title-model.r
  Fills the missing age by using the honorific title, and take in account titles that belong to nobility, cabin class, port of embarkment, sex and if it has a family on ship or not. Uses simple boot cross validation on data.
  
|Algorithm   | Kaggle Score   |
|---|---|
| Logistic Regression  | 0.78947  |
| Random Forests  |  0.78947|
| Support Vector Machines  |  0.79904 |


###discretized-age-gender-familyId.r
  Fills missing age values using a regression tree on initial features, plus family size and honorific title. Adds categorical age combined with sex, and a family id as a combination of surname and family size.

|Algorithm   | Kaggle Score   |
|---|---|
|  Decision tree | 0.81340  |
|  Naive Bayes Classifier |  0.72249|
|  Neural Network model |  0.79426 |
|  Random forest with conditional inference trees | 0.80383 |

