heating_model <- function(formula, data, indices){
  d <- data[indices,]
  d <- na.omit(d)
  rf <- rfsrc(rate~start_temp + outside_temp, data=d, 
              mtry=3,ntree = 500,importance = TRUE,nodesize=3,splitrule = "mse", ntime = 30)
  preds<-predict(rf, use_data)
  return(preds$predicted)
}

cooling_model <- function(formula, data, indices){
  d <- data[indices,] 
  d <- na.omit(d)
  rf <- svm(formula, data=d, gamma=0.1, cost=3.6, kernel = "radial")
  preds<-predict(rf, use_data_cool)
  return (preds)
}
