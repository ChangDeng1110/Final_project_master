regression_heating <- function(formula, data, indices) {
  d <- data[indices,] 
  d <- na.omit(d)
  fit <- lm(formula, data=d)
  c_vals <- coefficients(fit)
  print(c_vals)
  predictions <- c_vals[2]*test_heating$outside_temp + c_vals[1] + c_vals[3]*test_heating$start_temp
  rmse_value <- RMSE(test_heating$rate, predictions)
  print(predictions)
  print(rmse_value)
  return (rmse_value)
} 

svm_heating <- function(formula, data, indices){
  d <- data[indices,] 
  d <- na.omit(d)
  fit <- svm(formula, data=d, gamma=2.1, cost=1.5, kernel = "radial")
  predictions <- predict(fit, test_heating)
  rmse_value <- RMSE(test_heating$rate, predictions)
  #print(predictions)
  #print(test_heating$rate)
  #print(rmse_value)
  return (rmse_value)
}

NN_heating <- function(formula, data, indices){
  d <- data[indices,] 
  d <- na.omit(d)
  nn <- neuralnet(formula,data=d,hidden=c(4),linear.output=T, threshold = 0.1,algorithm = "sag",
                  learningrate = 0.001,act.fct = "tanh")
  pr.nn <- compute(nn,test_heating)
  rmse_value <- RMSE(test_heating$rate, pr.nn$net.result)
  print(rmse_value)
  return(rmse_value)
}

radom_forest_heating <- function(formula, data, indices){
  d <- data[indices,]
  d <- na.omit(d)
  #print(d)
  rf <- rfsrc(formula, data=d, 
              mtry=3,ntree = 5000,importance = TRUE,nodesize=9,splitrule = "mse", ntime = 30)
  #print("helo")
  preds<-predict(rf, test_rf_heating)
  print(preds)
  rmse_value <- RMSE(test_rf_heating$rate, preds$predicted)
  #print(rmse_value)
  return(rmse_value)
}

#rf_b_results <- boot(data=data_model_rf, statistic=radom_forest_b_cooling, R=300, formula=b ~ start_temp + outside_temp+SA_temp )
#boot(data=scaled, statistic=radom_forest_b, R=500, formula= b ~ start_temp + outside_temp + SA_temp)


###################### booststraping find uncertainty ##################

lr_heating <- boot(data=train_heating, statistic=regression_heating, R=200,formula=rate~outside_temp + start_temp)
svm_heating <- boot(data=train_heating, statistic=svm_heating, R=200, formula=rate~outside_temp + start_temp)
nn_heating <- boot(data=train_heating, statistic=NN_heating, R=200, formula=rate~outside_temp + start_temp)
rf_heating <- boot(data=train_rf_heating, statistic=radom_forest_heating, R=200, formula=rate~outside_temp + start_temp)
#nn_results_multi_b <- boot(data=data_model, statistic=NN_multi_b, R=300, formula=b + c ~ start_temp + outside_temp + SA_temp)

name <- c()
result <- c(lr_heating$t,svm_heating$t,nn_heating$t,rf_heating$t)

for(x in 1:200){
  name <- c(name, "Regression")
}
for(x in 1:200){
  name <- c(name, "SVM")
}
for(x in 1:200){
  name <- c(name, "NN")
}
for(x in 1:200){
  name <- c(name, "RF")
}

tbl <- data.frame(
  names = name,
  results = result
)

boxplot(results~names, data=tbl, xlab="Model", ylab="RMSE", par(cex.axis=1), par(cex.lab=1),main="RMSE ~ heating",names=c("NN","MLR",'Random forest','SVM'),
        xlab="Model type", ylab="Error",cex.main=2, cex.lab=1.25, cex.axis=2)

lr_heating
#plot(lr_b)
svm_heating
nn_heating
rf_heating

type=c("norm", "basic", "stud", "perc", "bca")
boot.ci(lr_heating, conf=0.95,type = c("norm","basic","perc"))
boot.ci(svm_heating, conf=0.95,type = c("norm","basic","perc"))
boot.ci(nn_heating, conf=0.95,type = c("norm","basic","perc"))
boot.ci(rf_heating, conf=0.95,type = c("norm","basic","perc"))

mean(lr_heating$t)
mean(svm_heating$t)
mean(nn_heating$t)
mean(rf_heating$t)
