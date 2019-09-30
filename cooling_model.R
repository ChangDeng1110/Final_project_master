regression_cooling <- function(formula, data, indices) {
  d <- data[indices,] 
  d <- na.omit(d)
  fit <- lm(formula, data=d)
  c_vals <- coefficients(fit)
  print(c_vals)
  predictions <- c_vals[2]*test_cooling$outside_temp + c_vals[1] + c_vals[3]*test_cooling$start_temp
  rmse_value <- RMSE(test_cooling$rate, predictions)
  return (rmse_value)
} 

svm_cooling <- function(formula, data, indices){
  d <- data[indices,] 
  d <- na.omit(d)
  fit <- svm(formula, data=d, gamma=0.1, cost=3.6, kernel = "radial")
  predictions <- predict(fit, test_cooling)
  rmse_value <- RMSE(test_cooling$rate, predictions)
  #print(predictions)
  #print(test_cooling$rate)
  #print(rmse_value)
  return (rmse_value)
}

NN_cooling <- function(formula, data, indices){
  d <- data[indices,] 
  d <- na.omit(d)
  nn <- neuralnet(formula,data=d,hidden=c(1),linear.output=T, threshold = 0.1
                  ,learningrate = 0.001,act.fct = "tanh")
  pr.nn <- compute(nn,test_cooling)
  rmse_value <- RMSE(test_cooling$rate, pr.nn$net.result)
  #print(rmse_value)
  return(rmse_value)
}

radom_forest_cooling <- function(formula, data, indices){
  d <- data[indices,]
  d <- na.omit(d)
  #print(d)
  rf <- rfsrc(formula, data=d, 
              mtry=3,ntree = 5000,importance = TRUE,nodesize=5,splitrule = "quantile.regr",ntime = 30)
  #print("helo")
  preds<-predict(rf, test_rf_cooling)
  #print(preds)
  rmse_value <- RMSE(test_rf_cooling$rate, preds$predicted)
  print(rmse_value)
  return(rmse_value)
}

###################### booststraping find uncertainty ##################

lr_cooling <- boot(data=train_cooling, statistic=regression_cooling, R=200,formula=rate~outside_temp + start_temp )
svm_cooling <- boot(data=train_cooling, statistic=svm_cooling, R=200, formula=rate~outside_temp + start_temp )
nn_cooling <- boot(data=train_cooling, statistic=NN_cooling, R=200, formula=rate~outside_temp + start_temp)
rf_cooling <- boot(data=train_rf_cooling, statistic=radom_forest_cooling, R=200, formula=rate~outside_temp + start_temp)
#nn_results_multi_b <- boot(data=data_model, statistic=NN_multi_b, R=300, formula=b + c ~ start_temp + outside_temp + SA_temp)

name <- c()
result <- c(lr_cooling$t,svm_cooling$t,nn_cooling$t,rf_cooling$t)

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

#for(x in 1:300){
#  name <- c(name, "NN_b_multi")
#}

tbl <- data.frame(
  names = name,
  results = result
)

boxplot(results~names, data=tbl, xlab="Model", ylab="RMSE", par(cex.axis=1), par(cex.lab=1),main="RMSE ~ cooling",names=c("NN","MLR",'Random forest','SVM'),
        xlab="Model type", ylab="Error",cex.main=2, cex.lab=1.25, cex.axis=2)

#lr_cooling
#plot(lr_b)
#svm_cooling
#nn_cooling
#rf_cooling

#type=c("norm", "basic", "stud", "perc", "bca")
#boot.ci(reg_results)

boot.ci(lr_cooling, conf=0.95,type = c("norm","basic","perc"))
#boot.ci(svm_results)
boot.ci(svm_cooling, conf=0.95,type = c("norm","basic","perc"))
boot.ci(nn_cooling, conf=0.95,type = c("norm","basic","perc"))
boot.ci(rf_cooling, conf=0.95,type = c("norm","basic","perc"))


mean(lr_cooling$t)
mean(svm_cooling$t)
mean(nn_cooling$t)
mean(rf_cooling$t)

############### RF AND SVM ################
svm_cooling
plot(svm_cooling)

plot(rf_cooling)
