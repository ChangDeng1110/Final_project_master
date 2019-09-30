set.seed(500)

folds <- createFolds(train_cooling$rate,k = 10)
indexll <- 1
plot_svm <- matrix(c(3:14), nrow = 51, ncol = 3)
for (i in seq(0,5,by = 0.1)){
  output_list_svm <- c()
  cv_result_svm <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    #print(test_data$rate)
    model <- svm(rate ~outside_temp + start_temp, data=train_data, gamma=i, cost=1, kernel = "radial")
    #print(model)
    prediction <- predict(model, test_data)
    #print(prediction)
    print("======")
    return(mmetric(test_data$rate,prediction,c("RMSE","MAE")))
  })
  
  output <- rowMeans(cbind(cv_result_svm$Fold1,cv_result_svm$Fold2,
                           cv_result_svm$Fold3,cv_result_svm$Fold4,
                           cv_result_svm$Fold5,cv_result_svm$Fold6,
                           cv_result_svm$Fold7))
  print(as.matrix(output)[1,1])
  plot_svm[indexll,1] <- i
  plot_svm[indexll,2] <- as.matrix(output)[1,1]
  plot_svm[indexll,3] <- as.matrix(output)[2,1]
  indexll <- indexll + 1
}

plot_svm_data <- as.data.frame(plot_svm)
names(plot_svm_data)[2] <- "RMSE"
names(plot_svm_data)[3] <- "MAE"
#plot_svm
ggplot(plot_svm_data, aes(V1)) + 
  geom_line(aes(y = RMSE, colour = "RMSE")) + 
  geom_line(aes(y = MAE, colour = "MAE")) + labs(title = "Error vs Gamma (cooling)") + xlab("Gamma") +
  ylab("Error")+theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                      title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "Error vs gamma (cooling)", x = "Gamma", y = "Error",color = "Records")+scale_color_hue(labels = c("MAE", "RMSE"))



########### C ################
indexll <- 1
plot_svm <- matrix(c(3:14), nrow = 50, ncol = 3)
for (i in seq(0.1,5,by = 0.1)){
  output_list_svm <- c()
  cv_result_svm <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    #print(test_data$rate)
    model <- svm(rate ~outside_temp + start_temp, data=train_data, gamma=0.1, cost=i, kernel = "radial")
    #print(model)
    prediction <- predict(model, test_data)
    #print(prediction)
    print("======")
    return(mmetric(test_data$rate,prediction,c("RMSE","MAE")))
  })
  
  output <- rowMeans(cbind(cv_result_svm$Fold1,cv_result_svm$Fold2,
                           cv_result_svm$Fold3,cv_result_svm$Fold4,
                           cv_result_svm$Fold5,cv_result_svm$Fold6,
                           cv_result_svm$Fold7))
  print(as.matrix(output)[1,1])
  plot_svm[indexll,1] <- i
  plot_svm[indexll,2] <- as.matrix(output)[1,1]
  plot_svm[indexll,3] <- as.matrix(output)[2,1]
  indexll <- indexll + 1
}

plot_svm_data <- as.data.frame(plot_svm)
names(plot_svm_data)[2] <- "RMSE"
names(plot_svm_data)[3] <- "MAE"
#plot_svm
ggplot(plot_svm_data, aes(V1)) + 
  geom_line(aes(y = RMSE, colour = "RMSE")) + 
  geom_line(aes(y = MAE, colour = "MAE")) + labs(title = "Error vs Cost (cooling)") + xlab("C") +
  ylab("Error")+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold")
                       ,title=element_text(size=18,face="bold"))

###############################

indexll <- 1
plot_svm <- matrix(c(3:14), nrow = 50, ncol = 3)
for (i in seq(0.1,5,by = 0.1)){
  output_list_svm <- c()
  cv_result_svm <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    #print(test_data$rate)
    model <- svm(rate ~outside_temp + start_temp, data=train_data,cost=i, kernel = "linear")
    #print(model)
    prediction <- predict(model, test_data)
    #print(prediction)
    print("======")
    return(mmetric(test_data$rate,prediction,c("RMSE","MAE")))
  })
  
  output <- rowMeans(cbind(cv_result_svm$Fold1,cv_result_svm$Fold2,
                           cv_result_svm$Fold3,cv_result_svm$Fold4,
                           cv_result_svm$Fold5,cv_result_svm$Fold6,
                           cv_result_svm$Fold7))
  print(as.matrix(output)[1,1])
  plot_svm[indexll,1] <- i
  plot_svm[indexll,2] <- as.matrix(output)[1,1]
  plot_svm[indexll,3] <- as.matrix(output)[2,1]
  indexll <- indexll + 1
}

plot_svm_data <- as.data.frame(plot_svm)
names(plot_svm_data)[2] <- "RMSE"
names(plot_svm_data)[3] <- "MAE"
#plot_svm
ggplot(plot_svm_data, aes(V1)) + 
  geom_line(aes(y = RMSE, colour = "RMSE")) + 
  geom_line(aes(y = MAE, colour = "MAE")) + labs(title = "Error vs Cost (cooling)") + xlab("C") +
  ylab("Error")+ theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold")
                       ,title=element_text(size=18,face="bold"))

#####################################
cv_result_svm_linear <- lapply(folds, function(x){
  train_data <- train_cooling[-x,]
  test_data <- train_cooling[x,]
  model <- svm(rate~outside_temp + start_temp, data=train_data,kernel = "linear")
  prediction <- predict(model, test_data)
  return(mmetric(test_data$rate,prediction,c("RMSE","MAE")))
})

svm_linear <- rowMeans(cbind(cv_result_svm_linear$Fold1,cv_result_svm_linear$Fold2,
                             cv_result_svm_linear$Fold3,cv_result_svm_linear$Fold4,
                             cv_result_svm_linear$Fold5,cv_result_svm_linear$Fold6,
                             cv_result_svm_linear$Fold7))

####################### LM-cv #################### 

cv_result_lm <- lapply(folds, function(x){
  train_data <- train_cooling[-x,]
  test_data <- train_cooling[x,]
  model <- lm(rate~outside_temp + start_temp, data=train_data)
  prediction <- predict(model, test_data)
  return(mmetric(test_data$rate,prediction,c("RMSE","MAE")))
})

lm_out <- rowMeans(cbind(cv_result_lm$Fold1,cv_result_lm$Fold2,
                         cv_result_lm$Fold3,cv_result_lm$Fold4,
                         cv_result_lm$Fold5,cv_result_lm$Fold6,
                         cv_result_lm$Fold7))

#################### NN - cv - sigmod ######################
test_nn_rprop <- function (number){
  list_ouput_final <- c()
  cv_result_nn_sig <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.1,
                       algorithm = "rprop+",learningrate = 0.001)
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn1 <- rowMeans(cbind(cv_result_nn_sig$Fold1,cv_result_nn_sig$Fold2,
                               cv_result_nn_sig$Fold3,cv_result_nn_sig$Fold4,
                               cv_result_nn_sig$Fold5,cv_result_nn_sig$Fold6,
                               cv_result_nn_sig$Fold7))
  
  cv_result_nn_tanh <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.1,
                       algorithm = "rprop+",learningrate = 0.001,act.fct = "tanh")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn2 <- rowMeans(cbind(cv_result_nn_tanh$Fold1,cv_result_nn_tanh$Fold2,
                               cv_result_nn_tanh$Fold3,cv_result_nn_tanh$Fold4,
                               cv_result_nn_tanh$Fold5,cv_result_nn_tanh$Fold6,
                               cv_result_nn_tanh$Fold7))
  
  cv_result_nn_logstic <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.1,
                       algorithm = "rprop+",learningrate = 0.001,act.fct = "logistic")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn3 <- rowMeans(cbind(cv_result_nn_logstic$Fold1,cv_result_nn_logstic$Fold2,
                               cv_result_nn_logstic$Fold3,cv_result_nn_logstic$Fold4,
                               cv_result_nn_logstic$Fold5,cv_result_nn_logstic$Fold6,
                               cv_result_nn_logstic$Fold7))
  list_ouput_final[1] <- as.data.frame(output_nn1)
  list_ouput_final[2] <- as.data.frame(output_nn2)
  list_ouput_final[3] <- as.data.frame(output_nn3)
  #print(list_ouput_final)
  return (list_ouput_final)
}

plot_nn_rprop <- as.data.frame(matrix(c(3:14), nrow = 12, ncol = 7))
name <- c("(0)","(1)","(2)","(3)","(4)","(1,1)","(1,2)","(1,3)","(2,3)","(2,4)","(1,1,2)","(1,2,2)")
plot_nn_rprop["V1"] <- name
names(plot_nn_rprop)[2] <- "RMSE-sigmod"
names(plot_nn_rprop)[3] <- "MAE-sigmod"
names(plot_nn_rprop)[4] <- "RMSE-tanh"
names(plot_nn_rprop)[5] <- "MAE-tanh"
names(plot_nn_rprop)[6] <- "RMSE-logistic"
names(plot_nn_rprop)[7] <- "MAE-logistic"

x <- test_nn_rprop(c(0))
plot_nn_rprop[1,2] <- x[[1]][1]
plot_nn_rprop[1,3] <- x[[1]][2]
plot_nn_rprop[1,4] <- x[[2]][1]
plot_nn_rprop[1,5] <- x[[2]][2]
plot_nn_rprop[1,6] <- x[[3]][1]
plot_nn_rprop[1,7] <- x[[3]][2]

x <- test_nn_rprop(c(1))
plot_nn_rprop[2,2] <- x[[1]][1]
plot_nn_rprop[2,3] <- x[[1]][2]
plot_nn_rprop[2,4] <- x[[2]][1]
plot_nn_rprop[2,5] <- x[[2]][2]
plot_nn_rprop[2,6] <- x[[3]][1]
plot_nn_rprop[2,7] <- x[[3]][2]

x <- test_nn_rprop(c(2))
plot_nn_rprop[3,2] <- x[[1]][1]
plot_nn_rprop[3,3] <- x[[1]][2]
plot_nn_rprop[3,4] <- x[[2]][1]
plot_nn_rprop[3,5] <- x[[2]][2]
plot_nn_rprop[3,6] <- x[[3]][1]
plot_nn_rprop[3,7] <- x[[3]][2]

x <- test_nn_rprop(c(3))
plot_nn_rprop[4,2] <- x[[1]][1]
plot_nn_rprop[4,3] <- x[[1]][2]
plot_nn_rprop[4,4] <- x[[2]][1]
plot_nn_rprop[4,5] <- x[[2]][2]
plot_nn_rprop[4,6] <- x[[3]][1]
plot_nn_rprop[4,7] <- x[[3]][2]

x <- test_nn_rprop(c(4))
plot_nn_rprop[5,2] <- x[[1]][1]
plot_nn_rprop[5,3] <- x[[1]][2]
plot_nn_rprop[5,4] <- x[[2]][1]
plot_nn_rprop[5,5] <- x[[2]][2]
plot_nn_rprop[5,6] <- x[[3]][1]
plot_nn_rprop[5,7] <- x[[3]][2]

x <- test_nn_rprop(c(1,1))
plot_nn_rprop[6,2] <- x[[1]][1]
plot_nn_rprop[6,3] <- x[[1]][2]
plot_nn_rprop[6,4] <- x[[2]][1]
plot_nn_rprop[6,5] <- x[[2]][2]
plot_nn_rprop[6,6] <- x[[3]][1]
plot_nn_rprop[6,7] <- x[[3]][2]

x <- test_nn_rprop(c(1,2))
plot_nn_rprop[7,2] <- x[[1]][1]
plot_nn_rprop[7,3] <- x[[1]][2]
plot_nn_rprop[7,4] <- x[[2]][1]
plot_nn_rprop[7,5] <- x[[2]][2]
plot_nn_rprop[7,6] <- x[[3]][1]
plot_nn_rprop[7,7] <- x[[3]][2]

x <- test_nn_rprop(c(1,3))
plot_nn_rprop[8,2] <- x[[1]][1]
plot_nn_rprop[8,3] <- x[[1]][2]
plot_nn_rprop[8,4] <- x[[2]][1]
plot_nn_rprop[8,5] <- x[[2]][2]
plot_nn_rprop[8,6] <- x[[3]][1]
plot_nn_rprop[8,7] <- x[[3]][2]

x <- test_nn_rprop(c(2,3))
plot_nn_rprop[9,2] <- x[[1]][1]
plot_nn_rprop[9,3] <- x[[1]][2]
plot_nn_rprop[9,4] <- x[[2]][1]
plot_nn_rprop[9,5] <- x[[2]][2]
plot_nn_rprop[9,6] <- x[[3]][1]
plot_nn_rprop[9,7] <- x[[3]][2]

x <- test_nn_rprop(c(2,4))
plot_nn_rprop[10,2] <- x[[1]][1]
plot_nn_rprop[10,3] <- x[[1]][2]
plot_nn_rprop[10,4] <- x[[2]][1]
plot_nn_rprop[10,5] <- x[[2]][2]
plot_nn_rprop[10,6] <- x[[3]][1]
plot_nn_rprop[10,7] <- x[[3]][2]

x <- test_nn_rprop(c(1,1,2))
plot_nn_rprop[11,2] <- x[[1]][1]
plot_nn_rprop[11,3] <- x[[1]][2]
plot_nn_rprop[11,4] <- x[[2]][1]
plot_nn_rprop[11,5] <- x[[2]][2]
plot_nn_rprop[11,6] <- x[[3]][1]
plot_nn_rprop[11,7] <- x[[3]][2]

x <- test_nn_rprop(c(1,2,2))
plot_nn_rprop[12,2] <- x[[1]][1]
plot_nn_rprop[12,3] <- x[[1]][2]
plot_nn_rprop[12,4] <- x[[2]][1]
plot_nn_rprop[12,5] <- x[[2]][2]
plot_nn_rprop[12,6] <- x[[3]][1]
plot_nn_rprop[12,7] <- x[[3]][2]

plot_nn_rprop

###################### NN - cv - backpropagate ###########
test_nn_backprop <- function (number){
  list_ouput_final <- c()
  cv_result_nn_sig <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "backprop",learningrate = 0.0001)
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn1 <- rowMeans(cbind(cv_result_nn_sig$Fold1,cv_result_nn_sig$Fold2,
                               cv_result_nn_sig$Fold3,cv_result_nn_sig$Fold4,
                               cv_result_nn_sig$Fold5,cv_result_nn_sig$Fold6,
                               cv_result_nn_sig$Fold7))
  
  cv_result_nn_tanh <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "backprop",learningrate = 0.0001,act.fct = "tanh")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn2 <- rowMeans(cbind(cv_result_nn_tanh$Fold1,cv_result_nn_tanh$Fold2,
                               cv_result_nn_tanh$Fold3,cv_result_nn_tanh$Fold4,
                               cv_result_nn_tanh$Fold5,cv_result_nn_tanh$Fold6,
                               cv_result_nn_tanh$Fold7))
  
  cv_result_nn_logstic <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "backprop",learningrate = 0.0001,act.fct = "logistic")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn3 <- rowMeans(cbind(cv_result_nn_logstic$Fold1,cv_result_nn_logstic$Fold2,
                               cv_result_nn_logstic$Fold3,cv_result_nn_logstic$Fold4,
                               cv_result_nn_logstic$Fold5,cv_result_nn_logstic$Fold6,
                               cv_result_nn_logstic$Fold7))
  list_ouput_final[1] <- as.data.frame(output_nn1)
  list_ouput_final[2] <- as.data.frame(output_nn2)
  list_ouput_final[3] <- as.data.frame(output_nn3)
  #print(list_ouput_final)
  return (list_ouput_final)
}



plot_nn_back <- as.data.frame(matrix(c(3:14), nrow = 12, ncol = 7))
name <- c("(0)","(1)","(2)","(3)","(4)","(1,1)","(1,2)","(1,3)","(2,3)","(2,4)","(1,1,2)","(1,2,2)")
plot_nn_back["V1"] <- name
names(plot_nn_back)[2] <- "RMSE-sigmod"
names(plot_nn_back)[3] <- "MAE-sigmod"
names(plot_nn_back)[4] <- "RMSE-tanh"
names(plot_nn_back)[5] <- "MAE-tanh"
names(plot_nn_back)[6] <- "RMSE-logistic"
names(plot_nn_back)[7] <- "MAE-logistic"

x <- test_nn_backprop(c(0))
plot_nn_back[1,2] <- x[[1]][1]
plot_nn_back[1,3] <- x[[1]][2]
plot_nn_back[1,4] <- x[[2]][1]
plot_nn_back[1,5] <- x[[2]][2]
plot_nn_back[1,6] <- x[[3]][1]
plot_nn_back[1,7] <- x[[3]][2]

x <- test_nn_backprop(c(1))
plot_nn_back[2,2] <- x[[1]][1]
plot_nn_back[2,3] <- x[[1]][2]
plot_nn_back[2,4] <- x[[2]][1]
plot_nn_back[2,5] <- x[[2]][2]
plot_nn_back[2,6] <- x[[3]][1]
plot_nn_back[2,7] <- x[[3]][2]

x <- test_nn_backprop(c(2))
plot_nn_back[3,2] <- x[[1]][1]
plot_nn_back[3,3] <- x[[1]][2]
plot_nn_back[3,4] <- x[[2]][1]
plot_nn_back[3,5] <- x[[2]][2]
plot_nn_back[3,6] <- x[[3]][1]
plot_nn_back[3,7] <- x[[3]][2]

x <- test_nn_backprop(c(3))
plot_nn_back[4,2] <- x[[1]][1]
plot_nn_back[4,3] <- x[[1]][2]
plot_nn_back[4,4] <- x[[2]][1]
plot_nn_back[4,5] <- x[[2]][2]
plot_nn_back[4,6] <- x[[3]][1]
plot_nn_back[4,7] <- x[[3]][2]

x <- test_nn_backprop(c(4))
plot_nn_back[5,2] <- x[[1]][1]
plot_nn_back[5,3] <- x[[1]][2]
plot_nn_back[5,4] <- x[[2]][1]
plot_nn_back[5,5] <- x[[2]][2]
plot_nn_back[5,6] <- x[[3]][1]
plot_nn_back[5,7] <- x[[3]][2]


x <- test_nn_backprop(c(1,1))
plot_nn_back[6,2] <- x[[1]][1]
plot_nn_back[6,3] <- x[[1]][2]
plot_nn_back[6,4] <- x[[2]][1]
plot_nn_back[6,5] <- x[[2]][2]
plot_nn_back[6,6] <- x[[3]][1]
plot_nn_back[6,7] <- x[[3]][2]

x <- test_nn_backprop(c(1,2))
plot_nn_back[7,2] <- x[[1]][1]
plot_nn_back[7,3] <- x[[1]][2]
plot_nn_back[7,4] <- x[[2]][1]
plot_nn_back[7,5] <- x[[2]][2]
plot_nn_back[7,6] <- x[[3]][1]
plot_nn_back[7,7] <- x[[3]][2]

x <- test_nn_backprop(c(1,3))
plot_nn_back[8,2] <- x[[1]][1]
plot_nn_back[8,3] <- x[[1]][2]
plot_nn_back[8,4] <- x[[2]][1]
plot_nn_back[8,5] <- x[[2]][2]
plot_nn_back[8,6] <- x[[3]][1]
plot_nn_back[8,7] <- x[[3]][2]

x <- test_nn_backprop(c(2,3))
plot_nn_back[9,2] <- x[[1]][1]
plot_nn_back[9,3] <- x[[1]][2]
plot_nn_back[9,4] <- x[[2]][1]
plot_nn_back[9,5] <- x[[2]][2]
plot_nn_back[9,6] <- x[[3]][1]
plot_nn_back[9,7] <- x[[3]][2]

x <- test_nn_backprop(c(2,4))
plot_nn_back[10,2] <- x[[1]][1]
plot_nn_back[10,3] <- x[[1]][2]
plot_nn_back[10,4] <- x[[2]][1]
plot_nn_back[10,5] <- x[[2]][2]
plot_nn_back[10,6] <- x[[3]][1]
plot_nn_back[10,7] <- x[[3]][2]

x <- test_nn_backprop(c(1,1,2))
plot_nn_back[11,2] <- x[[1]][1]
plot_nn_back[11,3] <- x[[1]][2]
plot_nn_back[11,4] <- x[[2]][1]
plot_nn_back[11,5] <- x[[2]][2]
plot_nn_back[11,6] <- x[[3]][1]
plot_nn_back[11,7] <- x[[3]][2]

x <- test_nn_backprop(c(1,2,2))
plot_nn_back[12,2] <- x[[1]][1]
plot_nn_back[12,3] <- x[[1]][2]
plot_nn_back[12,4] <- x[[2]][1]
plot_nn_back[12,5] <- x[[2]][2]
plot_nn_back[12,6] <- x[[3]][1]
plot_nn_back[12,7] <- x[[3]][2]

###################### NN - cv - sag ################

test_nn_sag <- function (number){
  list_ouput_final <- c()
  cv_result_nn_sig <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "sag",learningrate = 0.001)
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn1 <- rowMeans(cbind(cv_result_nn_sig$Fold1,cv_result_nn_sig$Fold2,
                               cv_result_nn_sig$Fold3,cv_result_nn_sig$Fold4,
                               cv_result_nn_sig$Fold5,cv_result_nn_sig$Fold6,
                               cv_result_nn_sig$Fold7))
  
  cv_result_nn_tanh <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "sag",learningrate = 0.001,act.fct = "tanh")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn2 <- rowMeans(cbind(cv_result_nn_tanh$Fold1,cv_result_nn_tanh$Fold2,
                               cv_result_nn_tanh$Fold3,cv_result_nn_tanh$Fold4,
                               cv_result_nn_tanh$Fold5,cv_result_nn_tanh$Fold6,
                               cv_result_nn_tanh$Fold7))
  
  cv_result_nn_logstic <- lapply(folds, function(x){
    train_data <- train_cooling[-x,]
    test_data <- train_cooling[x,]
    model <- neuralnet(as.formula(rate~outside_temp + start_temp),
                       data=train_data,hidden=number,linear.output=T, threshold = 0.5,
                       algorithm = "sag",learningrate = 0.001,act.fct = "logistic")
    prediction <- compute(model,test_data)
    #print(test_data)
    #print(prediction)
    return(mmetric(test_data$rate,prediction$net.result,c("RMSE","MAE")))
  })
  output_nn3 <- rowMeans(cbind(cv_result_nn_logstic$Fold1,cv_result_nn_logstic$Fold2,
                               cv_result_nn_logstic$Fold3,cv_result_nn_logstic$Fold4,
                               cv_result_nn_logstic$Fold5,cv_result_nn_logstic$Fold6,
                               cv_result_nn_logstic$Fold7))
  list_ouput_final[1] <- as.data.frame(output_nn1)
  list_ouput_final[2] <- as.data.frame(output_nn2)
  list_ouput_final[3] <- as.data.frame(output_nn3)
  #print(list_ouput_final)
  return (list_ouput_final)
}

#test_nn_sag(c(3))


plot_nn_sag <- as.data.frame(matrix(c(3:14), nrow = 12, ncol = 7))
name <- c("(0)","(1)","(2)","(3)","(4)","(1,1)","(1,2)","(1,3)","(2,3)","(2,4)","(1,1,2)","(1,2,2)")
plot_nn_sag["V1"] <- name
names(plot_nn_sag)[2] <- "RMSE-sigmod"
names(plot_nn_sag)[3] <- "MAE-sigmod"
names(plot_nn_sag)[4] <- "RMSE-tanh"
names(plot_nn_sag)[5] <- "MAE-tanh"
names(plot_nn_sag)[6] <- "RMSE-logistic"
names(plot_nn_sag)[7] <- "MAE-logistic"

x <- test_nn_sag(c(0))
plot_nn_sag[1,2] <- x[[1]][1]
plot_nn_sag[1,3] <- x[[1]][2]
plot_nn_sag[1,4] <- x[[2]][1]
plot_nn_sag[1,5] <- x[[2]][2]
plot_nn_sag[1,6] <- x[[3]][1]
plot_nn_sag[1,7] <- x[[3]][2]

x <- test_nn_sag(c(1))
plot_nn_sag[2,2] <- x[[1]][1]
plot_nn_sag[2,3] <- x[[1]][2]
plot_nn_sag[2,4] <- x[[2]][1]
plot_nn_sag[2,5] <- x[[2]][2]
plot_nn_sag[2,6] <- x[[3]][1]
plot_nn_sag[2,7] <- x[[3]][2]

x <- test_nn_sag(c(2))
plot_nn_sag[3,2] <- x[[1]][1]
plot_nn_sag[3,3] <- x[[1]][2]
plot_nn_sag[3,4] <- x[[2]][1]
plot_nn_sag[3,5] <- x[[2]][2]
plot_nn_sag[3,6] <- x[[3]][1]
plot_nn_sag[3,7] <- x[[3]][2]

x <- test_nn_sag(c(3))
plot_nn_sag[4,2] <- x[[1]][1]
plot_nn_sag[4,3] <- x[[1]][2]
plot_nn_sag[4,4] <- x[[2]][1]
plot_nn_sag[4,5] <- x[[2]][2]
plot_nn_sag[4,6] <- x[[3]][1]
plot_nn_sag[4,7] <- x[[3]][2]

x <- test_nn_sag(c(4))
plot_nn_sag[5,2] <- x[[1]][1]
plot_nn_sag[5,3] <- x[[1]][2]
plot_nn_sag[5,4] <- x[[2]][1]
plot_nn_sag[5,5] <- x[[2]][2]
plot_nn_sag[5,6] <- x[[3]][1]
plot_nn_sag[5,7] <- x[[3]][2]


x <- test_nn_sag(c(1,1))
plot_nn_sag[6,2] <- x[[1]][1]
plot_nn_sag[6,3] <- x[[1]][2]
plot_nn_sag[6,4] <- x[[2]][1]
plot_nn_sag[6,5] <- x[[2]][2]
plot_nn_sag[6,6] <- x[[3]][1]
plot_nn_sag[6,7] <- x[[3]][2]

x <- test_nn_sag(c(1,2))
plot_nn_sag[7,2] <- x[[1]][1]
plot_nn_sag[7,3] <- x[[1]][2]
plot_nn_sag[7,4] <- x[[2]][1]
plot_nn_sag[7,5] <- x[[2]][2]
plot_nn_sag[7,6] <- x[[3]][1]
plot_nn_sag[7,7] <- x[[3]][2]

x <- test_nn_sag(c(1,3))
plot_nn_sag[8,2] <- x[[1]][1]
plot_nn_sag[8,3] <- x[[1]][2]
plot_nn_sag[8,4] <- x[[2]][1]
plot_nn_sag[8,5] <- x[[2]][2]
plot_nn_sag[8,6] <- x[[3]][1]
plot_nn_sag[8,7] <- x[[3]][2]

x <- test_nn_sag(c(2,3))
plot_nn_sag[9,2] <- x[[1]][1]
plot_nn_sag[9,3] <- x[[1]][2]
plot_nn_sag[9,4] <- x[[2]][1]
plot_nn_sag[9,5] <- x[[2]][2]
plot_nn_sag[9,6] <- x[[3]][1]
plot_nn_sag[9,7] <- x[[3]][2]

x <- test_nn_sag(c(2,4))
plot_nn_sag[10,2] <- x[[1]][1]
plot_nn_sag[10,3] <- x[[1]][2]
plot_nn_sag[10,4] <- x[[2]][1]
plot_nn_sag[10,5] <- x[[2]][2]
plot_nn_sag[10,6] <- x[[3]][1]
plot_nn_sag[10,7] <- x[[3]][2]


x <- test_nn_sag(c(1,1,2))
plot_nn_sag[11,2] <- x[[1]][1]
plot_nn_sag[11,3] <- x[[1]][2]
plot_nn_sag[11,4] <- x[[2]][1]
plot_nn_sag[11,5] <- x[[2]][2]
plot_nn_sag[11,6] <- x[[3]][1]
plot_nn_sag[11,7] <- x[[3]][2]

x <- test_nn_sag(c(1,2,2))
plot_nn_sag[12,2] <- x[[1]][1]
plot_nn_sag[12,3] <- x[[1]][2]
plot_nn_sag[12,4] <- x[[2]][1]
plot_nn_sag[12,5] <- x[[2]][2]
plot_nn_sag[12,6] <- x[[3]][1]
plot_nn_sag[12,7] <- x[[3]][2]

########################################

plot_nn_rprop
plot_nn_back
plot_nn_sag

#################### random forest #####################
plot_rf <- as.data.frame(matrix(c(3:14), nrow = 20, ncol = 7))
names(plot_rf)[1] <- "node_size"
names(plot_rf)[2] <- "RMSE_mse"
names(plot_rf)[3] <- "MAE_mse"
names(plot_rf)[4] <- "RMSE_quantile_regr"
names(plot_rf)[5] <- "MAE_quantile_regr"
names(plot_rf)[6] <- "RMSE_la.quantile_regr"
names(plot_rf)[7] <- "MAE_la.quantile_regr"


for (i in seq(1,20,by = 1)){
  plot_rf[i,1] <- i
  output_list_rf <- c()
  cv_result_rf <- lapply(folds, function(x){
    train_data <- train_rf_cooling[-x,]
    test_data <- train_rf_cooling[x,]
    #print(test_data)
    model <- rfsrc(rate~outside_temp + start_temp, data=train_data, 
                   mtry=3,ntree = 5000,importance = TRUE,nodesize=i,splitrule = "mse",ntime = 30)
    #print(model)
    prediction <- predict(model, test_data)
    print(prediction$predicted)
    #print(test_data$b)
    return(mmetric(test_data$rate,prediction$predicted,c("RMSE","MAE")))
  })
  #print(cv_result_rf)
  output1 <- rowMeans(cbind(cv_result_rf$Fold1,cv_result_rf$Fold2,
                            cv_result_rf$Fold3,cv_result_rf$Fold4,
                            cv_result_rf$Fold5,cv_result_rf$Fold6,
                            cv_result_rf$Fold7))
  plot_rf[i,2] <- output1[[1]]
  plot_rf[i,3] <- output1[[2]]
  print(output1)
  
  cv_result_rf1 <- lapply(folds, function(x){
    train_data <- train_rf_cooling[-x,]
    test_data <- train_rf_cooling[x,]
    #print(test_data)
    model <- rfsrc(rate~outside_temp + start_temp, data=train_data, 
                   mtry=3,ntree = 5000,importance = TRUE,nodesize=i,splitrule = "quantile.regr",ntime = 30)
    #print(model)
    prediction <- predict(model, test_data)
    print(prediction$predicted)
    #print(test_data$b)
    return(mmetric(test_data$rate,prediction$predicted,c("RMSE","MAE")))
  })
  #print(cv_result_rf)
  output2<- rowMeans(cbind(cv_result_rf1$Fold1,cv_result_rf1$Fold2,
                           cv_result_rf1$Fold3,cv_result_rf1$Fold4,
                           cv_result_rf1$Fold5,cv_result_rf1$Fold6,
                           cv_result_rf1$Fold7))
  plot_rf[i,4] <- output2[[1]]
  plot_rf[i,5] <- output2[[2]]
  
  cv_result_rf3 <- lapply(folds, function(x){
    train_data <- train_rf_cooling[-x,]
    test_data <- train_rf_cooling[x,]
    #print(test_data)
    model <- rfsrc(rate~outside_temp + start_temp, data=train_data, 
                   mtry=3,ntree = 5000,importance = TRUE,nodesize=i,splitrule = "la.quantile.regr",ntime = 30)
    #print(model)
    prediction <- predict(model, test_data)
    print(prediction$predicted)
    #print(test_data$b)
    return(mmetric(test_data$rate,prediction$predicted,c("RMSE","MAE")))
  })
  #print(cv_result_rf)
  output2<- rowMeans(cbind(cv_result_rf3$Fold1,cv_result_rf3$Fold2,
                           cv_result_rf3$Fold3,cv_result_rf3$Fold4,
                           cv_result_rf3$Fold5,cv_result_rf3$Fold6,
                           cv_result_rf3$Fold7))
  plot_rf[i,6] <- output2[[1]]
  plot_rf[i,7] <- output2[[2]]
}




write.csv(plot_rf, file = "rf_cooling_cv.csv")

ggplot(plot_rf, aes(node_size)) + 
  geom_line(aes(y = RMSE_mse, colour = "mse")) + 
  geom_line(aes(y = RMSE_quantile_regr, colour = "quantile_regr")) +
  geom_line(aes(y = RMSE_la.quantile_regr, colour = "la_quantile_regr")) +
  labs(title = "RMSE vs node size (cooling)") + xlab("node size") +
  ylab("RMSE")+theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                     title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "RMSE vs node size (cooling)", x = "Node size", y = "Error",color = "Records")+scale_color_hue(labels = c("la.quantile regression","mse", "quantile regression"))



