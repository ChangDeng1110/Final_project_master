print_table_opt1 <- as.data.frame(matrix(c(0), nrow = 240, ncol = 5))
names(print_table_opt1)[1] <- "time"
names(print_table_opt1)[2] <- "model_1"
names(print_table_opt1)[3] <- "model_2"
names(print_table_opt1)[4] <- "outside_temp"
names(print_table_opt1)[5] <- "SA_temp"
print_table_opt1[,1] <- c(1:240) 
print_table_opt1[1,2] <- tem_set_point1
print_table_opt1[1,3] <- tem_set_point1

start_20 <- temp_75_mid[1:16,]
print_table_opt1[,4] <- rep(start_20[1:16,5], c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15))
print_table_opt1[,5] <- rep(start_20[1:16,4], c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15))

control <- 0
heating_x <- 0
cooling_x <- 0
heating_time_1 <- 0 

for (i in 1:239){
  temp_now <- print_table_opt1[i,2]
  print(temp_now)
  if (temp_now >= tem_set_point1){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,2], 
                                "outside_temp" = print_table_opt1[i,4])
    cool_ave <- predict(cooling_model_svm, use_data_cool)
    control <- 1
    heating_x <- 1
    print(cool_ave)
  }
  if (temp_now <= tem_set_point2 ){
    use_data <- data.frame("start_temp" = print_table_opt1[i,2], 
                           "outside_temp" = print_table_opt1[i,4])
    
    heat_ave <- predict(heating_model_rf, use_data)$predicted
    control <- 2
    cooling_x <- 1
    print(heat_ave)
  }
  if (heating_x == 15){
    use_data <- data.frame("start_temp" = print_table_opt1[i,2], 
                           "outside_temp" = print_table_opt1[i,4])
    heat_ave <- predict(heating_model_rf, use_data)$predicted
    print(heat_ave)
    heating_x <- 1
  }
  
  if (cooling_x == 15){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,2], 
                                "outside_temp" = print_table_opt1[i,4])
    print(use_data_cool)
    cool_ave <- predict(cooling_model_svm, use_data_cool)
    print(cool_ave)
    cooling_x <- 1
  }
  
  if (control == 1){
    reach_temp <- print_table_opt1[i,2] + cool_ave
    cooling_x <- cooling_x + 1
    print_table_opt1[i+1,2] <- reach_temp
  }
  if (control == 2){
    reach_temp <- print_table_opt1[i,2] + heat_ave
    heating_x <- heating_x + 1
    print_table_opt1[i+1,2] <- reach_temp
    heating_time_1 <- heating_time_1 + 1
  }
}



plot(print_table_opt1[,1], print_table_opt1[,2], type="o", col="blue", pch="o", lty=1, ylim=c(10,27),
     main = "Monash system",xlab = "Min",ylab = "Temperature")
points(print_table_opt1[,1], print_table_opt1[,3], col="red", pch="*")
#lines(print_table_opt1[,1], print_table_opt1[,3], col="red",lty=1)
abline(h=22, col="blue",lty=2)
abline(h=20, col="blue",lty=2)
#abline(h=15.64, col="red",lty=4)
#abline(h=22, col="red",lty=4)
legend(1, 27, legend=c("monash_model",'old temp_set_point'),
       col=c( "blue", 'blue'), lty=c(1,2), cex=0.8)



finish_data_test <- 0
#stop_data <- 100

for (k in 1:240){
  use_data_cool <- data.frame("start_temp" = print_table_opt1[k,3], 
                              "outside_temp" = print_table_opt1[k,4])
  #print(use_data_cool)
  if (k %% 15 == 1){
    cool_ave <- predict(cooling_model_svm, use_data_cool)
  }
  print(cool_ave)
  tem_temp <- print_table_opt1[k,3] + cool_ave
  print_table_opt1[k+1,3] <- tem_temp
  
  use_data <- data.frame("start_temp" = print_table_opt1[k+1,3], 
                         "outside_temp" = print_table_opt1[k+1,4])
  #print(use_data)
  heat_ave <- predict(heating_model_rf, use_data)$predicted
  print(heat_ave)
  index_heating_model <- 1
  
  for(m in (k+1):239){
    if (index_heating_model %% 15 == 0){
      use_data <- data.frame("start_temp" = print_table_opt1[m,3], 
                             "outside_temp" = print_table_opt1[m,4])
      
      heat_ave <- predict(heating_model_rf, use_data)$predicted
    }
    index_heating_model <- index_heating_model + 1
    print(heat_ave)
    reach_temp <- print_table_opt1[m,3] + heat_ave
    print_table_opt1[m+1,3] <- reach_temp
    diff <- 240 - m - 1
    print(diff)
    if (reach_temp >= (tem_set_point2+tem_set_point1)/2){
      if (diff <= 2){
        finish_data_test <- 1
      }else{
        break
      }
    }
  }
  print("@@@@")
  if (finish_data_test == 1){
    break
  }
  
}


plot(print_table_opt1[,1], print_table_opt1[,3], type="o", col="red", pch="o", lty=1, ylim=c(10,27),
     main = "Optimization_model",xlab = "Min",ylab = "Temperature")
#points(print_table_opt1[,1], print_table_opt1[,3], col="red", pch="*")
lines(print_table_opt1[,1], print_table_opt1[,3], col="red",lty=1)
#abline(h=22, col="blue",lty=2)
abline(h=20, col="blue",lty=2)
abline(h=15.64, col="red",lty=4)
abline(h=22, col="blue",lty=4)
legend(1, 27, legend=c("Optimization_model",'new temp_set_point'),
       col=c( "red", 'red'), lty=c(1,4), cex=0.8)

