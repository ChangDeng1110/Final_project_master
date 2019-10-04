################# Monday ###################
set.seed(500)
temp_75_mid <- read.csv("temprature_all.csv")[c(121:169),]
names(temp_75_mid)[3] <- "start_temp"
names(temp_75_mid)[5] <- "outside_temp"
rownames(temp_75_mid) <- 1:lengths(temp_75_mid)

tem_set_point1 <- 22
tem_set_point2 <- 21
print_table_opt1 <- as.data.frame(matrix(c(0), nrow = 720, ncol = 7))
names(print_table_opt1)[1] <- "time"
names(print_table_opt1)[2] <- "model_1"
names(print_table_opt1)[3] <- "model_2"
names(print_table_opt1)[4] <- "outside_temp"
names(print_table_opt1)[5] <- "SA_temp"
names(print_table_opt1)[6] <- "low_temp"
names(print_table_opt1)[7] <- "high_temp"

print_table_opt1[,1] <- c(1:720)
print_table_opt1[1,2] <- temp_75_mid[1,3]
print_table_opt1[1,3] <- temp_75_mid[1,3]
print_table_opt1[1,6] <- temp_75_mid[1,3]
print_table_opt1[1,7] <- temp_75_mid[1,3]

start_20 <- temp_75_mid[1:48,]
print_table_opt1[,4] <- rep(start_20[1:48,5], c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15))
print_table_opt1[,5] <- rep(start_20[1:48,4], c(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15))

control <- 0
heating_x <- 0
cooling_x <- 0
heating_time_1 <- 0 
heating_time_2 <- 0


for (i in 1:720){
  temp_now <- print_table_opt1[i,2]
  print(temp_now)
  if (temp_now >= tem_set_point1){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,2], 
                                "outside_temp" = print_table_opt1[i,4])
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    control <- 1
    heating_x <- 1
    print(cool_ave)
  }
  if (temp_now <= tem_set_point2 ){
    use_data <- data.frame("start_temp" = print_table_opt1[i,2], 
                           "outside_temp" = print_table_opt1[i,4])
    heat <- boot(data=train_rf_heating, statistic=heating_model, 
                 R=10, formula=rate ~ start_temp + outside_temp)
    
    heat_ave <- mean(heat$t)
    control <- 2
    cooling_x <- 1
    print(heat_ave)
  }
  if (heating_x == 15){
    use_data <- data.frame("start_temp" = print_table_opt1[i,2], 
                           "outside_temp" = print_table_opt1[i,4])
    heat <- boot(data=train_rf_heating, statistic=heating_model, 
                 R=10, formula=rate ~ start_temp + outside_temp)
    heat_ave <- mean(heat$t)
    print(heat_ave)
    heating_x <- 1
  }
  
  if (cooling_x == 15){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,2], 
                                "outside_temp" = print_table_opt1[i,4])
    print(use_data_cool)
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
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


for (i in 1:30){
  use_data_cool <- data.frame("start_temp" = print_table_opt1[i,3], 
                              "outside_temp" = print_table_opt1[i,4])
  #print(use_data_cool)
  if (i %% 15 == 0 | i == 1){
    print(use_data_cool)
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    print(cool_ave)
  }
  reach_temp <- print_table_opt1[i,3] + cool_ave
  print_table_opt1[i+1,3] <- reach_temp
}

finish_data_test <- 0

for (k in 30:120){
  use_data_cool <- data.frame("start_temp" = print_table_opt1[k,3], 
                              "outside_temp" = print_table_opt1[k,4])
  #print(use_data_cool)
  if (k %% 15 == 0){
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
  }
  print(cool_ave)
  tem_temp <- print_table_opt1[k,3] + cool_ave
  print_table_opt1[k+1,3] <- tem_temp
  
  use_data <- data.frame("start_temp" = print_table_opt1[k+1,3], 
                         "outside_temp" = print_table_opt1[k+1,4])
  #print(use_data)
  heat <- boot(data=train_rf_heating, statistic=heating_model, 
               R=10, formula=rate ~ start_temp + outside_temp)
  heat_ave <- mean(heat$t)
  print(heat_ave)
  index_heating_model <- 1
  
  for(m in (k+1):119){
    if (index_heating_model %% 15 == 0){
      use_data <- data.frame("start_temp" = print_table_opt1[m,3], 
                             "outside_temp" = print_table_opt1[m,4])
      
      heat <- boot(data=train_rf_heating, statistic=heating_model, 
                   R=10, formula=rate ~ start_temp + outside_temp)
      heat_ave <- mean(heat$t) 
    }
    index_heating_model <- index_heating_model + 1
    print(heat_ave)
    reach_temp <- print_table_opt1[m,3] + heat_ave
    print_table_opt1[m+1,3] <- reach_temp
    diff <- 120 - m - 1
    print(diff)
    if (reach_temp >= tem_set_point2){
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

for (i in 119:720){
  temp_now <- print_table_opt1[i,3]
  print(temp_now)
  if (temp_now >= tem_set_point1){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,3], 
                                "outside_temp" = print_table_opt1[i,4])
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    control <- 1
    heating_x <- 1
    print(cool_ave)
  }
  if (temp_now <= tem_set_point2 ){
    use_data <- data.frame("start_temp" = print_table_opt1[i,3], 
                           "outside_temp" = print_table_opt1[i,4])
    heat <- boot(data=train_rf_heating, statistic=heating_model, 
                 R=10, formula=rate ~ start_temp + outside_temp)
    
    heat_ave <- mean(heat$t)
    control <- 2
    cooling_x <- 1
    print(heat_ave)
  }
  if (heating_x == 15){
    use_data <- data.frame("start_temp" = print_table_opt1[i,3], 
                           "outside_temp" = print_table_opt1[i,4])
    heat <- boot(data=train_rf_heating, statistic=heating_model, 
                 R=10, formula=rate ~ start_temp + outside_temp)
    heat_ave <- mean(heat$t)
    print(heat_ave)
    heating_x <- 1
  }
  
  if (cooling_x == 15){
    use_data_cool <- data.frame("start_temp" = print_table_opt1[i,3], 
                                "outside_temp" = print_table_opt1[i,4])
    print(use_data_cool)
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    print(cool_ave)
    cooling_x <- 1
  }
  
  if (control == 1){
    reach_temp <- print_table_opt1[i,3] + cool_ave
    cooling_x <- cooling_x + 1
    print_table_opt1[i+1,3] <- reach_temp
  }
  if (control == 2){
    reach_temp <- print_table_opt1[i,3] + heat_ave
    heating_x <- heating_x + 1
    print_table_opt1[i+1,3] <- reach_temp
    heating_time_2 <- heating_time_2 + 1
  }
}

print_table_opt1["low_set_point"] <- 21
print_table_opt1["time_data"] <- 0
my.lt <- as.POSIXct(temp_75_mid[1,2])

#print_table_opt1$time_data <- as.POSIXct(print_table_opt1[,9], format = "%d-%b-%y %I:%M:%S %p",origin = "2017-06-28 08:00:00 AEST")

for(i in 1:720){
  print_table_opt1[i,9] <- as.character(my.lt)
  my.lt <- my.lt + 60
}


print_table_opt1$time_data <- as.POSIXct(strptime(print_table_opt1$time_data, format = "%Y-%m-%d %H:%M:%S"))
#print_table_opt1$time_data <- as.Time(strptime(print_table_opt1$time_data, format = "%Y-%m-%d %H:%M:%S"))

cols <- c("Simulation of OPT model"="red","Simulation of Monash model"="black","OPT setpoints"="#990000","Monash setpoints"="purple")
bar <- c('Occupy' = 'lightgreen', 'No-occupy' = 'lightblue')
types <- c("Simulation of OPT model"=1,"Simulation of Monash model"=1,"OPT setpoints"=4,"Monash setpoints"=2)
ggplot(print_table_opt1, aes(time_data),na.rm=T,group=1)+
  geom_rect(aes(xmin=as.POSIXct("2017-06-29 08:00:00 AEST"),
                xmax=as.POSIXct("2017-06-29 18:00:00 AEST"), ymin=21, ymax=22, fill="Occupy"), alpha=1) +
  geom_rect(aes(xmin=as.POSIXct("2017-06-29 6:00:00 AEST"),
                xmax=as.POSIXct("2017-06-29 8:00:00 AEST"), ymin=21, ymax=22, fill="No-occupy"), alpha=1) +
  geom_line(aes(time_data, model_2,colour="Simulation of OPT model",linetype="Simulation of OPT model"),size=0.5)+
  geom_line(aes(time_data, model_1,colour="Simulation of Monash model",linetype="Simulation of Monash model"),size=0.5)+
  geom_line(aes(time_data, low_set_point,colour="OPT setpoints",linetype="OPT setpoints"),size=1)+
  geom_hline(aes(yintercept=22,colour="Monash setpoints", linetype="Monash setpoints"),size=1)+
  geom_hline(aes(yintercept=21, colour="Monash setpoints", linetype="Monash setpoints"),size=1)+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
        title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+
  labs(title = "Friday (Schedule)", x = "Time", y = "Temperature (Celsius)",legend.text = element_text(size = 18))+
  scale_colour_manual(name="line",values=cols)+
  scale_linetype_manual(name = "line",values = types)+scale_fill_manual(name="Space",values=bar)




