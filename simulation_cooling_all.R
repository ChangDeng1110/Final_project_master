list_final_cooling

list_simulation_cooling <- list()

for (i in 2:80){
  simulation_cooling_table <- as.data.frame(matrix(NA, nrow = 50, ncol = 5))
  simulation_cooling_table[c(1:lengths(list_final_cooling[[i]])[1]),5] <- list_final_cooling[[i]][,2]
  names(simulation_cooling_table)[1] <- "start_temp"
  names(simulation_cooling_table)[2] <- "simu_ave_temp"
  names(simulation_cooling_table)[3] <- "simu_high_temp"
  names(simulation_cooling_table)[4] <- "simu_low_temp"
  names(simulation_cooling_table)[5] <- "outside"
  
  simulation_cooling_table[1,1] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,2] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,3] <- list_final_cooling[[i]][1,1]
  simulation_cooling_table[1,4] <- list_final_cooling[[i]][1,1]
  
  for(j in 1:lengths(list_final_cooling[[i]])[1]){
    simulation_cooling_table[j,1] <- list_final_cooling[[i]][j,1]
    use_data_cool <- data.frame("start_temp" = simulation_cooling_table[j,2], 
                           "outside_temp" = simulation_cooling_table[j,5])
    #print(use_data_cool)
    cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                 R=10, formula=rate~outside_temp + start_temp)
    cool_ave <- mean(cool$t)
    simulation_cooling_table[j+1,2] <- simulation_cooling_table[j,2] + cool_ave*15
    if (j == 1){
      cool_high <- quantile(cool$t,probs=c(0.025,0.975))[2]
      cool_low <- quantile(cool$t,probs=c(0.025,0.975))[1]
      simulation_cooling_table[j+1,3] <- simulation_cooling_table[j,2] + cool_high*15
      simulation_cooling_table[j+1,4] <- simulation_cooling_table[j,2] + cool_low*15
    }else{
      use_data_cool <- data.frame("start_temp" = simulation_cooling_table[j,3], 
                                  "outside_temp" = simulation_cooling_table[j,5])
      #print(use_data_cool)
      cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                   R=10, formula=rate~outside_temp + start_temp)
      cool_high <- quantile(cool$t,probs=c(0.025,0.975))[2]
      simulation_cooling_table[j+1,3] <- simulation_cooling_table[j,3] + cool_high*15
      
      use_data_cool <- data.frame("start_temp" = simulation_cooling_table[j,4], 
                                  "outside_temp" = simulation_cooling_table[j,5])
      #print(use_data_cool)
      cool <- boot(data=train_rf_cooling, statistic=cooling_model, 
                   R=10, formula=rate~outside_temp + start_temp)
      cool_low <- quantile(cool$t,probs=c(0.025,0.975))[1]
      simulation_cooling_table[j+1,4] <- simulation_cooling_table[j,4] + cool_low*15
    }
  }
  simulation_cooling_table <- na.omit(simulation_cooling_table)
  list_simulation_cooling[[i]] <- simulation_cooling_table
  print(i)
}


for (i in 2:80){
  list_simulation_cooling[[i]]['time'] <- seq(0,(lengths(list_simulation_cooling[[i]])[1]-1)*15, by = 15)
}


plot_cooling_simulation <- list()
for (i in 2:length(list_simulation_cooling)){
  print(i)
  x <- ggplot(list_simulation_cooling[[i]],aes(x=time))+
    geom_line(aes(y = start_temp,colour = "start_temp"),size=0.5)+
    geom_point(aes(y = start_temp, colour = "start_temp"),size=1)+
    geom_line(aes(y = simu_ave_temp,colour = "simu_ave_temp"),size=0.5)+
    geom_point(aes(y = simu_ave_temp, colour = "simu_ave_temp"),size=1)+
    geom_line(aes(y = simu_high_temp,colour = "simu_high_temp"),size=1,linetype="dotted")+
    geom_point(aes(y = simu_high_temp, colour = "simu_high_temp"),size=1) + 
    geom_line(aes(y = simu_low_temp,colour = "simu_low_temp"),size=1,linetype="dotted")+
    geom_point(aes(y = simu_low_temp, colour = "simu_low_temp"),size=1)+
    geom_errorbar(aes(ymin=simu_low_temp, ymax=simu_high_temp),
                  width=10,position=position_dodge(1),size = 0.3) 
  
  plot_cooling_simulation[[i]] <- x
}

pdf("cooling_simulation.pdf",family="GB1")
for(i in 2:length(plot_cooling_simulation)[1]){
  plot(plot_cooling_simulation[[i]])
}
dev.off()

RMSE_cooling <- c()
RMSE_cooling_high <- c()
RMSE_cooling_low <- c()

for (k in 2:80){
  rmse_value <- RMSE(list_simulation_cooling[[k]][2:lengths(list_simulation_cooling[[k]])[1],'start_temp'],
                     list_simulation_cooling[[k]][2:lengths(list_simulation_cooling[[k]])[1],'simu_ave_temp'])
  rmse_value1 <- RMSE(list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'start_temp'],
                      list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'simu_high_temp'])
  rmse_value2 <- RMSE(list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'start_temp'],
                      list_simulation_heating[[k]][2:lengths(list_simulation_heating[[k]])[1],'simu_low_temp'])
  RMSE_cooling[k-1] <- rmse_value
  RMSE_cooling_high[k-1] <- rmse_value1
  RMSE_cooling_low[k-1] <- rmse_value2
}


result <- c(RMSE_cooling,RMSE_cooling_high,RMSE_cooling_low)
name <- c()
for(x in 1:79){
  name <- c(name, "ave_temp")
}
for(x in 1:79){
  name <- c(name, "high_temp")
}
for(x in 1:79){
  name <- c(name, "low_temp")
}

tbl <- data.frame(
  names = name,
  results = result
)

boxplot(results~names, data=tbl, xlab="cooling model", ylab="RMSE", par(cex.axis=1), par(cex.lab=1),main="RMSE ~ cooling (temperature)",names=c("Average","Higher",'Lower'),
        xlab="Model type", ylab="Error",cex.main=2, cex.lab=1.25, cex.axis=2)





