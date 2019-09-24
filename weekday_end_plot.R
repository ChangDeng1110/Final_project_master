############### get winter day ####################
heating_period_weekday <- list()
heating_period_weekend <- list()
index <- 1


for (i in 1:135){
  if ((per_day_temp[[i]][1,6]!= "Saturday")){
    heating_period_weekday[[index]] <- per_day_temp[[i]]
    index <-  index + 1
  }
}

index2 <- 1
for (i in 1:135){
  if ((per_day_temp[[i]][1,6] == "Saturday")){
    heating_period_weekend[[index2]] <- per_day_temp[[i]]
    index2 <-  index2 + 1
  }
}

################# plot weekdays in winter ######################
plot_winter_weekdays <- list()
for (i in 1:length(heating_period_weekday)[1]){
  x <- ggplot(heating_period_weekday[[i]],aes(x=Timestamp))+
    geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
    geom_point(aes(y = SA_temp, colour = "SA_temp"))+
    geom_point(aes(y = Room_temp, colour = "Room_temp"))
  plot_winter_weekdays[[i]] <- x
}

pdf("winter_weekday.pdf",family="GB1")
for(i in 1:length(plot_winter_weekdays)){
  plot(plot_winter_weekdays[[i]])
}
dev.off()

################ plot weekends in winter ##############
plot_heating_weekend <- list()
for (i in 1:length(heating_period_weekend)[1]){
  x <- ggplot(heating_period_weekend[[i]],aes(x=Timestamp))+
    geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
    geom_point(aes(y = SA_temp, colour = "SA_temp"))+
    geom_point(aes(y = Room_temp, colour = "Room_temp"))
  plot_heating_weekend[[i]] <- x
}

pdf("winter_weekend.pdf",family="GB1")
for(i in 1:length(plot_heating_weekend)[1]){
  plot(plot_heating_weekend[[i]])
}
dev.off()

