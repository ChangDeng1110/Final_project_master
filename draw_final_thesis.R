draw_cooling <-list_final_cooling[[2]]
draw_cooling['time'] <- seq(0,600,by = 15)



ggplot(list_caluculate_cooling[[1]],aes(x=Timestamp))+
  geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
  geom_point(aes(y = SA_temp, colour = "SA_temp"))+
  geom_point(aes(y = Room_temp, colour = "Room_temp"))+theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                                                             title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "Temperature change", x = "Time", y = "Temperature (Celsius)",color = "Records")+scale_color_hue(labels = c("Outside temp", "Inside temp",'HVAC temp'))





plot(draw_cooling$time, draw_cooling$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Cooling",
     xlab="Time", ylab="Temperature")
lines(draw_cooling$time, draw_cooling$temprature, lty = 1, lwd = 1,col = "red")


draw_heating <- list_final[[2]]
draw_heating['time'] <- seq(0,60,by = 15)
plot(draw_heating$time, draw_heating$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Heating",
     xlab="Time", ylab="Temperature")
lines(draw_heating$time, draw_heating$temprature, lty = 1, lwd = 1,col = "red")
