draw_cooling <-list_final_cooling[[2]]
draw_cooling['time'] <- seq(0,600,by = 15)

plot(draw_cooling$time, draw_cooling$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Cooling",
     xlab="Time", ylab="Temperature")
lines(draw_cooling$time, draw_cooling$temprature, lty = 1, lwd = 1,col = "red")


draw_heating <- list_final[[2]]
draw_heating['time'] <- seq(0,60,by = 15)
plot(draw_heating$time, draw_heating$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Heating",
     xlab="Time", ylab="Temperature")
lines(draw_heating$time, draw_heating$temprature, lty = 1, lwd = 1,col = "red")
