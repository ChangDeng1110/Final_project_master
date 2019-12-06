draw_cooling <-list_final_cooling[[2]]
draw_cooling['time'] <- seq(0,600,by = 15)



ggplot(list_caluculate_cooling[[1]],aes(x=Timestamp))+
  geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
  geom_point(aes(y = SA_temp, colour = "SA_temp"))+
  geom_point(aes(y = Room_temp, colour = "Room_temp"))+theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                                                             title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "Temperature change", x = "Time", y = "Temperature (Celsius)",color = "Records")+scale_color_hue(labels = c("Outside temp", "Inside temp",'HVAC temp'))


plot(draw_cooling$time, draw_cooling$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Cooling",
     xlab="Time (min)", ylab="Temperature (Celsius)",cex.main=2, cex.lab=1.25, cex.axis=2)
lines(draw_cooling$time, draw_cooling$temprature, lty = 1, lwd = 1,col = "red")


draw_heating <- list_final[[2]]
draw_heating['time'] <- seq(0,60,by = 15)
plot(draw_heating$time, draw_heating$temprature, type = "b", lty = 1, lwd = 1, col = "black",main="Heating",
     xlab="Time (min)", ylab="Temperature (Celsius)",cex.main=2, cex.lab=1.25, cex.axis=2)
lines(draw_heating$time, draw_heating$temprature, lty = 1, lwd = 1,col = "red")


ggplot(list_simulation_heating[[117]],aes(x=time))+
  geom_line(aes(y = start_temp,colour = "start_temp"),size=0.5)+
  geom_point(aes(y = start_temp, colour = "start_temp"),size=1)+
  geom_line(aes(y = simu_ave_temp,colour = "simu_ave_temp"),size=0.5)+
  geom_point(aes(y = simu_ave_temp, colour = "simu_ave_temp"),size=1)+
  geom_line(aes(y = simu_high_temp,colour = "simu_high_temp"),size=1,linetype="dotted")+
  geom_point(aes(y = simu_high_temp, colour = "simu_high_temp"),size=1) + 
  geom_line(aes(y = simu_low_temp,colour = "simu_low_temp"),size=1,linetype="dotted")+
  geom_point(aes(y = simu_low_temp, colour = "simu_low_temp"),size=1)+
  geom_errorbar(aes(ymin=simu_low_temp, ymax=simu_high_temp),
                width=1,position=position_dodge(1),size = 0.3) +theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                                                                      title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "Simulation heating", x = "Time", y = "Temperature (Celsius)",color = "Records")+scale_color_hue(labels = c("average temp", "higher temp",'lower temp','inside temp'))

ggplot(list_simulation_cooling[[56]],aes(x=time))+
  geom_line(aes(y = start_temp,colour = "start_temp"),size=0.5)+
  geom_point(aes(y = start_temp, colour = "start_temp"),size=1)+
  geom_line(aes(y = simu_ave_temp,colour = "simu_ave_temp"),size=0.5)+
  geom_point(aes(y = simu_ave_temp, colour = "simu_ave_temp"),size=1)+
  geom_line(aes(y = simu_high_temp,colour = "simu_high_temp"),size=1,linetype="dotted")+
  geom_point(aes(y = simu_high_temp, colour = "simu_high_temp"),size=1) + 
  geom_line(aes(y = simu_low_temp,colour = "simu_low_temp"),size=1,linetype="dotted")+
  geom_point(aes(y = simu_low_temp, colour = "simu_low_temp"),size=1)+
  geom_errorbar(aes(ymin=simu_low_temp, ymax=simu_high_temp),
                width=1,position=position_dodge(1),size = 0.3) +theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"),
                                                                      title=element_text(size=18,face="bold"),legend.text = element_text(size = 18))+labs(title = "Simulation heating", x = "Time", y = "Temperature (Celsius)",color = "Records")+scale_color_hue(labels = c("average temp", "higher temp",'lower temp','inside temp'))




data <- data.frame(time = seq(1, 10, by = 1),
                   ert = runif(n = 10))

# Turn into date format - added as.Date to origin statement
data$time<-as.Date(data$time, "%d/%m/%y", origin = as.Date("1970-01-01"))

# Verify similar structure to OPs dataset
head(data)
#         time       ert
# 1 1970-01-02 0.4485163
# 2 1970-01-03 0.8100644
# 3 1970-01-04 0.8123895
# 4 1970-01-05 0.7943423
# 5 1970-01-06 0.4398317
# 6 1970-01-07 0.7544752

ggplot(data, aes(time, ert, group=1, na.rm=T))+
  geom_rect(xmin=2,
            xmax=5, ymin=0.2, ymax=0.5, fill="lightgreen", alpha=0.03) +
  geom_line()+
  labs(x="", y="ert")+
  geom_hline(aes(yintercept=0.5), colour="#990000", linetype="dashed")

pt1.plot


#sample data
set.seed(666)
dat <- data.frame(x = seq(as.POSIXct('2011-03-27 00:00:00'), 
                          len= (n=24), by="1 hour"), y = cumsum(rnorm(n)))
#Breaks for background rectangles
rects <- data.frame(xstart = as.POSIXct('2011-03-27 15:00:00'), 
                    xend = as.POSIXct('2011-03-27 18:00:00'))

library(ggplot2)
ggplot() + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf), alpha = 0.4) + 
  geom_line(data = dat, aes(x,y))




plot(print_table_opt1[,2], type="o", col="blue", pch="o", lty=1, ylim=c(15,26),
     main = "Monash model",xlab = "Min",ylab = "Temperature",cex.main=2, cex.lab=1.25, cex.axis=2)
lines(print_table_opt1[,1], print_table_opt1[,2], col="blue",lty=1)
abline(h=22, col="blue",lty=2)
abline(h=21, col="blue",lty=2)
legend(1, 26, legend=c("monash_model",'old temp_set_point'),
       col=c("blue", 'blue'), lty=c(1,2), cex=1)


plot(print_table_opt1[,3], type="o", col="red", pch="*", lty=1, ylim=c(13,26),
     main = "Optimisation model",xlab = "Min",ylab = "Temperature",cex.main=2, cex.lab=1.25, cex.axis=2)
abline(h=22, col="blue",lty=2)
abline(h=21, col="blue",lty=2)
abline(h=13.79, col="red",lty=2)
legend(1, 26, legend=c("OPT_model",'old temp_set_point',"new temp_set_point"),
       col=c("red", 'blue', 'red'), lty=c(1,2,2), cex=1)

