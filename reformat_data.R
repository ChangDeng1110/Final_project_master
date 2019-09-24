ac_1.4 <- read.csv("datasets/AC_1.4_1.21.csv",header = T,stringsAsFactors = F)
ac_1.4$Date.Time <- as.POSIXct(ac_1.4[,1], format = "%d/%m/%Y %I:%M:%S %p")
ac_1.4 <- ac_1.4[order(ac_1.4$Date.Time),]

list_roof <- ac_1.4[16946:38017,c(1,5)]

combine_data <- function(path1,path2){
  a = list.files(path1)
  dir = paste(path2,a,sep="")
  n = length(dir)
  merge.data = read.csv(file = dir[1],header=T,sep=",")
  for (i in 2:n){
    new.data = read.csv(file = dir[i], header=T, sep=",")
    merge.data = rbind(merge.data,new.data)
  }
  return (merge.data)
}

co2 <- combine_data("./datasets/co2","./datasets/co2/")
rm <- combine_data("./datasets/RMtemp/RM","./datasets/RMtemp/RM/")
sa <- combine_data("./datasets/SAtemp/SA","./datasets/SAtemp/SA/")

co2$Timestamp <- as.POSIXct(co2[,1], format = "%d-%b-%y %I:%M:%S %p")
rm$Timestamp <- as.POSIXct(rm[,1], format = "%d/%m/%Y %H:%M")
sa$Timestamp <- as.POSIXct(sa[,1], format = "%d/%m/%Y %H:%M")

#final1 <- merge(co2,rm,by="Timestamp")
final2 <- merge(rm,sa,by="Timestamp")
names(list_roof)[names(list_roof) == "Date.Time"] <- "Timestamp"
#list_roof <- colnames("Timestamp","outside")
final3 <- merge(final2,list_roof,by="Timestamp")
final_data <- final3[35:20910,]

names(final_data)[names(final_data) == "Bld_64_DDC_1.1_Rm_Temp..C."] <- "Room_temp"
names(final_data)[names(final_data) == "Bld_64_DDC_1.1_S.Air_Temp..C."] <- "SA_temp"
names(final_data)[names(final_data) == "Bld81_RoofOATemp"] <- "Roof_temp"
final_data$Room_temp= as.numeric(as.character(final_data$Room_temp))
final_data$SA_temp = as.numeric(as.character(final_data$SA_temp))
final_data$Roof_temp = as.numeric(as.character(final_data$Roof_temp))


name <- "2017/6/28"
days <- c()
index <- 1

for (i in 1:lengths(final_data)[1]){
  name_got <- paste(as.character(year(final_data[i,1])),as.character(month(final_data[i,1])),as.character(day(final_data[i,1])), 
                sep = "/", collapse = NULL)
  if (name == name_got){
    days[i] <- index
  }else{
    name <- name_got
    index <- index + 1
    days[i] <- index
  }
}

final_data["days"] <- days

per_day_temp <- list()
for (i in 1:218){
  a <- subset(final_data, days == i)
  weekday <- weekdays(as.Date(a[1,1]))
  a["weekday"] <- rep(weekday,lengths(a)[1])
  per_day_temp[[i]] <- a
}

plot_temp <- list()
for (i in 1:218){
  x <- ggplot(per_day_temp[[i]],aes(x=Timestamp))+
    geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
    geom_point(aes(y = SA_temp, colour = "SA_temp"))+
    geom_point(aes(y = Room_temp, colour = "Room_temp"))
  plot_temp[[i]] <- x
}

pdf("temprature_all.pdf",family="GB1")
for(i in 1:218){
  plot(plot_temp[[i]])
}
dev.off()
#detach(x)

write.csv(final_data, file = "temprature_all.csv")


