list_cooling_period <- list()
for (i in 1:115){
  list_cooling_period[[i]] <- final_data[(65 + 96 * (i-1) ):(161 + 96 * (i -1)),]
}

for (j in (1:115)){
  if ( (j%%7 == 4) | (j%%7 == 5) ){
    list_cooling_period[[j]] <- NA
  }
  
}

list_cooling_period[[4]] <- NA
list_cooling_period[[5]] <- NA

list_caluculate_cooling <- list()
index_c <- 1
for (k in (1:115)){
  if (!is.na(list_cooling_period[[k]])){
    list_caluculate_cooling[[index_c]] <- list_cooling_period[[k]]
    index_c <- index_c + 1
  }
}
list_caluculate_cooling[[79]] <- NULL
list_caluculate_cooling[[79]] <- NULL
list_caluculate_cooling[[79]] <- NULL

plot_temp_cooling <- list()
for (i in 1:length(list_caluculate_cooling)){
  x <- ggplot(list_caluculate_cooling[[i]],aes(x=Timestamp))+
    geom_point(aes(y = Roof_temp, colour = "Roof_temp"))+
    geom_point(aes(y = SA_temp, colour = "SA_temp"))+
    geom_point(aes(y = Room_temp, colour = "Room_temp"))
  plot_temp_cooling[[i]] <- x
}

pdf("cooling_day.pdf",family="GB1")
for(i in 1:length(list_caluculate_cooling)){
  plot(plot_temp_cooling[[i]])
}
dev.off()
#detach(x)

####################### find cooling period ######################
cooling_start_point <- function(input_data){
  list_point <- list()
  list_point[[1]] <- NA
  list_point[[2]] <- NA
  for (i in 1:lengths(input_data)[1]-1){
    start_point <- NA
    temp_diff <- as.numeric(input_data[i,2] - input_data[i+1,2])
    temp_diff2 <- as.numeric(input_data[i+1,2] - input_data[i+2,2])
    if (length(temp_diff) != 0){
      if (temp_diff > 0.3){
        if (temp_diff2 > 0.3){
          if((input_data[i+2,2] - input_data[i+3,2]) > 0.2){
            if((input_data[i+3,2] - input_data[i+4,2]) > 0.1){
              start_point <- i
              list_point[[1]] <- start_point
              break
            }
          }
        }
      }
    }
  }
  
  for (i in 1:lengths(input_data)[1]-1){
    finish_point <- NA
    temp_diff3 <- as.numeric(input_data[i+1,2] - input_data[i,2])
    temp_diff4 <- as.numeric(input_data[i+2,2] - input_data[i+1,2])
    if (length(temp_diff3) != 0){
      if (temp_diff3 > 0.5){
        if (temp_diff4 > 0.4){
          finish_point <- i
          list_point[[2]] <- finish_point
          break
        }
      }
    }
  }
  print("===========")
  print(start_point)
  print(finish_point)
  print("===========")
  if (!is.na(list_point[[2]])){
    for(i in (finish_point:(finish_point-10))){
      #print(i)
      if ((input_data[i,2] - input_data[i-1,2]) >= 0){
        if ((input_data[i-1,2] - input_data[i-2,2]) >= 0){
          if ((input_data[i-2,2] - input_data[i-3,2]) >= 0){
            if ((input_data[i-3,2] - input_data[i-4,2]) >= 0){
              #finish_point <- i
              list_point[[2]] <- (i-4)
            }
          }
        }
      }
    }
  }
  print(input_data[list_point[[2]],1])
  return (list_point)
}

get_cooling_period <- function(start,finish,input_data){
  start_time <- c(input_data[start,1])
  finish_time <- c(input_data[finish,1])
  start_temp <- c(input_data[start,2])
  finish_temp <- c(input_data[finish,2])
  outside_temp <- c(input_data[start,4])
  SA_temp <- c(input_data[start,3])
  #print(start_time)
  
  output_frame <- data.frame("start_time" = start_time,
                             "finish_time" = finish_time,
                             "start_temp" = start_temp,
                             "finish_temp" = finish_temp,
                             #"indoor_rate" = indoor_rate,
                             #"outside_temp_diff" = outside_temp_diff,
                             "outside_temp" = outside_temp,
                             "SA_temp" = SA_temp)
  output_frame <- na.omit(output_frame)
  output_frame["temp_diff_SA_RM"] <- output_frame$SA_temp - output_frame$start_temp
  output_frame["temp_diff_SA_out"] <- output_frame$SA_temp - output_frame$outside_temp
  output_frame["temp_diff_RM_out"] <- output_frame$start_temp - output_frame$outside_temp
  
  return (output_frame)
}

#x <- cooling_start_point(list_caluculate_cooling[[1]])
#y <- get_cooling_period(x[[1]],x[[2]],list_caluculate_cooling[[1]])
#get_heating_dataframe(list_caluculate_cooling[[1]][x[[1]],1],
#                      list_caluculate_cooling[[1]][x[[2]],1],list_caluculate_cooling[[1]])

list_final_cooling <- list()
index_final <- 1
for(i in 1:length(list_caluculate_cooling)){
  l <- cooling_start_point(list_caluculate_cooling[[i]])
  #output_final_dataset <- NA
  print(l)
  if (!is.na(l)){
    #print(y)
    data <- get_cooling_period(l[[1]],l[[2]],list_caluculate_cooling[[i]])
    print(data)
    newdata <- get_heating_dataframe(list_caluculate_cooling[[i]][l[[1]],1],
                                     list_caluculate_cooling[[i]][l[[2]],1],list_caluculate_cooling[[i]])
    print(newdata)
    index_final <- index_final + 1
    list_final_cooling[[index_final]] <- newdata
  }
}

cooling_dataframe <- as.data.frame(matrix(NA, nrow = 10000, ncol = 4))
index_set <- 1
for (i in 1:(length(list_final_cooling)-1)){
  print(i)
  if (length(list_final_cooling[[i]])>1){
    for (j in 1:(lengths(list_final_cooling[[i]])[1] - 1)){
      print(j)
      if ((as.numeric(list_final_cooling[[i]][j+1,1]) - as.numeric(list_final_cooling[[i]][j,1])) == 0){
        rate <- 0
      }else{
        rate <- (as.numeric(list_final_cooling[[i]][j+1,1]) - as.numeric(list_final_cooling[[i]][j,1]))/15
      }
      print(rate)
      cooling_dataframe[index_set,1] <- list_final_cooling[[i]][j,1]
      cooling_dataframe[index_set,2] <- list_final_cooling[[i]][j,2]
      cooling_dataframe[index_set,3] <- list_final_cooling[[i]][j,3]
      cooling_dataframe[index_set,4] <- rate
      index_set <- index_set+ 1
    }
  }
}
cooling_dataframe
cooling_dataframe <- na.omit(cooling_dataframe)
names(cooling_dataframe)[1] <- "start_temp"
names(cooling_dataframe)[2] <- "outside_temp"
names(cooling_dataframe)[3] <- "SA_temp"
names(cooling_dataframe)[4] <- "rate"

row_sub = apply(cooling_dataframe, 1, function(row) all(row !=0 ))
##Subset as usual
cooling_dataframe <- cooling_dataframe[row_sub,]

write.csv(cooling_dataframe, file = "cooling_rate.csv")
