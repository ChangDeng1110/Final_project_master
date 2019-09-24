detect_heating_start <- function(input_data){
  start_point <- NA
  for (i in 1:lengths(input_data)[1]-1){
    temp_diff <- as.numeric(input_data[i+1,2] - input_data[i,2])
    #print(temp_diff)
    if (length(temp_diff) != 0){
      if (temp_diff > 0.5){
        start_point <- i
        break
      }
    }
  }
  #print(input_data[start_point,1])
  return (start_point)
}


find_heating_stop <- function(input_data,start_position){
  finish_position <- NA
  for (i in start_position:95){
    if (input_data[i+1,2] <= input_data[i,2]){
      finish_position <- i
      break
    }
  }
  #print(input_data[finish_position,1])
  return (finish_position)
}

find_heating_period <- function(start_p,finish_p,input_data){
  start_time <- c()
  finish_time <- c()
  start_temp <- c()
  finish_temp <- c()
  #indoor_rate <- c()
  outside_temp_diff <- c()
  outside_temp <-  c()
  SA_temp <- c()
  stop_position_mid <- NA
  
  output <- list()
  for (i in start_p:finish_p){
    temp_diff <- as.numeric(input_data[i+1,2] - input_data[i,2])
    #print(temp_diff)
    if (length(temp_diff) != 0){
      if (temp_diff < 0.5){
        stop_position_mid <- i
        break
      }
    }
  }
  start_time <- c(input_data[start_p,1],input_data[stop_position_mid,1])
  finish_time <- c(input_data[stop_position_mid,1],input_data[finish_p,1])
  start_temp <- c(input_data[start_p,2],input_data[stop_position_mid,2])
  finish_temp <- c(input_data[stop_position_mid,2],input_data[finish_p,2])
  outside_temp <- c(input_data[start_p,4],input_data[stop_position_mid,4])
  SA_temp <- c(input_data[start_p,3],input_data[stop_position_mid,3])
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
  
  output[[1]] <- output_frame
  output[[2]] <- stop_position_mid
  return (output)
}

get_heating_dataframe <- function(start,finish,dataset){
  start_index <- NA
  finish_index <- NA
  for (i in 1:lengths(dataset)[1]){
    if (dataset[i,1] == start){
      start_index <- i
    }
    
    if (dataset[i,1] == finish){
      finish_index <- i
    }
  }
  subdata <- dataset[start_index:finish_index,]
  time <- subdata$Timestamp
  temprature <- subdata$Room_temp
  #min <- seq(0,(length(time)-1)*15, by = 15)
  outside <- subdata$Roof_temp
  SA <- subdata$SA_temp
  data_new <- data.frame(temprature,outside,SA)
  return (data_new)
}


x <- detect_heating_start(heating_period_weekday[[95]])
y <- find_heating_stop(heating_period_weekday[[95]],x)
h <- find_heating_period(x,y,heating_period_weekday[[95]])
get_heating_dataframe(h[[1]][1,1],h[[1]][1,2],heating_period_weekday[[95]])

plot_heating <- list()
list_final <- list()
index_final <- 1
for(i in 1:length(heating_period_weekday)){
  rate <- NA
  intercept <- NA
  l <- detect_heating_start(heating_period_weekday[[i]])
  output_final_dataset <- NA
  #print(x)
  if (!is.na(l)){
    p <- find_heating_stop(heating_period_weekday[[i]],l)
    #print(y)
    data <- find_heating_period(l,p,heating_period_weekday[[i]])
    #print(data)
    a <- data[[1]][1,1]
    b <- data[[1]][1,2]
    newdata <- get_heating_dataframe(a,b,heating_period_weekday[[i]])
    index_final <- index_final + 1
    list_final[[index_final]] <- newdata
  }
}

heating_dataframe <- as.data.frame(matrix(NA, nrow = 1000, ncol = 4))
index_set <- 1
for (i in 1:(length(list_final)-1)){
  print("hi")
  if (length(list_final[[i]])>1){
    print('hi')
    for (j in 1:lengths(list_final[[i]])[1]){
      print('hi')
      rate <- (as.numeric(list_final[[i]][j+1,1]) - as.numeric(list_final[[i]][j,1]))/15
      heating_dataframe[index_set,1] <- list_final[[i]][j,1]
      heating_dataframe[index_set,2] <- list_final[[i]][j,2]
      heating_dataframe[index_set,3] <- list_final[[i]][j,3]
      heating_dataframe[index_set,4] <- rate
      index_set <- index_set+ 1
    }
  }
}

heating_dataframe <- na.omit(heating_dataframe)
names(heating_dataframe)[1] <- "start_temp"
names(heating_dataframe)[2] <- "outside_temp"
names(heating_dataframe)[3] <- "SA_temp"
names(heating_dataframe)[4] <- "rate"

write.csv(heating_dataframe, file = "heating_rate.csv")

