############### heating dataset ####################
data_model_heating <- read.csv("heating_rate.csv")[,c(2,3,5)]
sample_size <- floor(0.8 * nrow(data_model_heating))
set.seed(1)
train_ind <- sample(seq_len(nrow(data_model_heating)), size = sample_size)
train_rf_heating <- data_model_heating[train_ind, ]
test_rf_heating <- data_model_heating[-train_ind, ]
data_model_rf_heating <- data_model_heating

maxs <- apply(data_model_heating[,1:2], 2, max)
mins <- apply(data_model_heating[,1:2], 2, min)
scaled <- as.data.frame(scale(data_model_heating[,1:2], center = mins, scale = maxs - mins))
data_model_heating[,1:2] <- scaled
train_heating <- data_model_heating[train_ind, ]
test_heating <- data_model_heating[-train_ind, ]

############### cooling dataset ####################
data_model_cooling <- read.csv("cooling_rate.csv")[,c(2,3,5)]
sample_size <- floor(0.8 * nrow(data_model_cooling))
set.seed(1)
train_ind <- sample(seq_len(nrow(data_model_cooling)), size = sample_size)
train_rf_cooling <- data_model_cooling[train_ind, ]
test_rf_cooling <- data_model_cooling[-train_ind, ]
data_model_rf_cooling <- data_model_cooling

maxs <- apply(data_model_cooling[,1:2], 2, max)
mins <- apply(data_model_cooling[,1:2], 2, min)
scaled <- as.data.frame(scale(data_model_cooling[,1:2], center = mins, scale = maxs - mins))
data_model_cooling[,1:2] <- scaled
train_cooling <- data_model_cooling[train_ind, ]
test_cooling <- data_model_cooling[-train_ind, ]

############## final test and train dataset ############
lengths(train_rf_heating) # heating for random forest
lengths(test_rf_heating) # heating for random forest
lengths(train_heating) # heating for other
lengths(test_heating) # heating for other

lengths(train_rf_cooling) # cooling for random forest
lengths(test_rf_cooling) # cooling for random forest
lengths(train_cooling) # cooling for other
lengths(test_cooling) # cooling for other

