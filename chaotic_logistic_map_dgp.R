set.seed(1)

# lengths of series
length_ss <- 6000
length_ms_hom_short <- 60
length_ms_hom_long <- 600
length_ms_het <- 600
length_group_feature <- 600

series_per_dataset <- 100
frequency <- 12
no_of_datasets <- 1000
burn_in <- 40

# create the data folder if not existing
dir.create(file.path(".", "data"), showWarnings = FALSE)
initial_value <- 0.5
coefficient <- 3.6

########### SS and MS-Hom-Short Scenarios
full_length_series <- NULL
for (dataset_index in 1:no_of_datasets){
  ts <- numeric(length_ss + burn_in)
  ts[1] <- initial_value

  for (i in 2:(length_ss + burn_in)){
    noise <- rnorm(1)
    ts[i] <- max(coefficient*ts[i-1]*(1-ts[i-1]) + noise/10 , 0)
  }

  ts <- ts[(burn_in + 1):length(ts)]
  full_length_series <- rbind(full_length_series, ts)
}

# SS scenario
required_multiples_list <- list(1,3,5,20,40,75,100)
for (dataset_index in 1:no_of_datasets){
  output_path_ss <- paste0("./data/chaotic_logistic_map_dgp_ss_dataset_", dataset_index, ".txt")
  ts <- full_length_series[dataset_index,]
  for (multiple in required_multiples_list){
    length <- length_ms_hom_short * multiple
    series_chunk <- ts[1:length]
    write.table(x=t(series_chunk), file=output_path_ss, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}

# MS-Hom-Short scenario
for (dataset_index in 1:no_of_datasets){
  output_path_ms_hom_short <- paste0("./data/chaotic_logistic_map_dgp_ms_hom_short_dataset_", dataset_index, ".txt")
  ts <- full_length_series[dataset_index,]
  last_index <- 0
  while (last_index < length_ss){
    start <- last_index + 1
    last_index <- start + length_ms_hom_short - 1
    series_chunk <- ts[start:last_index]
    write.table(x=t(series_chunk), file=output_path_ms_hom_short, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}

# MS-Hom-Long scenario
for (dataset_index in 1:no_of_datasets){
  full_length_series <- NULL
  for (series in 1:series_per_dataset) {
    ts <- numeric(length_ms_hom_long + burn_in)
    ts[1] <- initial_value

    for (i in 2:(length_ms_hom_long + burn_in)){
      noise <-  rnorm(1)
      ts[i] <- max(coefficient*ts[i-1]*(1-ts[i-1]) + noise/10 , 0)
    }

    ts <- ts[(burn_in + 1):length(ts)]

    full_length_series <- rbind(full_length_series, ts)
  }

  # write the series of a particular dataset into the file
  output_path_ms_hom_long <- paste0("./data/chaotic_logistic_map_dgp_ms_hom_long_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_hom_long, row.names = FALSE, col.names = FALSE, sep=",")
}

# MS-Het scenario
for (dataset_index in 1:no_of_datasets){
  full_length_series <- NULL
  for (series in 1:series_per_dataset) {
    ts <- numeric(length_ms_het + burn_in)

    initial_value <- runif(1,min=0,max=1)
    coefficient <- runif(1, min=0, max=4)

    ts[1] <- initial_value

    for (i in 2:(length_ms_het + burn_in)){
      noise <-  rnorm(1)
      ts[i] <- max(coefficient*ts[i-1]*(1-ts[i-1]) + noise/10 , 0)
    }

    ts <- ts[(burn_in + 1):length(ts)]
    full_length_series <- rbind(full_length_series, ts)
  }

  # write the series of a particular dataset into the file
  output_path_ms_het <- paste0("./data/chaotic_logistic_map_dgp_ms_het_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_het, row.names = FALSE, col.names = FALSE, sep=",")
}

# Group Feature scenario
no_of_dgps <- 4
parameters_list <- list()
for(dgp in 1:no_of_dgps){
  initial_value <- runif(1,min=0,max=1)
  coefficient <- runif(1, min=0, max=4)
  parameters <- c(initial_value, coefficient)
  parameters_list[[dgp]] <- parameters
}


for (dataset_index in 1:no_of_datasets){
  full_length_series <- NULL
  for (series in 1:series_per_dataset) {
    parameter_index <- ceiling(series/(series_per_dataset/no_of_dgps))
    parameters <- parameters_list[[parameter_index]]

    ts <- numeric(length_group_feature + burn_in)
    
    initial_value <- parameters[1]
    coefficient <- parameters[2]
    
    ts[1] <- initial_value
    
    for (i in 2:(length_group_feature + burn_in)){
      noise <-  rnorm(1)
      ts[i] <- max(coefficient*ts[i-1]*(1-ts[i-1]) + noise/10 , 0)
    }
    
    ts <- ts[(burn_in + 1):length(ts)]
    full_length_series <- rbind(full_length_series, ts)
  }

  # write the series of a particular dataset into the file
  output_path_group_feature = paste0("./data/chaotic_logistic_map_dgp_group_feature_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_group_feature, row.names = FALSE, col.names = FALSE, sep=",")
}

