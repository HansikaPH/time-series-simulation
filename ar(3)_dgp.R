library(forecast)

set.seed(1)

source("./ar_coefficients_generator.R")

lags <- 3 # for AR(3) process
maxRoot <- 5# for a non exploding process

# lengths of series
length_ss <- 1800
length_ms_hom_short <- 18
length_ms_hom_long <- 180
length_ms_het <- 180

series_per_dataset <- 100
frequency <- 12
no_of_datasets <- 1000
burn_in <- 100

# create the data folder if not existing
dir.create(file.path(".", "data"), showWarnings = FALSE)

parameters <- generate_random_arma_parameters(lags, maxRoot)

########### SS and MS-Hom-Short Scenarios
full_length_series <- NULL
for (dataset_index in 1:no_of_datasets){
  ts <- arima.sim(model=list(ar=parameters), n=length_ss, n.start = burn_in)
  
  # normalize to 0 mean and unit variance
  ts <- scale(ts)
  min <- min(ts)
  if (min < 0){
    ts <- ts - min
  }
  if (min < 1){
    ts <- ts + 1
  }
  full_length_series <- rbind(full_length_series, t(ts))
}

# SS scenario
required_multiples_list <- list(1,2,3,4,5,10,20,30,40,50,75,100)
for (dataset_index in 1:no_of_datasets){
  output_path_ss <- paste0("./data/ar3_dgp_ss_dataset_", dataset_index, ".txt")
  ts <- full_length_series[dataset_index,]
  for (multiple in required_multiples_list){
    length <- length_ms_hom_short * multiple
    series_chunk <- ts[1:length]
    write.table(x=t(series_chunk), file=output_path_ss, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}

# MS-Hom-Short scenario
for (dataset_index in 1:no_of_datasets){
  output_path_ms_hom_short <- paste0("./data/ar3_dgp_ms_hom_short_dataset_", dataset_index, ".txt")
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
    ts <- arima.sim(model=list(ar=parameters), n=length_ms_hom_long, n.start = burn_in)
    
    # normalize to 0 mean and unit variance
    ts <- scale(ts)
    min <- min(ts)
    if (min < 0){
      ts <- ts - min
    }
    if (min < 1){
      ts <- ts + 1
    }
    full_length_series <- rbind(full_length_series, t(ts))
  }
  
  # write the series of a particular dataset into the file
  output_path_ms_hom_long <- paste0("./data/ar3_dgp_ms_hom_long_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_hom_long, row.names = FALSE, col.names = FALSE, sep=",")
}

# MS-Het scenario
for (dataset_index in 1:no_of_datasets){
  full_length_series <- NULL
  for (series in 1:series_per_dataset) {
    
    # generate ar coefficients
    parameters <- generate_random_arma_parameters(lags, maxRoot)
    
    ts <- arima.sim(model=list(ar=parameters), n=length_ms_het, n.start = burn_in)
    
    # normalize to 0 mean and unit variance
    ts <- scale(ts)
    min <- min(ts)
    if (min < 0){
      ts <- ts - min
    }
    if (min < 1){
      ts <- ts + 1
    }
    full_length_series <- rbind(full_length_series, t(ts))
  }
  
  # write the series of a particular dataset into the file
  output_path_ms_het <- paste0("./data/ar3_dgp_ms_het_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_het, row.names = FALSE, col.names = FALSE, sep=",")
}
