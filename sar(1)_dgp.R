library(smooth)
library(forecast)

set.seed(1)

# lengths of series
length_ss <- 2400
length_ms_hom_short <- 24
length_ms_hom_long <- 240
length_ms_het <- 240

series_per_dataset <- 100
frequency <- 12
no_of_datasets <- 1000
burn_in <- 100


# create the data folder if not existing
dir.create(file.path(".", "data"), showWarnings = FALSE)

seasonal_arima_mod <- Arima(USAccDeaths, seasonal=c(1,0,0))

########### SS and MS-Hom-Short Scenarios
full_length_series = NULL
for (dataset_index in 1:no_of_datasets){
  ts <- simulate(seasonal_arima_mod, nsim=length_ss + burn_in, seed=dataset_index)
  ts <- ts[(length(ts) - length_ss + 1) : length(ts)]
  full_length_series <- rbind(full_length_series, t(ts))
}

# SS scenario
required_multiples_list <- list(1,2,3,4,5,10,20,30,40,50,75,100)
for (dataset_index in 1:no_of_datasets){
  output_path_ss <- paste0("./data/sar1_dgp_ss_dataset_", dataset_index, ".txt")
  ts <- full_length_series[dataset_index,]
  for (multiple in required_multiples_list){
    length <- length_ms_hom_short * multiple
    series_chunk <- ts[1:length]
    write.table(x=t(series_chunk), file=output_path_ss, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}

# MS-Hom-Short scenario
for (dataset_index in 1:no_of_datasets){
  output_path_ms_hom_short <- paste0("./data/sar1_dgp_ms_hom_short_dataset_", dataset_index, ".txt")
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
    ts <- simulate(seasonal_arima_mod, nsim=length_ms_hom_long + burn_in, seed=((dataset_index-1)*series_per_dataset + series))
    ts <- ts[(length(ts) - length_ms_hom_long + 1) : length(ts)]
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
  output_path_ms_hom_long <- paste0("./data/sar1_dgp_ms_hom_long_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_hom_long, row.names = FALSE, col.names = FALSE, sep=",")
}

# MS-Het scenario
for (dataset_index in 1:no_of_datasets){
  full_length_series <- NULL
  for (series in 1:series_per_dataset) {
    AR <- runif(1, -0.5, 0.5)
    ts <- sim.ssarima(ar.orders=c(0,1), i.orders=c(0,0), ma.orders=c(0,0), obs=(length_ms_het+burn_in), nsim=series_per_dataset, AR=c(0,AR), lags=c(0,frequency), frequency = frequency, bounds = "admissible")$data
    ts <- ts[(length(ts) - length_ms_het + 1) : length(ts)]

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
  output_path_ms_het <- paste0("./data/sar1_dgp_ms_het_dataset_", dataset_index, ".txt")
  write.table(x=full_length_series, file=output_path_ms_het, row.names = FALSE, col.names = FALSE, sep=",")
}
