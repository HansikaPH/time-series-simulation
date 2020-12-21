library(tsDyn)

set.seed(1)

# lengths of series
length_ss <- 6000
length_ms_hom_short <- 60
length_ms_hom_long <- 240
length_ms_het <- 240

series_per_dataset <- 100
frequency <- 12
no_of_datasets <- 100

# create the data folder if not existing
dir.create(file.path(".", "data"), showWarnings = FALSE)

# setar related variables
lags <- 2
no_of_thresholds <- 1
threshold_val <- 2
coefficient_matrix <- c(2.9,-0.4,-0.1,-1.5, 0.2,0.3)
starting_values <- c(2.8, 2.2)

########### SS and MS-Hom-Short Scenarios
full_length_series <- NULL
for (dataset_index in 1:no_of_datasets){
  ts<-setar.sim(B=coefficient_matrix, lag=lags, type="simul", n = length_ss, nthresh=no_of_thresholds, Thresh=threshold_val, starting=starting_values)$serie

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
required_multiples_list <- list(100)
for (dataset_index in 1:no_of_datasets){
  output_path_ss <- paste0("./data/setar_dgp_ss_dataset_", dataset_index, ".txt")
  ts <- full_length_series[dataset_index,]
  for (multiple in required_multiples_list){
    length <- length_ms_hom_short * multiple
    series_chunk <- ts[1:length]
    write.table(x=t(series_chunk), file=output_path_ss, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}

# MS-Hom-Short scenario
for (dataset_index in 1:no_of_datasets){
  output_path_ms_hom_short <- paste0("./data/setar_dgp_ms_hom_short_dataset_", dataset_index, ".txt")
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
  series_set <- NULL
  set.seed(dataset_index)
  output_path_ms_hom_long <- paste0("./data/setar_dgp_ms_hom_long_dataset_", dataset_index, ".txt")
  for (series in 1:series_per_dataset){
    ts<-setar.sim(B=coefficient_matrix, lag=lags, type="simul", n = length_ms_hom_long, nthresh=no_of_thresholds, Thresh=threshold_val, starting=starting_values)$serie

    # normalize to 0 mean and unit variance
    ts <- scale(ts)
    min <- min(ts)
    if (min < 0){
      ts <- ts - min
    }
    if (min < 1){
      ts <- ts + 1
    }
    
    series_set <- rbind(series_set, t(ts))
  }

  # persist into the dataset
  write.table(x=series_set, file=output_path_ms_hom_long, row.names = FALSE, col.names = FALSE, sep=",")
}

# MS-Het scenario
for (dataset_index in 1:no_of_datasets){
  series_set <- NULL
  set.seed(dataset_index)
  for (series in 1:series_per_dataset){
    coefficient_matrix <- coefficient_matrix + rnorm(n=1, mean=0, sd=0.007)

    ts<-setar.sim(B=coefficient_matrix, lag=lags, type="simul", n = length_ms_het, nthresh=no_of_thresholds,
                  Thresh=threshold_val, starting = starting_values)$serie
    # normalize to 0 mean and unit variance
    ts <- scale(ts)
    min <- min(ts)
    if (min < 0){
      ts <- ts - min
    }
    if (min < 1){
      ts <- ts + 1
    }
    series_set <- rbind(series_set, t(ts))
  }

  # persist into the dataset
  output_path = paste0("./data/setar_dgp_ms_het_dataset_", dataset_index, ".txt")
  write.table(x=series_set, file=output_path, row.names = FALSE, col.names = FALSE, sep=",")
}
