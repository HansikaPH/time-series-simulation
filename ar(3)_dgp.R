# for simple linear dgp data
library(CombMSC)
library(ggplot2)
library(reshape2)
library(forecast)

set.seed(1)

source("./statistical_methods/time_series_generation/ar_coefficients_generator.R")

lags = 3 # for AR(3) process
maxRoot = 5# for a non exploding process
# individual_length = 240

full_length_of_series = 1800
series_per_dataset = 100
frequency = 12
no_of_datasets = 1000
individual_length = 18

# # ############## same coefficients scenarios
output_path = "datasets/text_data/Generated_Data/coefficient_based/simple_dgp2_dataset"
unlink(paste(output_path, "*", sep = ""))
parameters <- generateRandomARMAParameters(lags, maxRoot)
full_length_series = NULL
for (dataset_index in 1:no_of_datasets){
  ts <- arima.sim(model=list(ar=parameters), n=full_length_of_series, n.start = 100)

  # normalize to 0 mean and unit variance
  ts = scale(ts)
  min <- min(ts)
  if (min < 0){
    ts <- ts - min
  }
  if (min < 1){
    ts = ts + 1
  }
  full_length_series = rbind(full_length_series, t(ts))
}

required_multiples_list = list(1,2,3,4,5,10,20,30,40,50,75,100)
### single series with varying length
# write varying length individual series into files
for (dataset_index in 1:no_of_datasets){
  output_path_extended = paste(output_path, "_", dataset_index, "_concat_series_different_lengths.txt", sep="")
  ts = full_length_series[dataset_index,]
  for (multiple in required_multiples_list){
    length = individual_length * multiple
    series_chunk = ts[1:length]
    write.table(x=t(series_chunk), file=output_path_extended, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}
### multiple series with varying number of series
# write all different number of series into files
for (dataset_index in 1:no_of_datasets){
  output_path_extended = paste(output_path, "_", dataset_index, ".txt", sep="")
  ts = full_length_series[dataset_index,]
  last_index = 0
  while (last_index < full_length_of_series){
    start = last_index + 1
    last_index = start + individual_length - 1
    series_chunk = ts[start:last_index]
    write.table(x=t(series_chunk), file=output_path_extended, row.names = FALSE, col.names = FALSE, sep=",", append = TRUE)
  }
}


# #### multiple series with varying lengths of series
# for (dataset_index in 1:no_of_datasets){
#   full_length_series = NULL
#   for (series in 1:series_per_dataset) {
#     ts <- arima.sim(model=list(ar=parameters), n=full_length_of_series, n.start = 100)
# 
#     # normalize to 0 mean and unit variance
#     ts = scale(ts)
#     min <- min(ts)
#     if (min < 0){
#       ts <- ts - min
#     }
#     if (min < 1){
#       ts = ts + 1
#     }
#     full_length_series = rbind(full_length_series, t(ts))
#   }
# 
#   # write the series of a particular dataset into the file
#   output_path_extended = paste(output_path, "_", dataset_index, ".txt", sep="")
#   write.table(x=full_length_series, file=output_path_extended, row.names = FALSE, col.names = FALSE, sep=",")
# }
# 
# ############## different coefficients scenarios
# # #### multiple series with varying lengths of series - many coefficients
# # output_path = "datasets/text_data/Generated_Data/coefficient_based/multiple_ar3_dgp_varying_lengths_dataset"
# # unlink(paste(output_path, "*", sep = ""))
# # 
# # for (dataset_index in 1:no_of_datasets){
# #   full_length_series = NULL
# #   for (series in 1:series_per_dataset) {
# #     
# #     # generate ar coefficients
# #     parameters <- generateRandomARMAParameters(lags, maxRoot)
# #     
# #     ts <- arima.sim(model=list(ar=parameters), n=full_length_of_series, n.start = 100)
# # 
# #     # normalize to 0 mean and unit variance
# #     ts = scale(ts)
# #     min <- min(ts)
# #     if (min < 0){
# #       ts <- ts - min
# #     }
# #     if (min < 1){
# #       ts = ts + 1
# #     }
# #     full_length_series = rbind(full_length_series, t(ts))
# #   }
# # 
# #   # write the series of a particular dataset into the file
# #   output_path_extended = paste(output_path, "_", dataset_index, ".txt", sep="")
# #   write.table(x=full_length_series, file=output_path_extended, row.names = FALSE, col.names = FALSE, sep=",")
# # }
# 
# #### multiple series with varying lengths of series - few coefficients
# # output_path = "datasets/text_data/Generated_Data/coefficient_based/few_ar3_dgp_varying_lengths_dataset"
# # unlink(paste(output_path, "*", sep = ""))
# # 
# # no_of_dgps = 4
# # parameters_list = list()
# # for(dgp in 1:no_of_dgps){
# #   parameters <- generateRandomARMAParameters(lags, maxRoot)
# #   parameters_list[[dgp]] <- parameters
# # }
# # 
# # 
# # for (dataset_index in 1:no_of_datasets){
# #   full_length_series = NULL
# #   for (series in 1:series_per_dataset) {
# #     parameter_index = ceiling(series/(series_per_dataset/no_of_dgps))
# #     print(parameter_index)
# #     parameters = parameters_list[[parameter_index]]
# #     
# #     ts <- arima.sim(model=list(ar=parameters), n=full_length_of_series, n.start = 100)
# #     
# #     # normalize to 0 mean and unit variance
# #     ts = scale(ts)
# #     min <- min(ts)
# #     if (min < 0){
# #       ts <- ts - min
# #     }
# #     if (min < 1){
# #       ts = ts + 1
# #     }
# #     full_length_series = rbind(full_length_series, t(ts))
# #   }
# #   
# #   # write the series of a particular dataset into the file
# #   output_path_extended = paste(output_path, "_", dataset_index, ".txt", sep="")
# #   write.table(x=full_length_series, file=output_path_extended, row.names = FALSE, col.names = FALSE, sep=",")
# # }
