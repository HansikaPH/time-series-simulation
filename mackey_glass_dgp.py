from nolitsa import data
import random
import numpy as np
import os

random.seed(0)
np.random.seed(0)

# lengths of series
length_ss = 6000
length_ms_hom_short = 60
length_ms_hom_long = 240
length_ms_het = 240

series_per_dataset = 100
frequency = 12
no_of_datasets = 100

# create the data folder if not existing
if not os.path.exists('data'):
    os.makedirs('data')

########### SS and MS-Hom-Short Scenarios
all_series = list()
for dataset in range(no_of_datasets):
    ts = data.mackey_glass(length=length_ss, sample=5)

    min = np.min(ts)
    if min < 0:
        ts = ts - min
    if min < 1:
        ts = ts + 1
    all_series.append(ts)

# SS scenario
required_multiples_list = list([100])
for dataset_index in range(no_of_datasets):
    output_path_ss = "./data/mackey_glass_dgp_ss_dataset_" + str(dataset_index + 1) + ".txt"
    with open(output_path_ss, "a") as data_file_object:
        ts = all_series[dataset_index]
        for multiple in required_multiples_list:
            length = length_ms_hom_short * multiple
            series_chunk = ts[0:length]
            data_file_object.write(",".join(str(elem) for elem in series_chunk) + "\n")

# MS-Hom-Short scenario
for dataset_index in range(no_of_datasets):
    output_path_ms_hom_short = "./data/mackey_glass_dgp_ms_hom_short_dataset_" + str(dataset_index + 1) + ".txt"
    ts = all_series[dataset_index]
    last_index = 0
    with open(output_path_ms_hom_short, "a") as data_file_object:
        while last_index < length_ss:
            start = last_index
            last_index = start + length_ms_hom_short
            series_chunk = ts[start:last_index]
            data_file_object.write(",".join(str(elem) for elem in series_chunk) + "\n")

# MS-Hom-Long scenario
for dataset_index in range(no_of_datasets):
    output_path_ms_hom_long = "./data/mackey_glass_dgp_ms_hom_long_dataset_" + str(dataset_index + 1) + ".txt"
    with open(output_path_ms_hom_long, "a") as data_file_object:
        for series in range(series_per_dataset):
            ts = data.mackey_glass(length=length_ms_hom_long, sample=5)
            # normalize to 0 mean and unit variance
            min = np.min(ts)
            if min < 0:
                ts = ts - min
            if min < 1:
                ts = ts + 1
            # full_length_series.append(ts)
            data_file_object.write(",".join(str(elem) for elem in ts) + "\n")

# MS-Het scenario
for dataset_index in range(no_of_datasets):
    output_path_ms_het = "./data/mackey_glass_dgp_ms_het_dataset_" + str(dataset_index + 1) + ".txt"
    with open(output_path_ms_het, "a") as data_file_object:
        for series in range(series_per_dataset):
            tau = np.random.uniform(17, 100)
            ts = data.mackey_glass(length=length_ms_het, sample=5, tau=tau)
            min = np.min(ts)
            if min < 0:
                ts = ts - min
            if min < 1:
                ts = ts + 1
            data_file_object.write(",".join(str(elem) for elem in ts) + "\n")
