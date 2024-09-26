# _targets.R
library(targets)
source("R/data_functions.R")
source("R/model_functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("readr", "dplyr", "tidyr", "ggplot2", "DT", "leaflet", "sf", "janitor"))
list(
  tar_target(benthic_data_file, "data_raw/DWR Benthic raw count data 1975-2020 2021_10_01.csv",format = "file"),
  tar_target(benthic_data, read_and_clean(benthic_data_file)),
  tar_target(zoop_data_file, "data/annual_averages/zoop.csv",format = "file"),
  tar_target(zoop_data, read_and_clean(zoop_data_file)),
  tar_target(integrate, integrate_data(c(benthic=benthic_data, zoop=zoop_data))),
  tar_target(model1, run_model1(integrate)),
  tar_target(model2, run_model2(integrate))
)
