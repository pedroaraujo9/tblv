library(testthat)
library(btblv)
library(tidyverse)

source("../rscripts/utils.R")
test_file("testthat/test-save_fit_btblv.R")
test_file("testthat/test-download_models_gdrive.R")
