### run_rais ###
## assuming all functions have been built and packages run, this file will run the data for you
source("./code/package_list.R")
source("./code/gather_rais.R")
source("./code/make_rais.R")
source("./code/masterlist_rais.R")
source("./code/clean_rais_2013.R")

base_dir <- "/media/spinner/rais/rais_original_files"
#base_dir <- "/mnt/danny_data/rais/rais_original_files"
year <- 2013 # or any year

rais_data <- map_dfr(gather_rais(base_dir, year = year),
                     ~make_rais(.x, base_dir, year = year, size = 1000))
