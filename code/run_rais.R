### run_rais ###
## assuming all functions have been built and packages run, this file will run the data for you 

base_dir <- "/media/spinner/rais/rais_original_files"
year <- 2012 # or any year

rais_data <- map_dfr(gather_rais(base_dir, year = 2012), ~make_rais(.x, base_dir, year = 2012))
