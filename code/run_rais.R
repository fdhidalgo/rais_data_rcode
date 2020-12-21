### run_rais ###
## assuming all functions have been built and packages run, this file will run the data for you
source("./code/package_list.R")
source("./code/masterlist_rais.R")
source("./code/clean_rais_2013.R")
setDTthreads(1)

clean_save_rais <- function(file, clean_func, out_file){
  clean_data <- fread(file,
                      colClasses=c("character")) %>%
    clean_func()
  fwrite(clean_data, file = out_file)
}

base_dir <- "/media/spinner/rais"
year <- 2013
clean_func <- clean_rais_2013

files <- dir(glue(base_dir, "/rais_original_files/", "{year}"), full.names = TRUE)
out_files <-  paste0(glue(base_dir, "/rais_cleaned/", "{year}", "/"),
                     gsub(".*/(.*)\\.txt", files, replacement = "\\1"),
                     ".gz")

for(i in 1:27){
  print(i)
  clean_save_rais(file = files[i], clean_func = clean_func, out_file = out_files[i])
}

