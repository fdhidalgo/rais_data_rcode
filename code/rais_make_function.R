# need clean_rais function for this to work
# vector for states. This is needed to read in the different files
library(pacman)
p_load(dtplyr, data.table, ggplot2, janitor, stringr, lubridate, glue, tidyverse, parallel)

source("./code/rais_clean_function.R")

states <- c("centro05", "nordeste05", "norte05", "sp05", "sp105", "sudeste05", "sul05" )

base_dir <- "/Volumes/HidalgoVault/data/RAIS/rais_original_files"

# make rais, but not parallel. It works, but trying with everything was not working....
# ALSO, change folder name in glue and set working directory to correct directory
make_rais <- function(states, base_dir, size = 10000) {
  states_list <- lapply( states , function(state) fread(glue('{base_dir}/2005/{state}.TXT'),
                                                        nrows = size, colClasses=c("character")))

  clean_states_list<- lapply(states_list, clean_rais)

  everything <- bind_rows(clean_states_list)

}

rais_2005 <- make_rais(states = states[1:2], base_dir = base_dir, size = Inf)


