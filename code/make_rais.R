### make_rais ###
## this function reads in the data and runs a specific clean function 


make_rais <- function(states, base_dir, year, size = 10000) {

  raw_data <-  fread(glue('{base_dir}/{year}/{states}'),
                     nrows = size, colClasses=c("character")) # read in data
                     
  correct_cleanfunction <- glue("clean_rais_{year}") # locate the correct clean function based off year
  
  cleaned_data <- eval(call(correct_cleanfunction, raw_data)) # call the clean function on the raw data
  cleaned_data
}
