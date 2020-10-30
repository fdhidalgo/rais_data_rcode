states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

base_dir <- "/media/spinner/rais/rais_original_files"


make_rais_2012 <- function(states, base_dir, size = 10000) {
  raw_data <-  fread(glue('{base_dir}/2012/{states}2012ID.txt'),
                     nrows = size, colClasses=c("character"))
  
  cleaned_data <- clean_rais_2012(raw_data)
}
