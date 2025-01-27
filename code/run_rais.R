### run_rais ###
## assuming all functions have been built and packages run, this file will run the data for you
source("./code/package_list.R")
source("./code/masterlist_rais.R")
walk(dir("./code",
  pattern = "clean_rais",
  full.names = TRUE
), source)

# data.table::setDTthreads(1)

clean_save_rais <- function(file, clean_func, out_file) {
  gc()
  clean_data <- fread(file,
    colClasses = c("character"),
    encoding = "Latin-1"
  ) |>
    clean_func()
  write_parquet(clean_data,
    sink = out_file,
    compression = "gzip"
  )
}

base_dir <- "/Volumes/brizola/rais"

clean_year <- function(year) {
  clean_func <- eval(parse(text = glue("clean_rais_{year}")))
  files <- dir(glue(base_dir, "/rais_original_files/", "{year}"), full.names = TRUE)
  files <- files[grep("ESTB", files, invert = TRUE, ignore.case = TRUE)]
  out_files <- paste0(
    glue(base_dir, "/rais_cleaned/", "{year}", "/"),
    gsub(".*/(.*)\\.txt\\.gz", files, replacement = "\\1", ignore.case = TRUE),
    ".parquet"
  )

  for (i in seq_along(files)) {
    print(files[i])
    if (file.exists(glue(base_dir, "/rais_cleaned/{year}")) == FALSE) {
      dir.create(glue(base_dir, "/rais_cleaned/{year}"))
    }
    clean_save_rais(file = files[i], clean_func = clean_func, out_file = out_files[i])
    gc()
  }
}

walk(2006, clean_year)
