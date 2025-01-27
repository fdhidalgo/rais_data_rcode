library(data.table)
library(arrow)
library(fs)
library(dplyr)

csvs <- dir_ls("/media/spinner/rais/rais_cleaned",
        recurse = TRUE,
        type = "file"
)

dir_create(paste0("/media/spinner/rais/rais_parquet/", 2001:2018))

for (i in seq_len(length(csvs))) {
        print(i)
        csv <- csvs[i]
        file_out <- path_ext_set(path_ext_remove(csvs[i]), ".parquet") %>%
                stringr::str_replace("rais_cleaned", "rais_parquet")
        data <- fread(csv)
        write_parquet(data, file_out, compression = "gzip")
}