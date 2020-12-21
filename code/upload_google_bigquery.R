library(vroom)
library(bigrquery)
library(arrow)

#Authenticate
bq_auth()
project <- "rais-mit"
dataset <- "rais_clean"
setwd("/media/spinner/rais/rais_cleaned/2013/")

#Example data frame to create schema
df <- vroom("/media/spinner/rais/rais_cleaned/2013/BA2013ID.gz", guess_max = 5000)
spec <- spec(df)

#Create table
rais_2013_table <- bq_table(project = project, dataset = dataset, table = 'rais_clean_2013')
bq_table_create(x = rais_2013_table, fields = as_bq_fields(df))

#Save as parquet files because it perservs schema
for(i in 1:length(dir())){
  print(i)
  df_tmp <- vroom(dir()[i], col_types = spec)
  write_parquet(df_tmp, "../parquet_tmp.parquet", compression = "gzip")
  system(paste0("/home/dhidalgo/google-cloud-sdk/bin/bq load --source_format=PARQUET \\rais_clean.rais_clean_2013 \\./", "../parquet_tmp.parquet"))
  file.remove("../parquet_tmp.parquet")
}

