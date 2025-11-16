library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# Read in already-processed series ID information
series <- read_csv(paste0(working_directory, "data/series_ids.csv"), show_col_types = FALSE)

# Read the file
county_series <- read.table(
  paste0(working_directory, "data-raw/la.data.64.County.txt"),
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE)

# Clean it a bit and join with our series ID information
county_series_clean <- county_series |> 
  mutate(period = substr(period, 2, 3),
         series_id = str_trim(series_id, side = "right")) |> 
  filter(period != 13) |> 
  left_join(series, by = "series_id") |> 
  select(series_id, year, month = period, value, area_code, seasonal,
         measure_name, area_name, county, state) |> 
  mutate()

write_csv(county_series_clean, paste0(working_directory, "data/county_estimates.csv"))
