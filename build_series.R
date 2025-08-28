library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# Define column widths
widths <- c(30, 1, 20, 4, 1, 1, 131, 4, 3, 4, 3)

# Read the file
la_series <- read.fwf(
  file = paste0(working_directory, "data-raw/la.series.txt"),
  widths = widths,
  skip = 1,
  header = FALSE,
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  buffersize = 10000
) |> 
  select(where(~!all(is.na(.))))

# Assign column names
names(la_series) <- c(
  "series_id", "area_type_code", "area_code", "measure_code", "seasonal",
  "srd_code1", "srd_code2", "series_title", "footnote_codes", "begin_year",
  "begin_period", "end_year", "end_period"
)

# Drop spacer columns
la_series_clean <- la_series |> 
  select(-footnote_codes) |> 
  filter(area_type_code == "F") |> 
  mutate(srd_code = paste0(srd_code1, srd_code2),
         measure_name = case_when(
           measure_code == "3" ~ "Unemployment Rate",
           measure_code == "4" ~ "Unemployment",
           measure_code == "5" ~ "Employment",
           measure_code == "6" ~ "Labor Force",),
         .keep = "unused") |> 
  mutate(area_name = series_title |>
           str_replace("^.*?:\\s*", "") |>
           str_remove_all("\\s*\\(U\\)"),
         county = str_trim(str_extract(area_name, "^[^,]+")),
         state  = str_trim(str_extract(area_name, "[^,]+$"))
  )
write_csv(la_series_clean, paste0(working_directory, "data/series.csv"))
