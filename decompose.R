library(dplyr)
library(tidyr)
library(readr)
library(mFilter)
library(lubridate)
library(zoo)
library(purrr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# Read in the cleaned county employment data
county_data <- read_csv(paste0(working_directory, "data/county_estimates.csv"),
                        show_col_types = FALSE)

# County/Area Data
area_level_info <- county_data |>
  select(area_name, county, state) |> 
  distinct()

# We should keep the other employment series data for later
county_non_unemployment <- county_data |> 
  filter(measure_name != "Unemployment Rate") |> 
  mutate(value = as.numeric(value),
         year = as.numeric(year),
         month = as.numeric(month)) |>
  select(-series_id, -seasonal) |>
  mutate(date = ymd(paste(year, sprintf("%02d", as.integer(month)), "01", sep = "-")),
         .keep = "unused") |>
  pivot_wider(names_from = measure_name, values_from = value)

# Get only unemployment rate data
county_unemployment <- county_data |> 
  filter(measure_name == "Unemployment Rate") |> 
  mutate(value = as.numeric(value),
         year = as.numeric(year),
         month = as.numeric(month))

# Define area names to iterate through and create a storage dataframe
area_names <- unique(county_unemployment$area_name)
area_storage <- data.frame()

# We will iterate through areas, filter to its data, then pass a time series object into HP filter
for (area in area_names) {
  print(area)
  area_series <- county_unemployment |> filter(area_name == area)
  
  ts_data <- ts(area_series$value, start = c(min(area_series$year), min(area_series$month)),
                frequency = 12)
  
  hpfilter_out <- hpfilter(ts_data, freq = 14400, # λ = 14,400 for monthly smoothing
                           type  = "lambda", drift = FALSE)
  
  area_storage <- bind_rows(area_storage, data.frame(
    date = as.Date(as.yearmon(time(ts_data))), cycle = as.numeric(hpfilter_out$cycle),
    trend = as.numeric(hpfilter_out$trend), area_name = area
  ))
}

area_storage |> 
  left_join(county_non_unemployment |> select(area_name, date, Unemployment, Employment, `Labor Force`),
            by = c("area_name", "date")) |> 
  left_join(area_level_info, by = "area_name") |> 
  write_csv(paste0(working_directory, "data/counties_decomposed.csv"))
