library(dplyr)
library(tidyr)
library(readr)
library(mFilter)
library(zoo)
library(purrr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# Read in the cleaned county employment data
county_unemployment <- read_csv(paste0(working_directory, "data/county_unemployment.csv"),
                                show_col_types = FALSE)

# County/Area Data
area_level_info <- county_unemployment |>
  select(LAUS_Code, State_FIPS, Area_Code, Area_Name, County, State) |> 
  distinct()

# Compute the trend and cycle components using hpfilter()
panel_hp <- county_unemployment |>
  # 1. Arrange & group
  arrange(County, Year, Month) |>
  group_by(County) |>
  # Capture each county’s data in a list-column
  summarise(data = list(cur_data_all()), .groups = "drop") |>
  # 2. Build ts object for each county
  mutate(ts_data = map(
    data, ~ ts(.x$Unemployment_Rate, start = c(min(.x$Year), min(.x$Month)),
      frequency = 12))) |>
  # 3. Apply hpfilter to each ts series
  mutate(hp_out = map(
    ts_data, ~ hpfilter(.x, freq = 14400, # λ = 14,400 for monthly smoothing
                        type  = "lambda", drift = FALSE))) |>
  # 4. Extract numeric vectors for cycle, trend, and build a Date vector
  mutate(cycle = map(hp_out, ~ as.numeric(.x$cycle)),
         trend = map(hp_out, ~ as.numeric(.x$trend)),
         date  = map(ts_data, ~ as.Date(as.yearmon(time(.x))))) |>
  # 5. Keep only the pieces we need
  select(County, date, cycle, trend) |>
  # 6. Unnest so each row = County × Date
  unnest(cols = c(date, cycle, trend))

# Left join the area information and save it for later
panel_hp |>
  left_join(area_level_info, by = "County") |> 
  write_csv(paste0(working_directory, "data/", "hpfilter_results.csv"))
