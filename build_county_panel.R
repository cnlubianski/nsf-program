library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(readxl)
library(tidycensus)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# General parameters
years <- 1990:2024
quarters <- c(1, 2, 3, 4)

# Iterate through each year, save csv, then save a large csv for all years in data/
all_year_storage <- data.frame()
yearly_directory <- paste0(working_directory, "data-raw/county-level/")
for (year in years) {
  print(year)
  
  year_directory <- paste0(yearly_directory, year, "_all_county_high_level/")
  year_suffix <- substr(year, 3, 4)
  
  year_storage <- data.frame()
  for (quarter in quarters) {
    quarter_panel <- read_xlsx(paste0(year_directory, "allhlcn", year_suffix, quarter, ".xlsx"))
    
    quarter_county_panel <- quarter_panel |> 
      filter(`Area Type` == "County") |> 
      select(-`Area Type`, -`Status Code`) |> 
      mutate(County_name = str_extract(Area, "^[^,]+")) |> 
      # We need to change the names of columns based on which quarter it is
      rename_with(~str_replace(., "January|April|July|October", "Month_1"),
                  .cols = contains(c("January", "April", "July", "October"))) |> 
      rename_with(~str_replace(., "February|May|August|November", "Month_2"),
                  .cols = contains(c("February", "May", "August", "November"))) |>
      rename_with(~str_replace(., "March|June|September|December", "Month_3"),
                  .cols = contains(c("March", "June", "September", "December")))
    
    year_storage <- bind_rows(year_storage, quarter_county_panel)
    write_csv(year_storage, paste0(year_directory, "all_quarters_county.csv"))
  }
  
  # Save year data in the year directory
  all_year_storage <- bind_rows(all_year_storage, year_storage)
}

# We'll add onto the state names with their abbreviations then save the csv file
all_year_storage |> 
  select(year = Year, quarter = Qtr, state_name = `St Name`, area = Area, county = County_name,
         ownership = Ownership, industry = Industry, establishment_count = `Establishment Count`,
         month_1_emp = `Month_1 Employment`, month_2_emp = `Month_2 Employment`,
         month_3_emp = `Month_3 Employment`, tot_quarterly_wages = `Total Quarterly Wages`,
         avg_weekly_wage = `Average Weekly Wage`,
         employment_loc_quotient = `Employment Location Quotient Relative to U.S.`,
         tot_wage_loc_quotient = `Total Wage Location Quotient Relative to U.S.`) |>
  left_join(tidycensus::fips_codes |> distinct(state_name, state),
            by = "state_name") |> 
  write_csv(paste0(working_directory, "data/county_panel.csv"))



