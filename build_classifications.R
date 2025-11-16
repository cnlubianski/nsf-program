library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(tidycensus)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"
class_directory <- paste0(working_directory, "data-raw/metro_classifications/")

# We need to build a panel of year by county of their metro classifications by year
typology_1989 <- read_csv(paste0(class_directory, "typology89(Data).csv"), show_col_types = FALSE)
typology_2004 <- read_csv(paste0(class_directory, "typology2004(all_final_codes).csv"), show_col_types = FALSE)
typology_2015 <- read_csv(paste0(class_directory, "erscountytypology2015edition.csv"), show_col_types = FALSE)
typology_2025 <- read_csv(paste0(class_directory, "erscountytypology2025edition.csv"), show_col_types = FALSE)

# For 1990 through 1992, we will use the `FM` column where 8 means metro, otherwise is nonmetro
counties_90_92 <- typology_1989 |> 
  select(FIPS, state = State, county = `County name`, FM) |> 
  mutate(metro = case_when(FM == 8 ~ 1, TRUE ~ 0), FIPS = as.numeric(FIPS), 
         county = str_to_title(county), .keep = "unused")

# For 1993 through 2003, we will use the Rural/Urban column designed for 1993 onwards
# 0-3 is metro and 4-9 is nonmetro (turns out it is the same as 90_92)
counties_93_03 <- typology_1989 |> 
  select(FIPS, state = State, county = `County name`, RuralUrb93) |> 
  mutate(metro = case_when(RuralUrb93 %in% 0:3 ~ 1, RuralUrb93 %in% 4:9 ~ 0),
         FIPS = as.numeric(FIPS), county = str_to_title(county), .keep = "unused")

# For 2004 through 2014, we will simply use the ERS' metro classification
counties_04_14 <- typology_2004 |> 
  select(FIPS, state = State, county = County, metro)

# For 2004 through 2013, we will simply use the ERS' metro classification
counties_15_24 <- typology_2015 |> 
  select(FIPS = FIPStxt, state = State, county = County_name,
         metro = `Metro-nonmetro status, 2013 0=Nonmetro 1=Metro`)

# Helper: repeat a classification df over a set of years
replicate_years <- function(df, years) {
  map_dfr(years, ~ mutate(df, year = .x))
}

# Expand each dataset to its year range
counties_90_92_exp <- replicate_years(counties_90_92, 1990:1992)
counties_93_03_exp <- replicate_years(counties_93_03, 1993:2003)
counties_04_14_exp <- replicate_years(counties_04_14, 2004:2014)
counties_15_24_exp <- replicate_years(counties_15_24, 2015:2024)

# Combine into one big panel
counties_panel <- bind_rows(
  counties_90_92_exp,
  counties_93_03_exp,
  counties_04_14_exp,
  counties_15_24_exp) |> 
  arrange(FIPS, state, county, year)

# We don't have the names quite perfect yet for examples like Mclean when we expect McLean
fips_codes_tidy <- tidycensus::fips_codes |> 
  mutate(FIPS = as.numeric(paste0(state_code, county_code))) |> 
  select(new_county = county, FIPS)

counties_panel <- counties_panel |> 
  mutate(FIPS = as.numeric(FIPS)) |> 
  left_join(fips_codes_tidy, by = "FIPS") |> 
  select(-county) |> 
  rename(county = new_county) |> 
  filter(str_detect(county, "County"))
  
# Save the csv file
write_csv(counties_panel, paste0(working_directory, "data/classifications.csv"))
