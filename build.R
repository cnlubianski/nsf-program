library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# We have to pre-format the input txt file because of the weird setup
widths <- c(16, 10, 10, 60, 8, 6, 17, 17, 17, 8)

# Define column names
col_names <- c("LAUS_Code", "State_FIPS", "Area_Code", "Area_Name",
               "Year", "Month", "Labor_Force", "Employment", "Unemployment", "Unemployment_Rate")

# Read the file then cut some rows on top and bottom to be in a dataframe format
df_raw <- read.fwf(paste0(working_directory, "data-raw/ssamatab1.txt"),
                   widths = widths,
                   skip = 5, # Skip header and column name lines
                   col.names = col_names,
                   strip.white = TRUE)

df_clean <- df_raw |> 
  as_tibble() |> 
  slice(1:(n() - 5)) |> 
  filter(!if_any(everything(), ~ str_detect(.x, "\\(n\\)"))) |> 
  mutate(across(c(Year, Month), as.integer),
         across(c(Labor_Force, Employment, Unemployment, Unemployment_Rate),
                ~ as.numeric(gsub(",", "", .))),
         Area_Name = str_remove(Area_Name, " MSA")) |> 
  separate(Area_Name, into = c("County", "State"), sep = ", ", remove = FALSE)
readr::write_csv(df_clean, "data/county_unemployment.csv")

# Several counties have missing data:
# - Slidell-Mandeville-Covington, LA MSA (9/2005 through 6/2006)
# - Seattle-Tacoma-Bellevue, WA MSA (1/1990 through 11/1993)
# - San Juan-Carolina-Caguas, PR MSA (3/2020 through 4/2020)
# - Ponce, PR MSA (3/2020 through 4/2020)
# - New Orleans-Metairie, LA MSA (9/2005 through 6/2006)
# - Mayaguez, PR MSA (3/2020 through 4/2020)
# - Guayama, PR MSA (3/2020 through 4/2020)
# - Cleveland, OH MSA (1/1990 through 12/1993)
# - Arecibo, PR MSA (3/2020 through 4/2020)
# - Aguadilla, PR MSA (3/2020 through 4/2020)
