library(dplyr)
library(tidyr)
library(readr)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"

# Read in the decomposed cyclical and trend dataset
decomposed_df <- read_csv(paste0(working_directory, "data/hpfilter_results.csv"),
                          show_col_types = FALSE)
