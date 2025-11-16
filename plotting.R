library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(sf)
library(tigris)

# Define directory paths
working_directory <- "C:/Users/cnlub/OneDrive/Documents/nsf-program/"
output_directory <- paste0(working_directory, "output/")

# Read in results from the output directory
oaxaca_results <- readRDS(paste0(output_directory, "oaxaca_results.rds"))
rif_results <- readRDS(paste0(output_directory, "rif_results.rds"))
regression_data <- read_csv(paste0(working_directory, "data/regression_data.csv"),
                            show_col_types = FALSE)

# We will plot the cyclical density graph
regression_plot_data <- regression_data |> 
  mutate(metro = case_when(metro == 1 ~ "Metropolitan", metro == 0 ~ "Non-metropolitan"))

ggplot(regression_plot_data, aes(x = cycle, group = metro, fill = factor(metro))) +
  geom_density(alpha = 0.5) + 
  theme_bw() +
  labs(x = "Cyclical Unemployment Rates", y = "Density", fill = NULL) +  # Removes legend title
  scale_fill_manual(values = c("Non-metropolitan" = "#1f77b4", "Metropolitan" = "#ff7f0e")) +       # Custom colors
  coord_cartesian(xlim = c(-10, 10)) +                                   # Limits x-axis
  theme(legend.position = "bottom")
ggsave(paste0(working_directory, "output/cyclical.jpeg"), dpi = 300)

# Quick analysis for whether the distributions are different
ks.test(
  regression_data$cycle[regression_data$metro == 1],
  regression_data$cycle[regression_data$metro == 0]
)

# We will now plot the RIF-Oaxaca results

# Prepare data for plotting
plot_data <- rif_results |>
  filter(tau < 1.00) %>%
  mutate(
    facet = ifelse(component %in% c("mean_diff", "mean_y_A", "mean_y_B"), 
                   "Raw quantile gap (Metro – Non-Metro)", 
                   "Decomposition components"),
    component = case_when(facet == "Decomposition components" & component == "total" ~ "mean_diff",
                          TRUE ~ component),
    component = factor(component, 
                       levels = c("mean_diff", "mean_y_A", "mean_y_B", "explained", "unexplained"),
                       labels = c("Difference", "Metro", "Non-Metro", "Explained", "Unexplained"))
  )

# Faceted plot
ggplot(plot_data, aes(x = tau, y = estimate, color = component)) +
  #geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ facet, scales = "free_y", ncol = 1) +
  labs(
    title = "RIF–Oaxaca Decomposition Across Quantiles",
    x = "Quantile",
    y = "Estimate (with 95% CI)",
    color = NULL,
    fill = NULL
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(working_directory, "output/rif_lines.jpeg"), dpi = 300)


# We will simply present the results by a table
oaxaca_table_tex <- oaxaca_results$estimates %>%
  mutate(
    across(c(estimate, se, ci_lower, ci_upper), ~ round(.x, 4)),
    `95% CI` = paste0("[", ci_lower, ", ", ci_upper, "]"),
    component = factor(component, 
                       levels = c("mean_diff", "mean_y_A", "mean_y_B", "explained", "unexplained", "total"),
                       labels = c("Mean Difference", "Metro Mean", "Non-Metro Mean", "Explained", "Unexplained", "Total"))
  ) %>%
  select(Component = component,
         Estimate = estimate,
         `Std. Error` = se,
         `95% CI`) %>%
  kable(align = "lrrc", format = "latex") |> 
  kableExtra::kable_styling(full_width = FALSE, font_size = 14)
save_kable(oaxaca_table_tex, file = paste0(output_directory, "oaxaca_table.tex"))

# Quickly, I want to plot the classifications for our plot
classifications <- read_csv(paste0(working_directory,
                        "data-raw/metro_classifications/typology2004(all_final_codes).csv"),
                        show_col_type = FALSE) |> 
  select(FIPSTXT, State, County, rururb2003)

options(tigris_use_cache = TRUE)

# Get counties
counties_sf <- counties(cb = TRUE, year = 2021) %>%
  mutate(FIPS = paste0(STATEFP, COUNTYFP))

# Shift Alaska
alaska <- counties_sf %>% filter(STATEFP == "02") %>%
  st_transform(2163) %>%
  st_scale(0.35) %>%                      # shrink
  st_shift(c(2400000, -2100000))          # move near WA/OR

# Shift Hawaii
hawaii <- counties_sf %>% filter(STATEFP == "15") %>%
  st_transform(2163) %>%
  st_shift(c(5200000, -1400000))

# Lower 48
lower48 <- counties_sf %>%
  filter(!STATEFP %in% c("02", "15")) %>%
  st_transform(2163)

# Combine
us_counties_shifted <- bind_rows(lower48, alaska, hawaii)

# Join data
map_data <- us_counties_shifted %>%
  left_join(classifications, by = c("FIPS" = "FIPSTXT")) %>%
  drop_na()

# Plot with continuous gradient
ggplot(map_data) +
  geom_sf(aes(fill = rururb2003), color = NA) +
  scale_fill_gradient(
    low = "#2166ac", high = "#b2182b", trans = "reverse",
    na.value = "grey80"
  ) +
  labs(
    title = "US Counties by rururb2003 Classification",
    fill = "Classification"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

