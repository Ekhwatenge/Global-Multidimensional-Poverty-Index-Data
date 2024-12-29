# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)
library(sf)
library(cluster)

# Load the dataset
data <- read.csv('hdx_hapi_poverty_rate_global.csv', stringsAsFactors = FALSE)

# Clean the dataset
data_cleaned <- data[-1, ]
colnames(data_cleaned) <- c('location_code', 'has_hrp', 'in_gho', 'provider_admin1_name', 'admin1_code', 'admin1_name', 'mpi', 'headcount_ratio', 'intensity_of_deprivation', 'vulnerable_to_poverty', 'in_severe_poverty', 'reference_period_start', 'reference_period_end')

# Convert numeric columns to appropriate data types
numeric_columns <- c('mpi', 'headcount_ratio', 'intensity_of_deprivation', 'vulnerable_to_poverty', 'in_severe_poverty')
data_cleaned[numeric_columns] <- lapply(data_cleaned[numeric_columns], as.numeric)

# Convert date columns to Date format
data_cleaned$reference_period_start <- as.Date(data_cleaned$reference_period_start)
data_cleaned$reference_period_end <- as.Date(data_cleaned$reference_period_end)

# Generate summary statistics
summary_statistics <- summary(data_cleaned[numeric_columns])

# Group data by location_code and calculate mean values
country_summary <- data_cleaned %>% 
  group_by(location_code) %>% 
  summarise(across(all_of(numeric_columns), mean, na.rm = TRUE))

# Top 10 countries with highest MPI
top_10_mpi <- country_summary %>%
  arrange(desc(mpi)) %>%
  head(10)

# Visualization: Top 10 countries with highest MPI
ggplot(top_10_mpi, aes(x = reorder(location_code, mpi), y = mpi)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries with Highest MPI",
       x = "Country Code",
       y = "Multidimensional Poverty Index (MPI)") +
  theme_minimal()

# Correlation analysis
correlation_matrix <- cor(data_cleaned[numeric_columns], use = "complete.obs")

# Visualization: Correlation heatmap
ggplot(data = reshape2::melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Poverty Indicators")

# Time series analysis
time_series_data <- data_cleaned %>%
  group_by(reference_period_start) %>%
  summarise(across(all_of(numeric_columns), mean, na.rm = TRUE))

# Visualization: Time series of MPI
ggplot(time_series_data, aes(x = reference_period_start, y = mpi)) +
  geom_line() +
  geom_point() +
  labs(title = "Global MPI Trend Over Time",
       x = "Year",
       y = "Average Multidimensional Poverty Index (MPI)") +
  theme_minimal()

# Regional analysis
regional_summary <- data_cleaned %>% 
  group_by(admin1_name) %>% 
  summarise(across(all_of(numeric_columns), mean, na.rm = TRUE)) %>%
  arrange(desc(mpi))

# Top 20 regions with highest MPI
top_20_regions <- head(regional_summary, 20)

# Visualization: Top 20 regions with highest MPI
ggplot(top_20_regions, aes(x = reorder(admin1_name, mpi), y = mpi)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 Regions with Highest MPI",
       x = "Region",
       y = "Multidimensional Poverty Index (MPI)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Generate report
report <- capture.output({
  cat("Poverty Rate Analysis Report\n\n")
  
  cat("1. Summary Statistics\n")
  print(kable(summary_statistics))
  
  cat("\n2. Top 10 Countries with Highest MPI\n")
  print(kable(top_10_mpi))
  
  cat("\n3. Correlation Analysis\n")
  print(kable(correlation_matrix))
  
  cat("\n4. Top 20 Regions with Highest MPI\n")
  print(kable(top_20_regions))
})

## Regional Disparities Analysis

# Calculate summary statistics by region
regional_summary <- data_cleaned %>%
  group_by(admin1_name) %>%
  summarise(
    mean_mpi = mean(mpi, na.rm = TRUE),
    sd_mpi = sd(mpi, na.rm = TRUE),
    min_mpi = min(mpi, na.rm = TRUE),
    max_mpi = max(mpi, na.rm = TRUE)
  )

# Perform one-way ANOVA
anova_result <- aov(mpi ~ admin1_name, data = data_cleaned)
summary(anova_result)

# Create a choropleth map (requires shapefile data)
# shapefile <- st_read("path_to_shapefile.shp")
# map_data <- left_join(shapefile, regional_summary, by = c("admin1_name" = "admin1_name"))
# ggplot(map_data) +
#   geom_sf(aes(fill = mean_mpi)) +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   labs(title = "Regional MPI Disparities", fill = "Mean MPI")

## Temporal Trend Analysis

# Calculate average annual rate of change
temporal_trends <- data_cleaned %>%
  group_by(location_code, admin1_name) %>%
  arrange(reference_period_start) %>%
  summarise(
    start_mpi = first(mpi),
    end_mpi = last(mpi),
    years = as.numeric(difftime(last(reference_period_end), first(reference_period_start), units = "days")) / 365,
    avg_annual_change = (end_mpi - start_mpi) / years
  )

# Visualize trends
ggplot(data_cleaned, aes(x = reference_period_start, y = mpi, color = admin1_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "MPI Trends Over Time", x = "Year", y = "MPI", color = "Region")

## Decomposition Analysis

# Assuming the dataset includes individual indicators that make up the MPI
indicators <- c("indicator1", "indicator2", "indicator3") # Replace with actual indicator names

data_cleaned %>%
  gather(key = "indicator", value = "value", indicators) %>%
  group_by(admin1_name, indicator) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = admin1_name, y = mean_value, fill = indicator)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "MPI Composition by Region", x = "Region", y = "Contribution to MPI")

## Correlation with Development Indicators

# Assuming additional development indicators are available in the dataset
development_indicators <- c("gdp_per_capita", "education_index", "health_index") # Replace with actual indicator names

correlation_matrix <- cor(data_cleaned[c("mpi", development_indicators)], use = "complete.obs")
corrplot::corrplot(correlation_matrix, method = "circle")

## Cluster Analysis

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(data_cleaned[c("mpi", "headcount_ratio", "intensity_of_deprivation")], centers = 5)

# Add cluster assignments to the dataset
data_cleaned$cluster <- kmeans_result$cluster

# Visualize clusters
ggplot(data_cleaned, aes(x = headcount_ratio, y = intensity_of_deprivation, color = factor(cluster))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Cluster Analysis of Poverty Profiles", color = "Cluster")

## Inequality Analysis

# Calculate Gini coefficient for MPI within each region
gini_coefficients <- data_cleaned %>%
  group_by(admin1_name) %>%
  summarise(gini_mpi = ineq::Gini(mpi))

# Create Lorenz curve
ggplot(data_cleaned, aes(x = cumsum(mpi) / sum(mpi), y = seq_along(mpi) / length(mpi))) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(title = "Lorenz Curve of MPI", x = "Cumulative Share of MPI", y = "Cumulative Share of Population")

# Save report to a text file
writeLines(report, "poverty_analysis_report.txt")

# Print confirmation
cat("Analysis complete. Report saved as 'poverty_analysis_report.txt'")

