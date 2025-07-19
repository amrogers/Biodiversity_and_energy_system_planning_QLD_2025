# Load necessary libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(gridExtra)  # Use gridExtra for combining plots
library(extrafont)

loadfonts(device = "win")
windowsFonts(Arial = windowsFont("TT Arial"))

#set working directory - uncomment and modify as needed
setwd("C:/Users/andrewrogers/OneDrive - The University of Melbourne/zonation/Test/")

# Specify the output folder path
#SNES100 run with CAPAD and Red Zones
output_folder <- "Vic_100m_SNES_ECNES/out_example1/"

#SNES_public run with CAPAD
#output_folder <- "Vic_100m_SNES_ECNES/out_example1"

# Read in data
input_file <- paste0(output_folder, "/feature_curves.csv")
if (file.exists(input_file)) {
  df <- read.csv(input_file)
  print(paste("Successfully loaded file with", ncol(df), "columns"))
  print("First few column names:")
  print(head(names(df), 10))
} else {
  print("File not found.")
}

# Calculate mean across all feature columns
# Identify feature columns (all columns except 'rank')
feature_cols <- setdiff(names(df), "rank")
print(paste("Number of feature columns:", length(feature_cols)))

# Add mean column to dataset
df$mean <- rowMeans(df[, feature_cols], na.rm = TRUE)

# Add standard deviation and confidence interval columns
df$sd <- apply(df[, feature_cols], 1, sd, na.rm = TRUE)
df$n <- length(feature_cols)
df$se <- df$sd / sqrt(df$n)
df$ci_lower <- df$mean - 1.96 * df$se
df$ci_upper <- df$mean + 1.96 * df$se

# Print summary of the mean and CI values
summary_stats <- df %>%
  summarise(
    min_mean = min(mean, na.rm = TRUE),
    max_mean = max(mean, na.rm = TRUE),
    min_ci = min(ci_lower, na.rm = TRUE),
    max_ci = max(ci_upper, na.rm = TRUE)
  )
print("Summary of calculated statistics:")
print(summary_stats)

##----------------line plots------------------------------

# Reshape dataframe for feature lines
df_long <- df %>%
  gather(key = "Feature", value = "Value", all_of(feature_cols))

# Create the plot with confidence intervals
ggplot() +
  # Add light grey lines for individual features
  geom_line(data = df_long, aes(x = rank, y = Value, group = Feature), 
            color = "grey", alpha = 0.3) +
  # Add dark black line for the mean
  geom_line(data = df, aes(x = rank, y = mean), 
            color = "black", linewidth = 1) +
  # Add confidence interval ribbon
  geom_ribbon(data = df, aes(x = rank, ymin = ci_lower, ymax = ci_upper),
              fill = "blue", alpha = 0.2) +
  # Customize the plot
  labs(x = "Priority rank",
       y = "Average distribution coverage") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(size = 24, family = "Arial"), 
        axis.text = element_text(color = "black")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Save the plot (optional)
output_filename <- paste0(output_folder, "Performance_curve_line_plot_with_CI.tif") 
ggsave(output_filename, width = 8, height = 8, dpi = 600)

###------------------------summarize features at full coverage--------------

# Function to count features with full coverage (allowing for small numerical differences)
count_full_coverage <- function(data, rank_min, rank_max, threshold = 0.99) {
  # Filter for the rank range
  filtered_data <- data %>%
    filter(rank >= rank_min & rank <= rank_max)
  
  # Get feature columns (excluding rank, mean, and statistics columns)
  feature_cols <- setdiff(names(filtered_data), c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
  
  # Count features that reach full coverage (>= threshold) at any point in this range
  full_coverage_counts <- filtered_data %>%
    select(all_of(feature_cols)) %>%
    summarise(across(everything(), ~max(.x, na.rm = TRUE) >= threshold)) %>%
    summarise(count = sum(.))
  
  return(full_coverage_counts$count)
}

# Function to get statistics at specific rank (or nearest rank)
get_stats_at_rank <- function(data, target_rank) {
  # Find the row with the closest rank value
  closest_row <- data %>%
    mutate(rank_diff = abs(rank - target_rank)) %>%
    arrange(rank_diff) %>%
    slice(1)
  
  return(data.frame(
    mean_value = closest_row$mean,
    ci_lower = closest_row$ci_lower,
    ci_upper = closest_row$ci_upper
  ))
}

# Create a data frame with the results for each range
rank_ranges <- data.frame(
  range = c("BAU", "0.9-1.0", "0.7-1.0", "0.5-1.0", "0.3-1.0", "0.1-1.0"),
  min_rank = c(0.87, 0.9, 0.7, 0.5, 0.3, 0.1),
  max_rank = 1.0
)

# Calculate counts and statistics for each range
results <- rank_ranges %>%
  rowwise() %>%
  mutate(
    feature_count = count_full_coverage(df, min_rank, max_rank),
    
    zero_coverage = {
      # Count features with exactly 0 coverage
      filtered_data <- df %>% filter(rank >= min_rank & rank <= max_rank)
      feature_cols <- setdiff(names(filtered_data), 
                              c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
      
      zero_counts <- filtered_data %>%
        select(all_of(feature_cols)) %>%
        summarise(across(everything(), ~max(.x, na.rm = TRUE) == 0)) %>%
        summarise(count = sum(.))
      
      as.numeric(zero_counts)
    },
    
    less_than_50pct = {
      # Count features with less than 50% coverage
      filtered_data <- df %>% filter(rank >= min_rank & rank <= max_rank)
      feature_cols <- setdiff(names(filtered_data), 
                              c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
      
      low_counts <- filtered_data %>%
        select(all_of(feature_cols)) %>%
        summarise(across(everything(), ~max(.x, na.rm = TRUE) < 0.5)) %>%
        summarise(count = sum(.))
      
      as.numeric(low_counts)
    },
    
    more_than_50pct = {
      # Count features with 50% or more coverage
      filtered_data <- df %>% filter(rank >= min_rank & rank <= max_rank)
      feature_cols <- setdiff(names(filtered_data), 
                              c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
      
      high_counts <- filtered_data %>%
        select(all_of(feature_cols)) %>%
        summarise(across(everything(), ~max(.x, na.rm = TRUE) >= 0.5)) %>%
        summarise(count = sum(.))
      
      as.numeric(high_counts)
    }
  ) %>%
  ungroup()

# Add statistics at min_rank
stats_at_ranks <- lapply(rank_ranges$min_rank, function(r) get_stats_at_rank(df, r))
stats_df <- do.call(rbind, stats_at_ranks)

# Combine with results
results <- cbind(results, stats_df)

# Round numeric columns for display
results <- results %>%
  mutate(across(c(mean_value, ci_lower, ci_upper), ~round(.x, 3)))

# Print results
print(results)

# Optional: Calculate the total number of features for reference
total_features <- length(setdiff(names(df), c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper")))
cat("\nTotal number of features:", total_features)

# Save the results to a CSV file (optional)
write.csv(results, paste0(output_folder, "feature_coverage_summary_with_CI.csv"), row.names = FALSE)

##----------------Histograms-------------------------------

# Assuming your data frame is named 'df'
# Melt the data to long format, keeping 'rank' and 'mean' fixed
df_long <- df %>%
  pivot_longer(cols = -c(rank, mean), names_to = "feature", values_to = "coverage")

# Filter for the desired rank range
df_filtered <- df_long %>%
  filter(rank >= 0.1 & rank <= 1.0)

# Filter data for the rank range
#df_filtered <- df %>%
#  filter(rank >= rank_range[1] & rank <= rank_range[2])

# # Summarize the data, focusing on the columns of interest
# summarized_data <- df_filtered %>%
#   pivot_longer(cols = -c(rank), names_to = "feature", values_to = "coverage") %>%
#   group_by(feature) %>%
#   summarize(
#     max_coverage = max(coverage)
#   )
# 
# # Create the histogram
# ggplot(summarized_data, aes(x = max_coverage)) +
#   geom_histogram(binwidth = .1, fill = "lightgrey", color = "black") + # Adjust binwidth as needed
#   #scale_x_binned(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
#   #scale_x_continuous(breaks = 0:1, labels= 0:1) +
#   scale_x_continuous(limits = c(-0.2, 1.2), breaks = seq(0, 1, by = 0.1), 
#                      labels = seq(0, 1, by = 0.1)) +
#   labs(
#     #title = "Distribution of Feature Coverages (Rank 0.9 - 1.0)",
#     x = "Feature coverage",
#     y = "Feature count"
#   ) +
#   theme_minimal()
# 
# # Save the plot (optional)
# ggsave("Test/250m_SNES_ECNES_red_zones_weighted/out_example1/GT90_plot.tif", width = 10, height = 6, dpi = 300)


###----------------------iterate histograms ---------------------------------
# Define rank ranges
rank_ranges <- list(c(0.1, 1), c(0.3, 1), c(0.5, 1), c(0.7, 1), c(0.8, 1), c(0.85, 1), c(0.9, 1))

# Iterate over rank ranges and create histograms
for (i in 1:length(rank_ranges)) {
  # Filter data for the current rank range
  df2_filtered <- df2 %>%
    filter(rank >= rank_ranges[[i]][1] & rank <= rank_ranges[[i]][2])
  
  # Summarize the data
  summarized_data <- df2_filtered %>%
    pivot_longer(cols = -c(rank), names_to = "feature", values_to = "coverage") %>%
    group_by(feature) %>%
    summarize(
      max_coverage = max(coverage)
    )
  
  # Create the histogram for maximum coverage
  p <- ggplot(summarized_data, aes(x = max_coverage)) +
    geom_histogram(binwidth = .1, fill = "lightgrey", color = "black") + # Adjust binwidth as needed
    #scale_x_binned(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
    #scale_x_continuous(breaks = 0:1, labels= 0:1) +
    scale_x_continuous(limits = c(-0.2, 1.2), breaks = seq(0, 1, by = 0.1), 
                       labels = seq(0, 1, by = 0.1)) +
    labs(
      #title = "Distribution of Feature Coverages (Rank 0.9 - 1.0)",
      x = "Feature coverage",
      y = "Feature count"
    ) +
    theme_minimal()+
    theme(axis.text = element_text(size = 6))
  
  # Save the plot to the specified output folder with rank range in the filename
  filename <- paste0(output_folder, "/histogram_rank_", rank_ranges[[i]][1], "_to_", rank_ranges[[i]][2], ".tif")
  ggsave(filename, p)
}



##----------------plot all histograms on one plot ----------
# Define rank ranges
rank_ranges <- list(c(0.1, 1), c(0.3, 1), c(0.5, 1), c(0.7, 1), c(0.9, 1)) # c(0.8, 1), c(0.85, 1),

# Prepare data for multi-histogram plot
multi_hist_data <- lapply(seq_along(rank_ranges), function(i) {
  # Filter data for the current rank range
  df2_filtered <- df2 %>%
    filter(rank >= rank_ranges[[i]][1] & rank <= rank_ranges[[i]][2])
  
  # Summarize the data
  summarized_data <- df2_filtered %>%
    pivot_longer(cols = -c(rank), names_to = "feature", 
                 values_to = "coverage") %>%
    group_by(feature) %>%
    summarize(
      max_coverage = max(coverage)
    ) %>%
    mutate(rank_range = paste0(rank_ranges[[i]][1], " - ",
                               rank_ranges[[i]][2]))
  
  return(summarized_data)
}) %>% 
  do.call(rbind, .)

# Create the multi-histogram plot
ggplot(multi_hist_data, aes(x = max_coverage, fill = rank_range)) +
  geom_histogram(binwidth = 0.1, color = "black", position = "identity",
                 alpha = 0.6) +
  scale_x_continuous(limits = c(-0.2, 1.2), breaks = seq(0, 1, by = 0.1), 
                     labels = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c(
    "0.1 - 1" = "#E6F2E6",     # Lightest green
    "0.3 - 1" = "#C5E1C5",
    "0.5 - 1" = "#A4D0A4",
    "0.7 - 1" = "#83BF83",
    "0.8 - 1" = "#62AE62",
    "0.85 - 1" = "#419D41",
    "0.9 - 1" = "darkgreen"      # Darkest green
  )) +
  labs(
    x = "Feature coverage",
    y = "Feature count",
    fill = "Rank Range"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10,family="arial"),
    legend.text = element_text(size = 8,family="arial"),
    
  )

# Save the plot to the specified output folder
ggsave(paste0(output_folder, "/multi_histogram_rank_ranges.tif"), 
       width = 10, height = 6, 
       dpi = 600)

##-----------------------------------------------------------------

# Define rank ranges 
rank_ranges <- list(c(0.1, 1), c(0.3, 1), c(0.5, 1), c(0.7, 1), c(0.9, 1)) 

# Prepare data for histogram plot
prepare_hist_data <- function(df2, rank_ranges) {
  multi_hist_data <- lapply(seq_along(rank_ranges), function(i) {
    # Filter data for the current rank range
    df2_filtered <- df2 %>% 
      filter(rank >= rank_ranges[[i]][1] & rank <= rank_ranges[[i]][2])
    
    # Summarize the data 
    summarized_data <- df2_filtered %>% 
      pivot_longer(cols = -c(rank), names_to = "feature", values_to = "coverage") %>% 
      group_by(feature) %>% 
      summarize(
        max_coverage = max(coverage) 
      ) %>% 
      mutate(
        rank_range = paste0(rank_ranges[[i]][1], " - ", rank_ranges[[i]][2]) 
      )
    
    return(summarized_data)
  }) %>% 
    do.call(rbind, .)
  
  return(multi_hist_data)
}

# Prepare data 
multi_hist_data <- prepare_hist_data(df2, rank_ranges)

# Create color palette
green_palette <- c(
  "0.1 - 1" = "#E6F2E6",    
  "0.3 - 1" = "#C5E1C5",
  "0.5 - 1" = "#A4D0A4",
  "0.7 - 1" = "#83BF83",
  "0.9 - 1" = "#208D20"     
)
#reverse pallet
green_palette <- c(
  "0.9 - 1" = "grey94",    
  "0.7 - 1" = "grey70",
  "0.5 - 1" = "grey60",
  "0.3 - 1" = "grey30",
  "0.1 - 1" = "grey20"     
)


# Log-transformed histogram (transforming the y-axis)
p2 <- ggplot(multi_hist_data, aes(x = max_coverage, fill = factor(rank_range, levels = rev(names(green_palette))))) +
  geom_histogram(binwidth = 0.1, color = "black", position = "identity", alpha = 0.5) + 
  scale_x_continuous(limits = c(-0.2, 1.2), breaks = seq(0, 1, by = 0.1), 
                     labels = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = green_palette) +
  labs(
    title = "Log-Transformed Data (Y-axis)",
    x = "Feature coverage",
    y = "Log(Feature count)",
    fill = "Rank Range"
  ) +
  scale_y_continuous(trans = "log1p") +  # Apply log transformation to y-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


p2

# Save the plot
ggsave(paste0(output_folder, "/log_transformed_histogram_tall.tif"), 
       p2,
       width = 6, 
       height = 12, 
       dpi = 600)