# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(here)
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# Read the data
data <- read.csv(file.path(data_root, "Energy_system_model_outputs", "BV_exclusion_area_overlap.csv"))

# Rename first column to something meaningful
colnames(data)[1] <- "threshold"

# Set the order of thresholds
data$threshold <- factor(data$threshold, 
                         levels = c("70-100", "50-70", "30-50", "0-30"))

# Reshape data for wind
wind_data <- data %>%
  select(threshold, wind_exclusion, wind_available_area, wind_exclusion_percent) %>%
  pivot_longer(cols = c(wind_exclusion, wind_available_area),
               names_to = "category",
               values_to = "area") %>%
  mutate(type = "Wind",
         category_type = paste0("Wind_", ifelse(category == "wind_exclusion", 
                                                "Exclusion", "Available")),
         category_type = factor(category_type, levels = c("Wind_Available", 
                                                          "Wind_Exclusion")),
         percent = wind_exclusion_percent,
         area = area / 1000)

# Reshape data for PV
pv_data <- data %>%
  select(threshold, pv_exlcusion, pv_available_land, pv_exlcuion_percent) %>%
  pivot_longer(cols = c(pv_exlcusion, pv_available_land),
               names_to = "category",
               values_to = "area") %>%
  mutate(type = "PV",
         category_type = paste0("PV_", ifelse(category == "pv_exlcusion", "Exclusion", "Available")),
         category_type = factor(category_type, levels = c("PV_Available", "PV_Exclusion")),
         percent = pv_exlcuion_percent,
         area = area / 1000)

# Combine datasets
plot_data <- bind_rows(wind_data, pv_data)

# Set factor order for type so PV appears first
plot_data$type <- factor(plot_data$type, levels = c("PV", "Wind"))

# Calculate total for each threshold-type combination for label positioning
totals <- plot_data %>%
  group_by(threshold, type) %>%
  summarise(total = sum(area),
            percent = first(percent),
            .groups = "drop")

# Threshold label mapping
thresh_labels <- c(
  "0-30"   = "Top 90%",
  "30-50"  = "Top 70%",
  "50-70"  = "Top 50%",
  "70-100" = "Top 30%"
)

# Create the stacked bar plot with facets
p <- ggplot(plot_data, aes(x = threshold, y = area, fill = category_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(data = totals,
            aes(x = threshold, y = total,
                label = paste0(round(percent * 100, 1), "%"), fill = NULL),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Wind_Available" = "#87CEEB",
                               "Wind_Exclusion" = "#8B4513",
                               "PV_Available"   = "#FFD700",
                               "PV_Exclusion"   = "#FF8C00"),
                    breaks = c("PV_Available", "PV_Exclusion",
                               "Wind_Available", "Wind_Exclusion"),
                    labels = c("PV available", "PV exclusion",
                               "Wind available", "Wind exclusion"),
                    name = "Category") +
  scale_x_discrete(labels = thresh_labels) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600)) +
  labs(
    x = NULL,
    y = "Area (k ha)"
  ) +
  facet_wrap(~ type, scales = "free_x", strip.position = "bottom") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    strip.placement = "outside",
    panel.spacing = unit(1.5, "lines")
  )

# Display the plot
print(p)

# Save the plot
ggsave(here("results", "figures", "Exclusions_stacked_bar_plot.png"),
       plot = p, width = 10, height = 6, dpi = 300)

# Print summary statistics
cat("\nSummary Statistics:\n")
print(totals)