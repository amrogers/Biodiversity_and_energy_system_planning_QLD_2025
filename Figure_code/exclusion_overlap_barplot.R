# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Read the data
data <- read.csv("C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Project_documents\\results\\BV_exlusion_area_overlap.csv")

# Rename first column to something meaningful
colnames(data)[1] <- "threshold"

# Set the order of thresholds (reversed so 70-100 appears first)
data$threshold <- factor(data$threshold, levels = rev(unique(data$threshold)))

# Reshape data for wind
wind_data <- data %>%
  select(threshold, wind_exclusion, wind_available_area, wind_exclusion_percent) %>%
  pivot_longer(cols = c(wind_exclusion, wind_available_area),
               names_to = "category",
               values_to = "area") %>%
  mutate(type = "Wind",
         category_type = paste0("Wind_", ifelse(category == "wind_exclusion", "Exclusion", "Available")),
         category_type = factor(category_type, levels = c("Wind_Available", "Wind_Exclusion")),
         percent = wind_exclusion_percent)

# Reshape data for PV
pv_data <- data %>%
  select(threshold, pv_exlcusion, pv_available_land, pv_exlcuion_percent) %>%
  pivot_longer(cols = c(pv_exlcusion, pv_available_land),
               names_to = "category",
               values_to = "area") %>%
  mutate(type = "PV",
         category_type = paste0("PV_", ifelse(category == "pv_exlcusion", "Exclusion", "Available")),
         category_type = factor(category_type, levels = c("PV_Available", "PV_Exclusion")),
         percent = pv_exlcuion_percent)

# Combine datasets
plot_data <- bind_rows(wind_data, pv_data)

# Calculate total for each threshold-type combination for label positioning
totals <- plot_data %>%
  group_by(threshold, type) %>%
  summarise(total = sum(area),
            percent = first(percent),
            .groups = "drop")

# Create the stacked bar plot
p <- ggplot(plot_data, aes(x = interaction(threshold, type), y = area, fill = category_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(data = totals, 
            aes(x = interaction(threshold, type), y = total, 
                label = paste0(round(percent * 100, 1), "%"), fill = NULL),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Wind_Available" = "#87CEEB",
                               "Wind_Exclusion" = "#8B4513", 
                               "PV_Available" = "#FFD700",
                               "PV_Exclusion" = "#FF8C00"),
                    labels = c("Wind Available", "Wind Exclusion", 
                               "PV Available", "PV Exclusion"),
                    breaks = c("PV_Available", "PV_Exclusion", 
                               "Wind_Available", "Wind_Exclusion"),
                    name = "Category") +
  labs(#title = "Wind and PV Land Exclusion and Availability",
    #subtitle = "By threshold level",
    x = "Threshold and Energy Type",
    y = "Area (km²)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom") +
  scale_x_discrete(labels = function(x) {
    parts <- strsplit(as.character(x), "\\.")
    sapply(parts, function(p) paste(p[1], p[2], sep = "\n"))
  })

# Display the plot
print(p)

# Optional: Save the plot
ggsave("C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Project_documents\\Images\\Exclusions_stacked_bar_plot.png", 
       plot = p, width = 10, height = 6, dpi = 300)

# Print summary statistics
cat("\nSummary Statistics:\n")
print(totals)