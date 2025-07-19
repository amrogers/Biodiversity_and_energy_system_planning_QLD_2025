# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Read the data file
data <- read_csv("Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/cost_increase_results.csv")

# Simplified data processing with fix for NA in x-axis
plot_data <- data.frame(
  tx_scenario = data[[1]],
  HBVA_exclusion = data[[2]],
  residential = data[[3]],
  industrial = data[[4]]
)

# Fix: Clean HBVA values and ensure they match expected categories
plot_data$HBVA_exclusion <- trimws(plot_data$HBVA_exclusion)  # Remove whitespace

# Convert to long format and apply fixes for NA categories
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(residential, industrial),
    names_to = "cost_type",
    values_to = "cost_increase"
  ) %>%
  # Remove rows with NA cost values
  filter(!is.na(cost_increase)) %>%
  # Only keep rows with expected HBVA values
  filter(HBVA_exclusion %in% c("BAU", "30", "50", "70", "90")) %>%
  # Set factor levels to ensure proper ordering
  mutate(HBVA_exclusion = factor(HBVA_exclusion, levels = c("BAU", "30", "50", "70", "90")))

# Create the plot
ggplot(plot_data_long, 
       aes(x = HBVA_exclusion, 
           y = cost_increase,
           group = interaction(tx_scenario, cost_type),
           color = cost_type,
           linetype = tx_scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_linetype_manual(values = c(
    "tx_1" = "solid",
    "tx_2" = "longdash"  # Using longdash instead of dashed for better visibility
  )) +
  scale_color_brewer(palette = "Set1", name = "Cost Type") +
  theme_minimal() +
  labs(
    x = "% High Biodiversity Value Area Excluded",
    y = "Increase in Energy Costs (%)",
    #title = "Energy Cost Increases by HBVA Exclusion Level",
    color = "Cost Type",
    linetype = "Transmission Scenario"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key.width = unit(3, "cm"),  # Wider keys to better see line patterns
    legend.key.height = unit(1, "cm"),  # Taller keys
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
  )

# Uncomment to save the plot
 ggsave(filename = "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Project_documents/Images/energy_cost_plot.png",
        width = 12, height = 6, dpi = 300, bg = "white")
 