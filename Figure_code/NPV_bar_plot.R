# Load required libraries
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggpattern)
library(forcats)
library(cowplot)
library(grid)
library(gridExtra)

# Read both files
npv_data <- read_excel("C:/Users/andrewrogers/OneDrive - The University of Melbourne/Project_documents/results/eplus_Domestic_NPV_2025.xlsx",
                       col_types = c("text", "text", "numeric", "numeric", "numeric"))  # Explicitly set column types

percent_data <- read_excel("C:/Users/andrewrogers/OneDrive - The University of Melbourne/Project_documents/results/Build percent_build_vs_trans.xlsx",
                           col_types = c("text", "text", "numeric", "numeric", "numeric"))

# Process NPV data and ensure thresholds ordering
npv_long <- npv_data %>%
  pivot_longer(
    cols = c(`2030`, `2040`, `2050`),
    names_to = "year",
    values_to = "value"
  ) %>%
  # Filter out the 0.1 threshold
  filter(thresholds != "0.1") %>%
  # Use direct factor() with explicit levels, removing 0.1
  mutate(thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")))

# Process percent data
percent_long <- percent_data %>%
  pivot_longer(
    cols = c(`2030`, `2040`, `2050`),
    names_to = "year",
    values_to = "percent"
  ) %>%
  # Filter out the 0.1 threshold to match
  filter(thresholds != "0.1")

# Combine the datasets
combined_data <- npv_long %>%
  left_join(percent_long, by = c("scenario", "thresholds", "year")) %>%
  mutate(build_value = value * percent) %>%
  # Reapply factor after join to ensure ordering is preserved, without 0.1
  mutate(thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")))

# Print to verify the levels (optional debugging)
print("Threshold levels:")
print(levels(combined_data$thresholds))
print("Unique threshold values:")
print(unique(combined_data$thresholds))

# Create the plot
p <- ggplot(combined_data, aes(x = factor(year))) +
  # Base bars (total NPV)
  geom_col_pattern(
    aes(y = value, fill = thresholds),
    pattern = 'none',
    colour = "black",
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  # Overlay pattern bars (build portion)
  geom_col_pattern(
    aes(y = build_value, fill = thresholds),
    pattern = 'stripe',
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    colour = "black",  # Add black outline to the patterned bars
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
  # Faceting
  facet_wrap(~scenario) +
  
  # Customize appearance
  theme_minimal() +
  labs(
    x = "Year",
    y = "NPV (billion AUD)",
    fill = "Thresholds"
  ) +
  
  # Theme customization
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 20),
    strip.background = element_rect(fill = "light grey", color = NA),
    # Remove pattern from legend keys
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(1.5, "cm")
  ) +
  
  # Color scheme
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

# Fix the main plot legend
p <- p + guides(
  fill = guide_legend(
    override.aes = list(pattern = "none"),  # Override the pattern in threshold legend
    title = "Thresholds",
    label.hjust = 0,       # Left-align the labels
    label.position = "right",
    label.theme = element_text(margin = margin(l = 10, r = 0)),  # Add left margin to labels
    keywidth = unit(1.0, "cm"),
    keyheight = unit(1.0, "cm")
  )
) +
  # Ensure consistent legend key size and text size
  theme(
    legend.key.size = unit(1.0, "cm"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.spacing.x = unit(0.5, "cm"),  # Increase horizontal spacing
    legend.background = element_rect(fill = NA, color = NA),  # Remove legend background border
    legend.box.background = element_rect(fill = NA, color = NA),  # Remove legend box background border
    plot.margin = margin(20, 40, 20, 40, "pt")  # top, right, bottom, left padding
  )

# Create a minimal plot for the component legend with proper borders
legend_plot <- ggplot(
  data.frame(
    x = c(1, 1),
    y = c(1, 2),
    fill = c("Added transmission", "Solar and wind projects")
  ),
  aes(x = x, y = y, fill = fill)
) +
  geom_col_pattern(
    aes(pattern = fill),  # Map pattern to fill
    colour = "black",     # Keep the black border around the symbols
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    width = 0.7
  ) +
  scale_pattern_manual(
    values = c(
      "Added transmission" = "none",
      "Solar and wind projects" = "stripe"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Added transmission" = "white",
      "Solar and wind projects" = "white"
    ),
    name = NULL
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.0, "cm"),
    legend.background = element_rect(fill = NA, color = NA),  # Make legend background transparent, no border
    legend.key = element_rect(fill = "white", color = "black"), # Keep black border around the keys
    legend.box.background = element_rect(fill = NA, color = NA),  # Make legend box background transparent, no border
    legend.box.margin = margin(0, 0, 0, 0),  # Remove margin around the legend box
    legend.spacing.x = unit(0.5, "cm")  # Increase horizontal spacing
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        pattern = c("none", "stripe"),
        fill = c("white", "white")
      ),
      # Add margin to the left of text labels
      label.theme = element_text(margin = margin(l = 10, r = 0)),
      label.hjust = 0,       # Left-align the labels
      label.position = "right",
      # Specify legend element dimensions
      keywidth = unit(1.0, "cm"),
      keyheight = unit(1.0, "cm")
    )
  )

# Extract the legend from this minimal plot
component_legend_grob <- cowplot::get_legend(legend_plot)

# Extract legends with minimal spacing - apply same fixes to remove borders around text
threshold_legend <- get_legend(
  p + 
    theme(
      legend.box.margin = margin(0, 0, 0, 0),
      legend.background = element_rect(fill = NA, color = NA),
      legend.box.background = element_rect(fill = NA, color = NA),
      legend.spacing.x = unit(0.5, "cm")  # Increase horizontal spacing
    ) +
    guides(
      fill = guide_legend(
        label.theme = element_text(margin = margin(l = 10, r = 0)),
        label.hjust = 0,  # Left-align the labels
        label.position = "right"
      )
    )
)

# Create the final plot with tighter spacing between legends and proper vertical alignment
final_plot <- cowplot::plot_grid(
  p + theme(legend.position = "none"),  # Plot without legend
  cowplot::plot_grid(
    NULL,  # Add empty space at top
    threshold_legend,
    component_legend_grob,
    NULL,  # Add empty space at bottom
    ncol = 1,
    align = 'v',
    rel_heights = c(0.5, 4, 2, 0.5),  # Add space above and below
    axis = 'l',
    greedy = FALSE,
    spacing = 0.01  # Reduce spacing between legends
  ),
  rel_widths = c(3.5, .8),  # Smaller second value brings legend closer to plot
  align = "h",
  nrow = 1,
  axis = "tb"
)


final_plot

# Save the final plot with both legends
ggsave(plot = final_plot,
       filename = "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Project_documents/Images/npv_domestic_plot_with_build_2025AUD.png",
       width = 18, height = 7, dpi = 300,
       device = "png",
       bg = "white",
       units = "in",
       limitsize = FALSE)
