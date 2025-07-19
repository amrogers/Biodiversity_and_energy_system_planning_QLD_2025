library(readxl)
library(dplyr)
library(writexl)

# Paths
input_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/model_tx_summaries/ex_mod_join_tables"
output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/QLD_ex_mod_summaries"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # recursive = TRUE creates parent directories if needed
}


thresholds <- c(0, 10, 30, 50, 70, 90) #0, 10, 30, 50, 70, 90

# Loop through thresholds
for (t in thresholds) {
  file_name <- paste0("transmission_y2050_t", t, ".xlsx")
  file_path <- file.path(input_dir, file_name)
  
  # Check if the input file exists before trying to read it.
  if (!file.exists(file_path)) {
    print(paste0("Warning: Input file not found: ", file_path))
    next # Skip to the next iteration of the loop
  }
  
  # Read data
  df <- read_excel(file_path)
  
  # Filter out rows where kv is null or 0
  df_filtered <- df %>% filter(!is.na(kv) & kv != 0)
  
  # Summarize data
  df_summarized <- df_filtered %>%
    group_by(TARGET_FID) %>%
    summarize(
      max_mod_kv = max(kv, na.rm = TRUE),
      max_length_km = max(length_k_1, na.rm = TRUE),
      max_ex_kv = max(max_ex_kv, na.rm = TRUE),
      max_ex_easement = max(ex_easemen, na.rm = TRUE),
      count_existing = max(count_exis, na.rm = TRUE) # Count of existing lines for each FID
    ) %>%
    mutate(
      mod_tx_easement = case_when(
        max_mod_kv < 100 ~ 0.02,
        max_mod_kv >= 100 & max_mod_kv <= 275 ~ 0.04,
        max_mod_kv == 330 ~ 0.06,
        max_mod_kv == 500 ~ 0.07,
        TRUE ~ NA_real_  # Handle cases outside defined ranges
      ),
      area_ex = max_ex_easement * max_length_km,
      area_mod = mod_tx_easement * max_length_km,
      area_diff = ifelse(count_existing > 1, (area_mod - area_ex) / 2, area_mod - area_ex)
    )
  
  
  # Save summarized data
  output_file_name <- paste0("summarized_transmission_t", t, ".xlsx")
  output_file_path <- file.path(output_dir, output_file_name)
  write_xlsx(df_summarized, output_file_path)
  
}

# Create total area increase table

total_area_increase <- data.frame(
  threshold = thresholds,
  total_area_diff = NA_real_,
  percent_increase = NA_real_
)

for (t in thresholds) {
  summary_file_name <- paste0("summarized_transmission_t", t, ".xlsx")
  summary_file_path <- file.path(output_dir, summary_file_name)
  
  # Check if the summary file exists before trying to read it.
  if (!file.exists(summary_file_path)) {
    print(paste0("Warning: Summary file not found: ", summary_file_path))
    next # Skip to the next iteration of the loop
  }
  
  df_summary <- read_excel(summary_file_path)
  
  total_area_diff <- sum(df_summary$area_diff, na.rm = TRUE) #na.rm = TRUE to deal with any NA values created in the case_when statement
  total_area_increase$total_area_diff[total_area_increase$threshold == t] <- total_area_diff
  total_area_increase$percent_increase[total_area_increase$threshold == t] <- (total_area_diff / 663.64) * 100 #Percent increase
}

# Save total area increase table
total_area_increase_file <- file.path(output_dir, "total_area_increase.xlsx")
write_xlsx(total_area_increase, total_area_increase_file)


print("Processing complete. Files saved to output directory.")


#both scenarios
library(readxl)
library(dplyr)
library(writexl)

# Scenarios to process (choose "tx1", "tx2", or both)
scenarios <- c("tx1", "tx2")  # Or just c("tx1") or c("tx2")

for (scenario in scenarios) {
  # Paths (dynamic based on scenario)
  input_dir <- paste0("Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/", scenario, "_domestic_transmission/model_tx_summaries/ex_mod_join_tables")
  output_dir <- paste0("Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/", scenario, "_domestic_transmission/QLD_ex_mod_summaries")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  thresholds <- c(0, 10, 30, 50, 70, 90) #0, 10, 30, 50, 70, 90
  
  # Loop through thresholds
  for (t in thresholds) {
    file_name <- paste0("transmission_y2050_t", t, ".xlsx")
    file_path <- file.path(input_dir, file_name)
    
    # Check if the input file exists before trying to read it.
    if (!file.exists(file_path)) {
      print(paste0("Warning: Input file not found: ", file_path))
      next # Skip to the next iteration of the loop
    }
    
    # Read data
    df <- read_excel(file_path)
    
    # Filter out rows where kv is null or 0
    df_filtered <- df %>% filter(!is.na(kv) & kv != 0)
    
    # Summarize data
    df_summarized <- df_filtered %>%
      group_by(TARGET_FID) %>%
      summarize(
        max_mod_kv = max(kv, na.rm = TRUE),
        max_length_km = max(length_k_1, na.rm = TRUE),
        max_ex_kv = max(max_ex_kv, na.rm = TRUE),
        max_ex_easement = max(ex_easemen, na.rm = TRUE),
        count_existing = max(count_exis, na.rm = TRUE) # Count of existing lines for each FID
      ) %>%
      mutate(
        mod_tx_easement = case_when(
          max_mod_kv < 100 ~ 0.02,
          max_mod_kv >= 100 & max_mod_kv <= 275 ~ 0.04,
          max_mod_kv == 330 ~ 0.06,
          max_mod_kv == 500 ~ 0.07,
          TRUE ~ NA_real_  # Handle cases outside defined ranges
        ),
        area_ex = max_ex_easement * max_length_km,
        area_mod = mod_tx_easement * max_length_km,
        area_diff = ifelse(count_existing > 1, (area_mod - area_ex) / 2, area_mod - area_ex)
      )
    
    
    # Save summarized data
    output_file_name <- paste0("summarized_transmission_t", t, ".xlsx")
    output_file_path <- file.path(output_dir, output_file_name)
    write_xlsx(df_summarized, output_file_path)
  
    
    # Save summarized data
    output_file_name <- paste0("summarized_transmission_t", t, ".xlsx")
    output_file_path <- file.path(output_dir, output_file_name)
    write_xlsx(df_summarized, output_file_path)
    
  }
  
  # Create total area increase table
  
  total_area_increase <- data.frame(
    threshold = thresholds,
    total_area_diff = NA_real_,
    percent_increase = NA_real_
  )
  
  for (t in thresholds) {
    summary_file_name <- paste0("summarized_transmission_t", t, ".xlsx")
    summary_file_path <- file.path(output_dir, summary_file_name)
    
    # Check if the summary file exists before trying to read it.
    if (!file.exists(summary_file_path)) {
      print(paste0("Warning: Summary file not found: ", summary_file_path))
      next # Skip to the next iteration of the loop
    }
    
    df_summary <- read_excel(summary_file_path)
    
    total_area_diff <- sum(df_summary$area_diff, na.rm = TRUE) #na.rm = TRUE to deal with any NA values created in the case_when statement
    total_area_increase$total_area_diff[total_area_increase$threshold == t] <- total_area_diff
    total_area_increase$percent_increase[total_area_increase$threshold == t] <- (total_area_diff / 663.64) * 100 #Percent increase
  }
  
  # Save total area increase table
  total_area_increase_file <- file.path(output_dir, paste0("total_area_increase_", scenario, ".xlsx"))
  write_xlsx(total_area_increase, total_area_increase_file)
  
  print(paste0("Processing for ", scenario, " complete. Files saved to output directory."))
} # End of scenario loop

scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/model_tx_summaries/ex_mod_join_tables")
