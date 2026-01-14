<<<<<<< HEAD
# =============================================================================
# MASTER RUNNER: Biodiversity and Energy System Planning QLD 2025
# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================
# Purpose: This script executes the full analysis pipeline in the correct order.
# =============================================================================

# 1. INITIAL SETUP
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, magick)

cat("\n🚀 Starting Full Analysis Pipeline...\n")
start_time <- Sys.time()

# 2. DEFINE PIPELINE STEPS
# We order these so that data processing happens before complex mapping
pipeline <- list(
  "Species Coverage" = here("species_code", "critically_endangered_coverage.R"),
  "Zonation Curves"  = here("Figure_code", "Zonation curves.R"),
  "Cost Analysis"    = here("Figure_code", "percent cost increase_line plot.R"),
  "NPV Analysis"     = here("Figure_code", "NPV_bar_plot.R"),
  "Spatial Mapping"  = here("Figure_code", "2050_domestic_CPA_comparison.R")
)

# 3. EXECUTION ENGINE
results_log <- data.frame(
  Step = names(pipeline),
  Status = "Pending",
  Time = NA,
  stringsAsFactors = FALSE
)

for (i in seq_along(pipeline)) {
  step_name <- names(pipeline)[i]
  step_path <- pipeline[[i]]
  
  cat(sprintf("\n--- Running Step %d/%d: %s ---\n", i, length(pipeline), step_name))
  
  if (file.exists(step_path)) {
    tryCatch({
      source(step_path)
      results_log$Status[i] <- "Success ✅"
    }, error = function(e) {
      results_log$Status[i] <- paste("Failed ❌:", e$message)
    })
  } else {
    results_log$Status[i] <- "File Not Found ❌"
  }
  results_log$Time[i] <- format(Sys.time(), "%H:%M:%S")
}

# 4. FINAL STATUS REPORT
cat("\n" , "="*40, "\n")
cat("FINAL PIPELINE REPORT\n")
cat("="*40, "\n")
print(results_log, row.names = FALSE)

end_time <- Sys.time()
cat(sprintf("\nTotal Runtime: %0.2f minutes\n", as.numeric(difftime(end_time, start_time, units="mins"))))
cat("All outputs are available in the /results folder.\n")
=======
# =============================================================================
# MASTER RUNNER: Biodiversity and Energy System Planning QLD 2025
# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================
# Purpose: This script executes the full analysis pipeline in the correct order.
# =============================================================================

# 1. INITIAL SETUP
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, magick)

cat("\n🚀 Starting Full Analysis Pipeline...\n")
start_time <- Sys.time()

# 2. DEFINE PIPELINE STEPS
# We order these so that data processing happens before complex mapping
pipeline <- list(
  "Species Coverage" = here("species_code", "critically_endangered_coverage.R"),
  "Zonation Curves"  = here("Figure_code", "Zonation curves.R"),
  "Cost Analysis"    = here("Figure_code", "percent cost increase_line plot.R"),
  "NPV Analysis"     = here("Figure_code", "NPV_bar_plot.R"),
  "Spatial Mapping"  = here("Figure_code", "2050_domestic_CPA_comparison.R")
)

# 3. EXECUTION ENGINE
results_log <- data.frame(
  Step = names(pipeline),
  Status = "Pending",
  Time = NA,
  stringsAsFactors = FALSE
)

for (i in seq_along(pipeline)) {
  step_name <- names(pipeline)[i]
  step_path <- pipeline[[i]]
  
  cat(sprintf("\n--- Running Step %d/%d: %s ---\n", i, length(pipeline), step_name))
  
  if (file.exists(step_path)) {
    tryCatch({
      source(step_path)
      results_log$Status[i] <- "Success ✅"
    }, error = function(e) {
      results_log$Status[i] <- paste("Failed ❌:", e$message)
    })
  } else {
    results_log$Status[i] <- "File Not Found ❌"
  }
  results_log$Time[i] <- format(Sys.time(), "%H:%M:%S")
}

# 4. FINAL STATUS REPORT
cat("\n" , "="*40, "\n")
cat("FINAL PIPELINE REPORT\n")
cat("="*40, "\n")
print(results_log, row.names = FALSE)

end_time <- Sys.time()
cat(sprintf("\nTotal Runtime: %0.2f minutes\n", as.numeric(difftime(end_time, start_time, units="mins"))))
cat("All outputs are available in the /results folder.\n")
>>>>>>> f3251691bae3b7a35195d3961322c754fcc324f3
cat("="*40, "\n")