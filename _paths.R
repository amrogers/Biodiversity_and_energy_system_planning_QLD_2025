# =============================================================================
# Central Path Management
# =============================================================================
# All scripts source this file via: source(here::here("_paths.R"))
# Add new data paths here rather than hardcoding them in individual scripts.
#
# LOCAL OVERRIDES
# ---------------
# If your data lives somewhere other than the default BESP_data_qld_2025/
# location (e.g. a network drive), create _paths_local.R in the project root
# and override specific entries in the paths list there. For example:
#
#   paths$tx1_new_summary <- "Z:/my_drive/tx1/QLD_threshold_tx1_new_summary.csv"
#
# Every script automatically loads _paths_local.R (if it exists) immediately
# after sourcing this file. _paths_local.R is listed in .gitignore and must
# never be committed — it is machine-specific.
# =============================================================================
library(here)

# =============================================================================
# 1. Base Data Folder (Where the unzipped Figshare data lives)
#    Looks for a non-empty "data/" first, then the full Figshare folder name
# =============================================================================
if (dir.exists(here("data")) && length(list.files(here("data"))) > 0) {
  data_root <- here("data")
} else {
  data_root <- here("BESP_data_qld_2025")
}

# =============================================================================
# 2. Input Data Paths
# =============================================================================
paths <- list(

  # --- Energy system model outputs ---
  energy_outputs = file.path(data_root, "Energy_system_model_outputs"),

  # --- Zonation analysis ---
  zonation_raw = file.path(data_root, "Zonation_analysis"),
  zonation_out = file.path(data_root, "Zonation_analysis", "Zonation_output"),
  rankmap      = file.path(data_root, "Zonation_analysis", "Zonation_output",
                           "250m_QLD_2024", "out_example1", "rankmap.tif"),

  # --- Energy model GDBs (unzip from .gdb.zip before use) ---
  gdb_tx1 = file.path(data_root, "Energy_system_model_outputs",
                      "Energy_system_analysis_scenarios", "QLD_v202412_eplus_tx1.gdb"),
  gdb_tx2 = file.path(data_root, "Energy_system_model_outputs",
                      "Energy_system_analysis_scenarios", "QLD_v202412_eplus_tx2.gdb"),

  # --- Tx_outputs: pre-computed new-build-only transmission summary CSVs ---
  # (produced by QLD_new_tx_processing_summary.R; stored in Tx_outputs after
  #  removing segments overlapping the existing QLD network)
  tx_outputs      = file.path(data_root, "Energy_system_model_outputs",
                              "Electricity_Transmission_Lines", "Tx_outputs"),
  tx1_new_summary = file.path(data_root, "Energy_system_model_outputs",
                              "Electricity_Transmission_Lines", "Tx_outputs",
                              "tx1_domestic_transmission", "QLD_threshold_tx_new",
                              "QLD_threshold_tx1_new_summary.csv"),
  tx2_new_summary = file.path(data_root, "Energy_system_model_outputs",
                              "Electricity_Transmission_Lines", "Tx_outputs",
                              "tx2_domestic_transmission", "QLD_threshold_tx_new",
                              "QLD_threshold_tx2_new_summary.csv"),

  # --- Species / SNES spatial data ---
  snes_dir        = file.path(data_root, "QLD_100m_SNES_500spp"),
  snes_shapefiles = file.path(data_root, "QLD_100m_SNES_500spp", "shapefiles"),
  snes_attributes = file.path(data_root, "QLD_100m_SNES_500spp", "species_attributes.csv"),

  # --- Existing QLD transmission lines (reference spatial data) ---
  # Source: QLD electricity transmission lines dataset (Boundless / state government)
  existing_tx_csv  = file.path(data_root, "Energy_system_model_outputs",
                               "Electricity_Transmission_Lines",
                               "QLD_exisiting_tx_projected.csv"),
  existing_tx      = file.path(data_root, "Energy_system_model_outputs",
                               "Electricity_Transmission_Lines",
                               "Electricity_Transmission_Lines.shp"),
  existing_tx_buff = file.path(data_root, "Energy_system_model_outputs",
                               "Electricity_Transmission_Lines",
                               "Electricity_Transmission_Lines_1km_buff.shp"),
  existing_tx_simp = file.path(data_root, "Energy_system_model_outputs",
                               "Electricity_Transmission_Lines",
                               "QLD_existing_tx_simplified_buff_no_overlap2.shp")
)

# =============================================================================
# 3. Output Paths
# =============================================================================

# --- General results root ---
results_root <- here("results")
if (!dir.exists(results_root)) dir.create(results_root)

# --- Transmission processing intermediate outputs (large spatial files) ---
# These folders are created automatically by the transmission pipeline scripts.
# They are listed in .gitignore (under results/) and are not committed to the repo.
tx1_processing <- here("results", "transmission_processing", "tx1")
tx2_processing <- here("results", "transmission_processing", "tx2")
