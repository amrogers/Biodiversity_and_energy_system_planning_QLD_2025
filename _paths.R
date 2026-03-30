# =============================================================================
# Central Path Management
# =============================================================================
# All scripts source this file via: source(here::here("_paths.R"))
# Add new data paths here rather than hardcoding them in individual scripts.
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
  gdb_tx1 = file.path(data_root, "QLD_v202412_eplus_tx1.gdb"),
  gdb_tx2 = file.path(data_root, "QLD_v202412_eplus_tx2.gdb"),

  # --- Existing QLD transmission lines (reference spatial data) ---
  # Place the following shapefiles in BESP_data_qld_2025/Spatial_reference_data/
  # Source: QLD electricity transmission lines dataset (Boundless / state government)
  existing_tx      = file.path(data_root, "Spatial_reference_data",
                               "Electricity_Transmission_Lines.shp"),
  existing_tx_buff = file.path(data_root, "Spatial_reference_data",
                               "Electricity_Transmission_Lines_1km_buff.shp"),
  existing_tx_simp = file.path(data_root, "Spatial_reference_data",
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
