# Central Path Management
library(here)

# 1. Base Data Folder (Where the unzipped Figshare data lives)
# Looks for a non-empty "data/" first, then the full Figshare folder name
if (dir.exists(here("data")) && length(list.files(here("data"))) > 0) {
  data_root <- here("data")
} else {
  data_root <- here("supplementary data_biodiversity and energy system planning_qld_2025")
}

# 2. Specific Data Anchors (Based on your folder map)
paths <- list(
  energy_outputs = file.path(data_root, "Energy_system_model_outputs"),
  zonation_raw   = file.path(data_root, "Zonation_analysis"),
  zonation_out   = file.path(data_root, "Zonation_analysis", "Zonation_output"),
  
  # The GDBs (Note: These must be unzipped to be read by sf)
  gdb_tx1 = file.path(data_root, "QLD_v202412_eplus_tx1.gdb"),
  gdb_tx2 = file.path(data_root, "QLD_v202412_eplus_tx2.gdb")
)

# 3. Output Folder (Where your R scripts will save new results)
results_root <- here("results")
if (!dir.exists(results_root)) dir.create(results_root)