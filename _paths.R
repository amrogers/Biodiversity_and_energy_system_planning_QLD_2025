# Central Path Management
library(here)

# 1. Base Data Folder (Where the unzipped Figshare data lives)
data_root <- here("data")

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