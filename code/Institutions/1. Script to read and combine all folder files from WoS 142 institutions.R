# =========================================
# Combine WoS .txt by USA State folders
# =========================================
# ---- Configure your root folders ----
# Root 1: folders like "State_Code_University"
root_dir <- "C:/Users/OMEN/Box/Andrea_Universities/Project_InternetEffect/InternetWBD/Global USAagg/Final version Nature Paper/WoS data/USA States"
# Root 2: extra states/universities (same naming convention)
root_dir_2 <- "C:/Users/OMEN/Box/Andrea_Universities/Project_InternetEffect/InternetWBD/Global USAagg/Final version Nature Paper/WoS data/USA States Texas Wyoming"

# List immediate subfolders (each should be State_Code_University)
folders   <- list.dirs(root_dir,  full.names = TRUE, recursive = FALSE)
folders_2 <- list.dirs(root_dir_2, full.names = TRUE, recursive = FALSE)

# ---- Utilities ----
source("Scripts/utilities.R") #Libraries and functions

# ---- Read all folders (Part 1) ----
initial_time=Sys.time()

combined_wos <- read_all_folders(folders)
message(sprintf("✅ Part 1 loaded. Rows: %s | Cols: %s",
                nrow(combined_wos), ncol(combined_wos)))

# Rename StateCode → University_Code for clarity/consistency
if ("StateCode" %in% names(combined_wos)) {
  combined_wos <- combined_wos %>% dplyr::rename(University_Code = StateCode)
}

# ---- Read all folders (Part 2) ----
combined_wos_part2 <- read_all_folders(folders_2[10])
message(sprintf("✅ Part 2 loaded. Rows: %s | Cols: %s",
                nrow(combined_wos_part2), ncol(combined_wos_part2)))

if ("StateCode" %in% names(combined_wos_part2)) {
  combined_wos_part2 <- combined_wos_part2 %>% dplyr::rename(University_Code = StateCode)
}

# ---- Select common fields and enforce consistent types before binding ----
# Fields we want to export
cols_keep <- c("State", "University", "University_Code", "DI", "EM", "AU", "PY")

combined_wos      <- coerce_export_types(combined_wos)
combined_wos_part2<- coerce_export_types(combined_wos_part2)

# ---- Combine both parts ----
combined_wos_complete <- dplyr::bind_rows(combined_wos, combined_wos_part2)

message(sprintf("✅ Combined (Part 1 + Part 2). Rows: %s | Cols: %s",
                nrow(combined_wos_complete), ncol(combined_wos_complete)))

end_time=Sys.time()
# ---- Write outputs ----
# Full combined CSV
readr::write_csv(
  combined_wos_complete,
  file.path("C:/Users/apmon/Box/Andrea_Universities/Project_InternetEffect/InternetWBD/Global USAagg/Final version Nature Paper/Outputs",
            "WoS_USA_States_combined complete 10_20_2025.csv")
)

# Unique list for distances: (State, Code, University)
combined_wos_complete %>%
  dplyr::select(State, University_Code, University) %>%
  dplyr::distinct() %>%
  readr::write_csv(
    file.path("C:/Users/apmon/Box/Andrea_Universities/Project_InternetEffect/InternetWBD/Global USAagg/Final version Nature Paper/Outputs",
              "List of universities with sciscinet code and state 10_20_2025.csv")
  )

# If you insist on dropping row names (mostly for data.frame; tibbles ignore them):
if (!is.null(rownames(combined_wos_complete))) rownames(combined_wos_complete) <- NULL



