library(readr)
library(stringr)
library(httr2)
library(dplyr)

# Identify urls scraped from NYC Dept of Finance page
urls <- read.csv("final_project/data/tax_class_urls.csv", header = TRUE)[, 1]

# Cycle through all urls to download
for (i in 1:length(urls)) {
  url <- urls[i]

  temp_file <- tempfile(fileext = ".zip")
  temp_dir <- "final_project/data/temp"

  response <- request(url) |>
    req_perform()

  writeBin(resp_body_raw(response), temp_file)

  unzip(temp_file, exdir = temp_dir)

  # Add a sleep to avoid hitting rate limits
  Sys.sleep(1)
}


PARENT_DIR <- "final_project/data/temp"

for (i in 9:23) {
  dir_name <- paste(PARENT_DIR, i, sep = "/")
  dir.create(dir_name, showWarnings = FALSE)
  print(paste("Created directory:", dir_name))
}

FILES <- list.files("final_project/data/temp", pattern = "\\.zip$", full.names = TRUE)

for (i in 1:length(FILES)) {
  number <- sub(".*_(\\d+)\\.zip$", "\\1", FILES[i])

  # Extract the number from the file name
  number <- sub(".*_(\\d+)\\.zip$", "\\1", FILES[i])

  # New Directory
  dir_name <- paste0("final_project/data/temp/", number)

  # New file path
  new_file_path <- file.path(dir_name, basename(FILES[i]))

  # Move the file to the corresponding directory
  file.rename(FILES[i], new_file_path)
}


BASE_DIR <- "final_project/data/temp"

# List subdirectories in the base directory
folders <- list.dirs(BASE_DIR, full.names = TRUE, recursive = FALSE)

# Loop through each folder
for (folder in folders) {
  # Skip if the folder name is not numeric (i.e., avoid the 'extracted' folder)
  folder_name <- basename(folder)
  if (!grepl("^\\d+$", folder_name)) {
    next
  }

  # List .zip files in the folder
  zip_files <- list.files(folder, pattern = "\\.zip$", full.names = TRUE)

  # Extract and rename files
  for (zip_file in zip_files) {
    # Create a subfolder named "extracted" within the current folder
    extracted_dir <- file.path(folder, "extracted")
    if (!dir.exists(extracted_dir)) {
      dir.create(extracted_dir)
    }

    # Unzip the file into the 'extracted' directory within the current folder
    unzip(zip_file, exdir = extracted_dir)

    # List all files extracted in the 'extracted' folder
    extracted_files <- list.files(extracted_dir, full.names = TRUE)

    # Rename each extracted file by adding the folder name to the file
    for (extracted_file in extracted_files) {
      new_name <- paste0(folder_name, "_", basename(extracted_file))
      new_path <- file.path(extracted_dir, new_name)

      file.rename(extracted_file, new_path)
    }
  }
}

# Define the base directory
BASE_DIR <- "final_project/data/temp"

# List all subdirectories (recursive) and find those named 'extracted'
extracted_folders <- list.dirs(BASE_DIR, full.names = TRUE, recursive = TRUE)
extracted_folders <- extracted_folders[grepl("extracted$", extracted_folders)]

# Loop through each 'extracted' folder
for (folder in extracted_folders) {
  # List all files in the 'extracted' folder
  extracted_files <- list.files(folder, full.names = TRUE)

  # Move each file to the 'temp' directory
  for (file in extracted_files) {
    # Define the new location in the 'temp' directory
    new_location <- file.path(BASE_DIR, basename(file))

    # Move the file
    file.rename(file, new_location)
  }

  # Remove the now-empty 'extracted' folder
  unlink(folder, recursive = TRUE)
}

# List all items in the directory
all_items <- list.files(BASE_DIR, full.names = TRUE)

# Filter for directories
folders <- all_items[file.info(all_items)$isdir]

# List all .mdb and .txt files
files_to_keep <- list.files(BASE_DIR, pattern = "\\.(mdb|txt)$", full.names = TRUE)

# Loop through each folder and delete it if it's not a .mdb or .txt file
for (folder in folders) {
  if (!(folder %in% files_to_keep)) {
    unlink(folder, recursive = TRUE)
  }
}

# All the mdb files were converted to .csv files i.e :
# MacBook-Pro temp % mdb-tables 16_16_tc1.mdb
# NameTable tc1
# MacBook-Pro temp % mdb-export 16_16_tc1.mdb tc1 > 16_TC1.csv

# Remove all .mdb files
mdb_dir <- "final_project/data/temp/mdb"
dir.create(mdb_dir)

mdb_files <- list.files("final_project/data/temp", pattern = "\\.mdb$", full.names = TRUE)

# Move each .mdb file to the 'mdb' directory
for (file in mdb_files) {
  file.rename(file, file.path(mdb_dir, basename(file)))
}

finance_dir <- "final_project/data/finance"
dir.create(finance_dir)

csv_files <- list.files("final_project/data/temp", pattern = "\\.csv$", full.names = TRUE)

# Create base information for properties narrowed by residential, Brooklyn properties from 2010 - 2019
base_property_info_1 <- read_csv(csv_files[1]) |>
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3) |>
  mutate(LOT = trimws(LOT)) |>
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  filter(ZIP %in% c(11215, 11218)) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI)

base_property_info_2 <- read_csv(csv_files[2]) |>
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3, TXCL %in% c("2", "2A", "2B", "2C")) |>
  mutate(LOT = trimws(LOT)) |>
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  filter(ZIP %in% c(11215, 11218)) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI)

base_joined <- bind_rows(base_property_info_1, base_property_info_2)

reference_base_2010_2019 <- base_joined

# Merge the 2 classes of commercial properties
for (i in seq(1, length(csv_files), by = 2)) {
  number <- sub(".*\\/([0-9]+)_.+", "\\1", csv_files[i])
  col_name <- paste("VALUE", number, sep = "_")
  first <- read.csv(csv_files[i])
  second <- read.csv(csv_files[i + 1])
  first_t <- first |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3) |>
    mutate(LOT = trimws(LOT)) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    filter(ZIP %in% c(11215, 11218)) |>
    select(ID, !!sym(col_name))

  second_t <- second |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3, TXCL %in% c("2", "2A", "2B", "2C")) |>
    mutate(LOT = trimws(LOT)) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    filter(ZIP %in% c(11215, 11218)) |>
    select(ID, !!sym(col_name))

  joined <- bind_rows(second_t, first_t)

  reference_base_2010_2019 <- reference_base_2010_2019 |>
    left_join(joined, by = "ID")
}

reference_base_copy <- reference_base_2010_2019

# Open all the .txt files for 2020 - 2023
txt_files <- list.files("final_project/data/temp", pattern = "\\.(txt|TXT)$", full.names = TRUE)

# Process all .txt files from 2020 - 2023 then merge with the base_copy file of 2010 - 2019 values
for (i in seq(1, length(csv_files), by = 2)) {
  # change to i
  number <- sub(".*/(\\d+)_.*", "\\1", txt_files[i])

  col_name <- paste("VALUE", number, sep = "_")

  # change to i
  first <- readLines(txt_files[i])
  first_records <- strsplit(first, "\n")
  first_parsed <- lapply(first_records, function(record) strsplit(record, "\t"))
  first_rows <- do.call(rbind, lapply(first_parsed, function(x) unlist(x)))
  first_df <- as.data.frame(first_rows)

  # change to i
  second <- readLines(txt_files[i + 1])
  second_records <- strsplit(second, "\n")
  second_parsed <- lapply(second_records, function(record) strsplit(record, "\t"))
  second_rows <- do.call(rbind, lapply(second_parsed, function(x) unlist(x)))
  second_df <- as.data.frame(second_rows)

  nrow(first_df)
  nrow(second_df)

  first_t <- first_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1", "1A", "1B", "1C", "1D", "2", "2A", "2B", "2C")) |>
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25) |>
    select(ID, !!sym(col_name))

  second_t <- second_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1", "1A", "1B", "1C", "1D", "2", "2A", "2B", "2C")) |>
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25) |>
    select(ID, !!sym(col_name))

  joined_2020_2023 <- bind_rows(second_t, first_t)

  reference_base_copy <- reference_base_copy |>
    inner_join(joined_2020_2023, by = "ID")
}

# No understanding why, but this code is creating duplicates of ID "874-68"
reference_base_copy_t <- reference_base_copy |>
  filter(ID != "874-68")

# Save data to a .csv
write.csv(x = reference_base_copy_t, "final_project/data/finance/2010_2023_park_slope_values.csv")
