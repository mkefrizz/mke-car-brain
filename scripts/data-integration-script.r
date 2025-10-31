# =============================================================================
# DATA INTEGRATION & PROCESSING SCRIPT
# =============================================================================
# Merges EPA data, scraped specs, and manual entries into master dataset
# =============================================================================

library(tidyverse)
library(glue)
library(readxl)
library(writexl)

# -----------------------------------------------------------------------------
# STEP 1: LOAD ALL DATA SOURCES
# -----------------------------------------------------------------------------

load_all_data_sources <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  LOADING DATA FROM ALL SOURCES\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  sources <- list()
  
  # 1. Manual data entry (primary source)
  cat("1. Loading manual data entry...\n")
  if (file.exists("data/vehicle_data_entry.xlsx")) {
    sources$manual <- read_xlsx("data/vehicle_data_entry.xlsx")
    cat(glue("   ✓ Loaded {nrow(sources$manual)} rows from manual entry\n"))
  } else if (file.exists("data/vehicle_data_entry.csv")) {
    sources$manual <- read_csv("data/vehicle_data_entry.csv", show_col_types = FALSE)
    cat(glue("   ✓ Loaded {nrow(sources$manual)} rows from manual entry\n"))
  } else {
    cat("   ⚠ No manual data entry file found\n")
    sources$manual <- NULL
  }
  
  # 2. EPA fuel economy data
  cat("\n2. Loading EPA fuel economy data...\n")
  if (file.exists("data/processed/epa_filtered.csv")) {
    sources$epa <- read_csv("data/processed/epa_filtered.csv", show_col_types = FALSE)
    cat(glue("   ✓ Loaded {nrow(sources$epa)} rows from EPA data\n"))
  } else {
    cat("   ⚠ No EPA data found (run download_epa_data() first)\n")
    sources$epa <- NULL
  }
  
  # 3. Scraped specifications
  cat("\n3. Loading scraped specifications...\n")
  if (file.exists("data/raw/scraped_specs.rds")) {
    sources$scraped_raw <- readRDS("data/raw/scraped_specs.rds")
    cat(glue("   ✓ Loaded scraped data for {length(sources$scraped_raw)} vehicle-years\n"))
  } else {
    cat("   ⚠ No scraped data found (run scrape_all_vehicles() first)\n")
    sources$scraped_raw <- NULL
  }
  
  cat("\n")
  return(sources)
}

# -----------------------------------------------------------------------------
# STEP 2: PROCESS SCRAPED DATA INTO STRUCTURED FORMAT
# -----------------------------------------------------------------------------

process_scraped_data <- function(scraped_raw) {
  cat("Processing scraped data into structured format...\n")
  
  if (is.null(scraped_raw) || length(scraped_raw) == 0) {
    cat("  ⚠ No scraped data to process\n\n")
    return(NULL)
  }
  
  # Convert scraped list into dataframe
  scraped_df <- map_df(scraped_raw, function(x) {
    
    # Initialize with basic info
    row <- tibble(
      brand = x$brand,
      model = x$model,
      year = x$year,
      scrape_source = x$source %||% NA_character_,
      scrape_success = x$success
    )
    
    # Try to extract dimensions if available
    if (!is.null(x$data) && !is.null(x$data$specs)) {
      specs <- x$data$specs
      
      row$length_in_scraped <- extract_dimension(specs$length, "in")
      row$width_in_scraped <- extract_dimension(specs$width, "in")
      row$height_in_scraped <- extract_dimension(specs$height, "in")
      row$curb_weight_lbs_scraped <- extract_dimension(specs$weight, "lbs")
    } else {
      row$length_in_scraped <- NA_real_
      row$width_in_scraped <- NA_real_
      row$height_in_scraped <- NA_real_
      row$curb_weight_lbs_scraped <- NA_real_
    }
    
    return(row)
  })
  
  cat(glue("  ✓ Processed {nrow(scraped_df)} scraped records\n"))
  cat(glue("  ✓ Successfully scraped: {sum(scraped_df$scrape_success)} records\n"))
  cat(glue("  ✓ With dimensions: {sum(!is.na(scraped_df$length_in_scraped))} records\n\n"))
  
  return(scraped_df)
}

# Helper function to extract dimensions (from scraper)
extract_dimension <- function(text, unit = "in") {
  if (is.null(text) || is.na(text) || text == "") return(NA)
  
  num <- str_extract(text, "\\d+\\.?\\d*")
  if (is.na(num)) return(NA)
  
  value <- as.numeric(num)
  
  # Convert units if needed
  if (grepl("mm", text, ignore.case = TRUE) && unit == "in") {
    value <- value / 25.4
  } else if (grepl("kg", text, ignore.case = TRUE) && unit == "lbs") {
    value <- value * 2.20462
  }
  
  return(value)
}

# -----------------------------------------------------------------------------
# STEP 3: MERGE ALL DATA SOURCES
# -----------------------------------------------------------------------------

merge_all_sources <- function(sources) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  MERGING ALL DATA SOURCES\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Start with manual entry as base (has all vehicle-year combinations)
  if (is.null(sources$manual)) {
    stop("No manual data entry file found. Cannot proceed without base template.")
  }
  
  master <- sources$manual
  cat(glue("Starting with {nrow(master)} rows from manual entry\n\n"))
  
  # Add EPA fuel economy data
  if (!is.null(sources$epa)) {
    cat("Merging EPA fuel economy data...\n")
    
    # Clean EPA data for merging
    epa_clean <- sources$epa %>%
      select(year, make, model, 
             fuel_econ_city_epa = fuel_econ_city,
             fuel_econ_hwy_epa = fuel_econ_hwy,
             fuel_econ_combined_epa = fuel_econ_combined) %>%
      group_by(year, make, model) %>%
      summarise(across(starts_with("fuel_econ"), ~mean(., na.rm = TRUE)), 
                .groups = "drop")
    
    # Merge
    master <- master %>%
      left_join(epa_clean, 
                by = c("year" = "year", "brand" = "make", "model" = "model"))
    
    # Fill in manual entry gaps with EPA data
    master <- master %>%
      mutate(
        fuel_econ_city = coalesce(fuel_econ_city, fuel_econ_city_epa),
        fuel_econ_hwy = coalesce(fuel_econ_hwy, fuel_econ_hwy_epa),
        fuel_econ_combined = coalesce(fuel_econ_combined, fuel_econ_combined_epa)
      ) %>%
      select(-fuel_econ_city_epa, -fuel_econ_hwy_epa, -fuel_econ_combined_epa)
    
    epa_added <- sum(!is.na(master$fuel_econ_combined))
    cat(glue("  ✓ Added/updated fuel economy for {epa_added} records\n\n"))
  }
  
  # Add scraped specifications
  if (!is.null(sources$scraped)) {
    cat("Merging scraped specifications...\n")
    
    master <- master %>%
      left_join(sources$scraped,
                by = c("brand" = "brand", "model" = "model", "year" = "year"))
    
    # Fill in manual entry gaps with scraped data
    master <- master %>%
      mutate(
        length_in = coalesce(length_in, length_in_scraped),
        width_in = coalesce(width_in, width_in_scraped),
        height_in = coalesce(height_in, height_in_scraped),
        curb_weight_lbs = coalesce(curb_weight_lbs, curb_weight_lbs_scraped),
        # Update data source if scraped data was used
        data_source = case_when(
          !is.na(length_in_scraped) & is.na(data_source) ~ scrape_source,
          TRUE ~ data_source
        )
      ) %>%
      select(-ends_with("_scraped"), -scrape_source, -scrape_success)
    
    scraped_added <- sum(!is.na(master$length_in))
    cat(glue("  ✓ Added/updated dimensions for {scraped_added} records\n\n"))
  }
  
  cat("✓ Merge complete!\n\n")
  return(master)
}

# -----------------------------------------------------------------------------
# STEP 4: DATA QUALITY ASSESSMENT
# -----------------------------------------------------------------------------

assess_data_quality <- function(master) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  DATA QUALITY ASSESSMENT\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  total_records <- nrow(master)
  
  # Completeness by field
  completeness <- tibble(
    field = c("Length", "Width", "Height", "Weight", "Wheelbase", 
              "Fuel Econ City", "Fuel Econ Hwy", "Sales"),
    column = c("length_in", "width_in", "height_in", "curb_weight_lbs",
               "wheelbase_in", "fuel_econ_city", "fuel_econ_hwy", "annual_sales"),
    complete = map_int(column, ~sum(!is.na(master[[.x]]))),
    missing = map_int(column, ~sum(is.na(master[[.x]]))),
    pct_complete = round((complete / total_records) * 100, 1)
  ) %>%
    select(-column) %>%
    arrange(desc(pct_complete))
  
  cat("COMPLETENESS BY FIELD:\n")
  print(completeness, n = Inf)
  cat("\n")
  
  # Completeness by vehicle
  vehicle_completeness <- master %>%
    group_by(brand, model, type) %>%
    summarise(
      total_years = n(),
      length_complete = sum(!is.na(length_in)),
      width_complete = sum(!is.na(width_in)),
      weight_complete = sum(!is.na(curb_weight_lbs)),
      fuel_complete = sum(!is.na(fuel_econ_combined)),
      pct_complete = round((length_complete + width_complete + weight_complete) / 
                           (total_years * 3) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_complete))
  
  cat("COMPLETENESS BY VEHICLE:\n")
  print(vehicle_completeness, n = Inf)
  cat("\n")
  
  # Data sources breakdown
  source_summary <- master %>%
    filter(!is.na(data_source)) %>%
    count(data_source, name = "records") %>%
    arrange(desc(records))
  
  if (nrow(source_summary) > 0) {
    cat("DATA SOURCES:\n")
    print(source_summary, n = Inf)
    cat("\n")
  }
  
  # Overall summary
  key_fields_complete <- master %>%
    filter(!is.na(length_in), !is.na(width_in), !is.na(curb_weight_lbs))
  
  cat("OVERALL SUMMARY:\n")
  cat(glue("  Total records: {total_records}\n"))
  cat(glue("  Complete records (L/W/Wt): {nrow(key_fields_complete)} ({round(nrow(key_fields_complete)/total_records*100, 1)}%)\n"))
  cat(glue("  Ready for analysis: {nrow(key_fields_complete) >= 30}\n\n"))
  
  quality <- list(
    completeness = completeness,
    vehicle_completeness = vehicle_completeness,
    source_summary = source_summary,
    total_complete = nrow(key_fields_complete)
  )
  
  return(quality)
}

# -----------------------------------------------------------------------------
# STEP 5: SAVE INTEGRATED DATASET
# -----------------------------------------------------------------------------

save_integrated_data <- function(master, quality) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  SAVING INTEGRATED DATASET\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Save master dataset in multiple formats
  write_csv(master, "data/processed/vehicle_data_integrated.csv")
  cat("✓ Saved: data/processed/vehicle_data_integrated.csv\n")
  
  write_tsv(master, "data/processed/vehicle_data_integrated.tsv")
  cat("✓ Saved: data/processed/vehicle_data_integrated.tsv\n")
  
  write_xlsx(master, "data/processed/vehicle_data_integrated.xlsx")
  cat("✓ Saved: data/processed/vehicle_data_integrated.xlsx\n")
  
  saveRDS(master, "data/processed/vehicle_data_integrated.rds")
  cat("✓ Saved: data/processed/vehicle_data_integrated.rds\n\n")
  
  # Save quality assessment
  write_csv(quality$completeness, "output/data_quality_completeness.csv")
  cat("✓ Saved: output/data_quality_completeness.csv\n")
  
  write_csv(quality$vehicle_completeness, "output/data_quality_by_vehicle.csv")
  cat("✓ Saved: output/data_quality_by_vehicle.csv\n")
  
  if (nrow(quality$source_summary) > 0) {
    write_csv(quality$source_summary, "output/data_quality_sources.csv")
    cat("✓ Saved: output/data_quality_sources.csv\n")
  }
  
  cat("\n")
}

# -----------------------------------------------------------------------------
# MASTER INTEGRATION FUNCTION
# -----------------------------------------------------------------------------

integrate_all_data <- function() {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║           DATA INTEGRATION & PROCESSING PIPELINE              ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  
  # Step 1: Load all sources
  sources <- load_all_data_sources()
  
  # Step 2: Process scraped data
  if (!is.null(sources$scraped_raw)) {
    sources$scraped <- process_scraped_data(sources$scraped_raw)
  }
  
  # Step 3: Merge everything
  master <- merge_all_sources(sources)
  
  # Step 4: Assess quality
  quality <- assess_data_quality(master)
  
  # Step 5: Save
  save_integrated_data(master, quality)
  
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║                  INTEGRATION COMPLETE!                        ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  cat("NEXT STEPS:\n")
  cat("  1. Review: data/processed/vehicle_data_integrated.csv\n")
  cat("  2. Check quality: output/data_quality_*.csv\n")
  cat("  3. Fill remaining gaps in manual entry file\n")
  cat("  4. Re-run integration after updates\n")
  cat("  5. When ready: run_analysis_pipeline()\n\n")
  
  return(list(
    master = master,
    quality = quality,
    sources = sources
  ))
}

# -----------------------------------------------------------------------------
# CONVENIENCE FUNCTIONS
# -----------------------------------------------------------------------------

# Quick reload of integrated data
load_integrated_data <- function() {
  if (file.exists("data/processed/vehicle_data_integrated.rds")) {
    readRDS("data/processed/vehicle_data_integrated.rds")
  } else {
    stop("No integrated data found. Run integrate_all_data() first.")
  }
}

# Show data gaps (what still needs manual entry)
show_data_gaps <- function() {
  master <- load_integrated_data()
  
  gaps <- master %>%
    filter(is.na(length_in) | is.na(width_in) | is.na(curb_weight_lbs)) %>%
    select(brand, model, year, type, length_in, width_in, curb_weight_lbs, 
           data_source, notes) %>%
    arrange(brand, model, year)
  
  cat(glue("\nFound {nrow(gaps)} records with missing key dimensions:\n\n"))
  print(gaps, n = Inf)
  
  return(gaps)
}

# Update master with new manual entries
update_integrated_data <- function() {
  cat("Re-integrating data with latest manual entries...\n\n")
  integrate_all_data()
}

# -----------------------------------------------------------------------------
# STARTUP MESSAGE
# -----------------------------------------------------------------------------

cat("\n📊 Data Integration Script loaded!\n\n")
cat("MAIN FUNCTION:\n")
cat("  result <- integrate_all_data()\n")
cat("  # Merges EPA + scraped + manual data into master dataset\n\n")
cat("HELPER FUNCTIONS:\n")
cat("  master <- load_integrated_data()    # Load integrated data\n")
cat("  gaps <- show_data_gaps()            # See what's missing\n")
cat("  update_integrated_data()            # Re-run after adding data\n\n")
cat("WORKFLOW:\n")
cat("  1. Collect some data (manual, EPA, scraping)\n")
cat("  2. Run: integrate_all_data()\n")
cat("  3. Check quality reports in output/\n")
cat("  4. Fill gaps in vehicle_data_entry file\n")
cat("  5. Re-run: update_integrated_data()\n")
cat("  6. When satisfied: run_analysis_pipeline()\n\n")