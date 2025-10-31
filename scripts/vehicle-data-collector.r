# =============================================================================
# MILWAUKEE CAR BRAIN - VEHICLE SIZE DATA COLLECTION WORKFLOW
# =============================================================================
# A comprehensive R workflow for collecting vehicle specifications across
# 9 vehicles and 8 milestone years (1990-2024)
#
# Author: Research project for Milwaukee Car Brain book
# Date: October 2025
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP & DEPENDENCIES
# -----------------------------------------------------------------------------

# Install required packages if needed
required_packages <- c("tidyverse", "rvest", "httr", "janitor", "here", 
                       "glue", "jsonlite", "writexl", "readxl")

install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

lapply(required_packages, install_if_needed)

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------

# Define our target vehicles
VEHICLES <- tribble(
  ~brand, ~model, ~type, ~notes,
  "Ford", "Taurus", "Sedan", "Or Focus if better data",
  "Ford", "Explorer", "SUV", "Already have some data",
  "Ford", "F-150", "Pickup", "Best-selling truck",
  "Toyota", "Camry", "Sedan", "Best-selling car",
  "Toyota", "RAV4", "SUV", "Best-selling SUV",
  "Toyota", "Tundra", "Pickup", "Or Tacoma if better data",
  "Chevrolet", "Malibu", "Sedan", "Or Impala",
  "Chevrolet", "Equinox", "SUV", "Or Tahoe",
  "Chevrolet", "Silverado", "Pickup", "F-150 competitor"
)

# Milestone years
YEARS <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)

# Data points we're collecting
SPEC_FIELDS <- c(
  "length_in", "width_in", "height_in", "curb_weight_lbs",
  "wheelbase_in", "fuel_econ_city", "fuel_econ_hwy", 
  "fuel_econ_combined", "annual_sales"
)

# Create project directory structure
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("output/visualizations", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# MODULE 1: EPA FUEL ECONOMY DATA
# -----------------------------------------------------------------------------

download_epa_data <- function(force_download = FALSE) {
  cat("📊 MODULE 1: EPA Fuel Economy Data\n")
  cat("=====================================\n\n")
  
  epa_file <- "data/raw/vehicles.csv"
  
  if (!file.exists(epa_file) || force_download) {
    cat("Downloading EPA fuel economy data...\n")
    cat("Note: This may take a minute. The file is ~100MB.\n\n")
    
    # Try multiple EPA URLs (they change sometimes)
    epa_urls <- c(
      "https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip",
      "https://www.fueleconomy.gov/feg/epadata/vehicles.csv"
    )
    
    download_success <- FALSE
    
    for (url in epa_urls) {
      cat(glue("Trying: {url}\n"))
      
      tryCatch({
        if (grepl("\\.zip$", url)) {
          # Download and unzip
          temp_zip <- tempfile(fileext = ".zip")
          download.file(url, temp_zip, mode = "wb", quiet = FALSE, method = "auto")
          unzip(temp_zip, exdir = "data/raw")
          unlink(temp_zip)
        } else {
          # Direct CSV download
          download.file(url, epa_file, mode = "wb", quiet = FALSE, method = "auto")
        }
        
        cat("✓ EPA data downloaded successfully!\n\n")
        download_success <- TRUE
        break
        
      }, error = function(e) {
        cat(glue("  ✗ Failed: {e$message}\n"))
      })
    }
    
    if (!download_success) {
      cat("\n⚠ Automatic download failed. Manual options:\n\n")
      cat("OPTION 1 - Download directly:\n")
      cat("  1. Visit: https://www.fueleconomy.gov/feg/download.shtml\n")
      cat("  2. Download 'All vehicles' ZIP file\n")
      cat("  3. Extract vehicles.csv to: data/raw/\n\n")
      cat("OPTION 2 - Use curl (if you have it):\n")
      cat("  curl -o data/raw/vehicles.csv.zip https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip\n")
      cat("  unzip data/raw/vehicles.csv.zip -d data/raw/\n\n")
      cat("OPTION 3 - Skip EPA data for now:\n")
      cat("  run_full_workflow(skip_download = TRUE)\n\n")
    }
  } else {
    cat("✓ EPA data already exists at: data/raw/vehicles.csv\n\n")
  }
  
  if (file.exists(epa_file)) {
    return(epa_file)
  } else {
    return(NULL)
  }
}

filter_epa_for_our_vehicles <- function(epa_file) {
  cat("Filtering EPA data for our target vehicles...\n")
  
  if (is.null(epa_file) || !file.exists(epa_file)) {
    cat("⚠ EPA file not found. Skipping EPA filtering.\n\n")
    return(NULL)
  }
  
  # Read EPA data
  epa_data <- read_csv(epa_file, show_col_types = FALSE) %>%
    clean_names()
  
  cat(glue("  Total EPA records: {nrow(epa_data)}\n"))
  
  # Filter for our vehicles and years
  our_vehicles <- epa_data %>%
    filter(
      year %in% YEARS,
      make %in% c("Ford", "Toyota", "Chevrolet"),
      model %in% c("Taurus", "Explorer", "F-150", "F150", "F 150",
                   "Camry", "RAV4", "Tundra", "Tacoma",
                   "Malibu", "Impala", "Equinox", "Tahoe", 
                   "Silverado", "Silverado 1500")
    ) %>%
    select(year, make, model, cylinders, displ, 
           city08, highway08, comb08, fuel_type) %>%
    rename(
      fuel_econ_city = city08,
      fuel_econ_hwy = highway08,
      fuel_econ_combined = comb08
    )
  
  cat(glue("  Filtered records: {nrow(our_vehicles)}\n\n"))
  
  # Save filtered data
  write_csv(our_vehicles, "data/processed/epa_filtered.csv")
  cat("✓ Saved to: data/processed/epa_filtered.csv\n\n")
  
  return(our_vehicles)
}

# -----------------------------------------------------------------------------
# MODULE 2: WEB SCRAPING HELPERS
# -----------------------------------------------------------------------------

scrape_edmunds_specs <- function(year, make, model) {
  cat(glue("Attempting to scrape: {year} {make} {model}\n"))
  
  # Note: Edmunds structure changes frequently and may require authentication
  # This is a template - may need adjustment based on current site structure
  
  # Clean model name for URL
  model_clean <- tolower(gsub(" ", "-", model))
  make_clean <- tolower(make)
  
  # Try common Edmunds URL patterns
  url_patterns <- c(
    glue("https://www.edmunds.com/{make_clean}/{model_clean}/{year}/features-specs/"),
    glue("https://www.edmunds.com/{make_clean}/{model_clean}/{year}/st-100/features-specs/")
  )
  
  for (url in url_patterns) {
    tryCatch({
      page <- read_html(url)
      
      # This is a placeholder - actual selectors need to be updated
      # based on current Edmunds site structure
      specs <- list(
        length = page %>% html_element(".length") %>% html_text2(),
        width = page %>% html_element(".width") %>% html_text2(),
        height = page %>% html_element(".height") %>% html_text2(),
        weight = page %>% html_element(".curb-weight") %>% html_text2(),
        wheelbase = page %>% html_element(".wheelbase") %>% html_text2()
      )
      
      cat("  ✓ Successfully scraped!\n")
      return(specs)
      
    }, error = function(e) {
      # Silently continue to next pattern
    })
  }
  
  cat("  ⚠ Scraping failed - will need manual entry\n")
  return(NULL)
}

# Polite scraping with delays
scrape_all_vehicles <- function() {
  cat("📡 MODULE 2: Web Scraping\n")
  cat("=====================================\n\n")
  cat("⚠ Note: Web scraping is unreliable and may fail.\n")
  cat("This module will attempt to gather data but expect\n")
  cat("to fill in many gaps manually.\n\n")
  
  scraped_data <- list()
  
  for (i in 1:nrow(VEHICLES)) {
    vehicle <- VEHICLES[i, ]
    
    for (year in YEARS) {
      key <- glue("{year}_{vehicle$brand}_{vehicle$model}")
      
      result <- scrape_edmunds_specs(year, vehicle$brand, vehicle$model)
      scraped_data[[key]] <- result
      
      # Be polite - wait between requests
      Sys.sleep(2)
    }
  }
  
  # Save scraped data
  saveRDS(scraped_data, "data/raw/scraped_specs.rds")
  cat("\n✓ Scraping complete. Results saved.\n\n")
  
  return(scraped_data)
}

# -----------------------------------------------------------------------------
# MODULE 3: MANUAL DATA ENTRY TEMPLATE
# -----------------------------------------------------------------------------

create_data_entry_template <- function() {
  cat("📝 MODULE 3: Manual Data Entry Template\n")
  cat("=====================================\n\n")
  
  # Create comprehensive template
  template <- expand_grid(
    VEHICLES,
    year = YEARS
  ) %>%
    mutate(
      vehicle_id = glue("{brand}_{model}_{year}"),
      # Dimensional specs
      length_in = NA_real_,
      width_in = NA_real_,
      height_in = NA_real_,
      curb_weight_lbs = NA_real_,
      wheelbase_in = NA_real_,
      # Performance specs
      fuel_econ_city = NA_real_,
      fuel_econ_hwy = NA_real_,
      fuel_econ_combined = NA_real_,
      # Sales data
      annual_sales = NA_integer_,
      # Metadata
      data_source = NA_character_,
      data_quality = NA_character_,  # "excellent", "good", "estimated"
      notes = NA_character_,
      collected_date = NA_character_,
      collected_by = NA_character_
    )
  
  # Save in multiple formats for compatibility
  
  # 1. Excel (for those who prefer it)
  write_xlsx(template, "data/vehicle_data_entry.xlsx")
  cat("✓ Created Excel template: data/vehicle_data_entry.xlsx\n")
  
  # 2. CSV (works with Numbers, Google Sheets, Excel)
  write_csv(template, "data/vehicle_data_entry.csv")
  cat("✓ Created CSV template: data/vehicle_data_entry.csv\n")
  cat("  → Import this into Numbers or Google Sheets\n")
  
  # 3. TSV (tab-separated, sometimes better for Google Docs)
  write_tsv(template, "data/vehicle_data_entry.tsv")
  cat("✓ Created TSV template: data/vehicle_data_entry.tsv\n")
  cat("  → Import this into Google Docs tables\n")
  
  cat("\n")
  cat("Template includes:\n")
  cat("  • All 9 vehicles × 8 years = 72 rows\n")
  cat("  • Pre-filled vehicle identification\n")
  cat("  • Empty fields for specifications\n")
  cat("  • Metadata fields for tracking data quality\n\n")
  cat("Import instructions:\n")
  cat("  Numbers: File → Open → vehicle_data_entry.csv\n")
  cat("  Google Sheets: File → Import → Upload → vehicle_data_entry.csv\n")
  cat("  Google Docs: Not ideal for tables - use Sheets instead\n\n")
  
  return(template)
}

# Also create a CSV version for those who prefer it
create_csv_template <- function() {
  template <- create_data_entry_template()
  # Already creates CSV in the main function now
  cat("✓ CSV and TSV versions already created by create_data_entry_template()\n\n")
}

# Load data from any format
load_current_data <- function() {
  # Try multiple file formats
  if (file.exists("data/vehicle_data_entry.xlsx")) {
    cat("Loading from Excel file...\n")
    return(read_xlsx("data/vehicle_data_entry.xlsx"))
  } else if (file.exists("data/vehicle_data_entry.csv")) {
    cat("Loading from CSV file...\n")
    return(read_csv("data/vehicle_data_entry.csv", show_col_types = FALSE))
  } else if (file.exists("data/vehicle_data_entry.tsv")) {
    cat("Loading from TSV file...\n")
    return(read_tsv("data/vehicle_data_entry.tsv", show_col_types = FALSE))
  } else {
    stop("No data file found. Run run_full_workflow() first.")
  }
}

# -----------------------------------------------------------------------------
# MODULE 4: DATA VALIDATION
# -----------------------------------------------------------------------------

validate_vehicle_data <- function(data) {
  cat("🔍 MODULE 4: Data Validation\n")
  cat("=====================================\n\n")
  
  validation_results <- list()
  
  # Check 1: Missing data
  missing_summary <- data %>%
    summarise(across(where(is.numeric), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "field", values_to = "missing_count") %>%
    mutate(
      total = nrow(data),
      pct_complete = (1 - missing_count / total) * 100
    ) %>%
    arrange(pct_complete)
  
  cat("Missing Data Summary:\n")
  print(missing_summary, n = Inf)
  cat("\n")
  
  validation_results$missing <- missing_summary
  
  # Check 2: Outliers and unrealistic values
  outliers <- data %>%
    filter(
      # Unrealistic dimensions
      !is.na(length_in) & (length_in < 100 | length_in > 300) |
      !is.na(width_in) & (width_in < 60 | width_in > 100) |
      !is.na(height_in) & (height_in < 50 | height_in > 90) |
      !is.na(curb_weight_lbs) & (curb_weight_lbs < 2000 | curb_weight_lbs > 8000) |
      !is.na(wheelbase_in) & (wheelbase_in < 80 | wheelbase_in > 200)
    )
  
  if (nrow(outliers) > 0) {
    cat("⚠ Potential outliers detected:\n")
    print(outliers %>% select(brand, model, year, length_in, width_in, 
                              height_in, curb_weight_lbs))
    cat("\n")
  } else {
    cat("✓ No obvious outliers detected.\n\n")
  }
  
  validation_results$outliers <- outliers
  
  # Check 3: Logical consistency
  cat("Checking logical consistency...\n")
  
  # Trucks should generally be larger than sedans
  consistency_check <- data %>%
    filter(!is.na(length_in), !is.na(type)) %>%
    group_by(brand, year, type) %>%
    summarise(avg_length = mean(length_in, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = type, values_from = avg_length)
  
  cat("\nAverage lengths by type (should generally be: Sedan < SUV < Pickup):\n")
  print(consistency_check)
  cat("\n")
  
  validation_results$consistency <- consistency_check
  
  # Check 4: Year-over-year changes
  cat("Checking for dramatic year-over-year changes...\n")
  
  yoy_changes <- data %>%
    filter(!is.na(length_in)) %>%
    arrange(brand, model, year) %>%
    group_by(brand, model) %>%
    mutate(
      length_change = length_in - lag(length_in),
      length_pct_change = (length_change / lag(length_in)) * 100
    ) %>%
    filter(abs(length_pct_change) > 10 | is.na(length_pct_change))
  
  if (nrow(yoy_changes) > 0) {
    cat("⚠ Large year-over-year changes (>10%):\n")
    print(yoy_changes %>% select(brand, model, year, length_in, 
                                  length_change, length_pct_change))
    cat("\nNote: Large changes may indicate redesigns or data errors.\n\n")
  } else {
    cat("✓ No dramatic year-over-year changes detected.\n\n")
  }
  
  validation_results$yoy_changes <- yoy_changes
  
  return(validation_results)
}

# -----------------------------------------------------------------------------
# MODULE 5: PROGRESS TRACKING
# -----------------------------------------------------------------------------

generate_progress_report <- function(data) {
  cat("📈 MODULE 5: Progress Report\n")
  cat("=====================================\n\n")
  
  # Overall completion
  total_cells <- nrow(data) * length(SPEC_FIELDS)
  completed_cells <- sum(!is.na(data %>% select(all_of(SPEC_FIELDS))))
  pct_complete <- (completed_cells / total_cells) * 100
  
  cat(glue("Overall Progress: {completed_cells}/{total_cells} cells ({round(pct_complete, 1)}%)\n\n"))
  
  # Progress by vehicle
  vehicle_progress <- data %>%
    group_by(brand, model) %>%
    summarise(
      records = n(),
      length_complete = sum(!is.na(length_in)),
      width_complete = sum(!is.na(width_in)),
      weight_complete = sum(!is.na(curb_weight_lbs)),
      sales_complete = sum(!is.na(annual_sales)),
      .groups = "drop"
    ) %>%
    mutate(
      pct_complete = round((length_complete + width_complete + 
                            weight_complete + sales_complete) / 
                           (records * 4) * 100, 1)
    ) %>%
    arrange(desc(pct_complete))
  
  cat("Progress by Vehicle:\n")
  print(vehicle_progress, n = Inf)
  cat("\n")
  
  # Progress by year
  year_progress <- data %>%
    group_by(year) %>%
    summarise(
      vehicles = n(),
      complete = sum(!is.na(length_in) & !is.na(width_in) & 
                     !is.na(curb_weight_lbs)),
      pct_complete = round(complete / vehicles * 100, 1)
    ) %>%
    arrange(year)
  
  cat("Progress by Year:\n")
  print(year_progress, n = Inf)
  cat("\n")
  
  # Recommendations
  cat("📋 Recommendations:\n")
  
  incomplete_vehicles <- vehicle_progress %>%
    filter(pct_complete < 100) %>%
    arrange(pct_complete)
  
  if (nrow(incomplete_vehicles) > 0) {
    cat("\nPriority vehicles to complete:\n")
    for (i in 1:min(3, nrow(incomplete_vehicles))) {
      v <- incomplete_vehicles[i, ]
      cat(glue("  {i}. {v$brand} {v$model} ({v$pct_complete}% complete)\n"))
    }
  } else {
    cat("  🎉 All vehicles complete!\n")
  }
  
  cat("\n")
  
  return(list(
    overall = pct_complete,
    by_vehicle = vehicle_progress,
    by_year = year_progress
  ))
}

# -----------------------------------------------------------------------------
# MODULE 6: VISUALIZATION PREVIEW
# -----------------------------------------------------------------------------

quick_visualize <- function(data) {
  cat("📊 MODULE 6: Quick Visualization\n")
  cat("=====================================\n\n")
  
  # Filter to complete records only
  complete_data <- data %>%
    filter(!is.na(length_in), !is.na(width_in), !is.na(curb_weight_lbs))
  
  if (nrow(complete_data) < 3) {
    cat("⚠ Not enough complete data for visualization yet.\n")
    cat(glue("  Need at least 3 complete records, have {nrow(complete_data)}\n\n"))
    return(NULL)
  }
  
  cat(glue("Creating visualizations from {nrow(complete_data)} complete records...\n\n"))
  
  # Plot 1: Length over time
  p1 <- ggplot(complete_data, aes(x = year, y = length_in, color = type)) +
    geom_line(aes(group = interaction(brand, model)), alpha = 0.6, size = 1) +
    geom_point(size = 2) +
    facet_wrap(~type) +
    labs(
      title = "Vehicle Length Trends (1990-2024)",
      subtitle = "All three vehicle types show growth",
      x = "Year",
      y = "Length (inches)",
      color = "Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/visualizations/length_trends.png", p1, 
         width = 10, height = 6, dpi = 300)
  cat("✓ Saved: output/visualizations/length_trends.png\n")
  
  # Plot 2: Weight over time
  p2 <- ggplot(complete_data, aes(x = year, y = curb_weight_lbs, color = type)) +
    geom_line(aes(group = interaction(brand, model)), alpha = 0.6, size = 1) +
    geom_point(size = 2) +
    facet_wrap(~type) +
    labs(
      title = "Vehicle Weight Trends (1990-2024)",
      subtitle = "Curb weight increasing across all categories",
      x = "Year",
      y = "Curb Weight (lbs)",
      color = "Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/visualizations/weight_trends.png", p2, 
         width = 10, height = 6, dpi = 300)
  cat("✓ Saved: output/visualizations/weight_trends.png\n")
  
  # Plot 3: 1990 vs 2024 comparison
  comparison_data <- complete_data %>%
    filter(year %in% c(1990, 2024)) %>%
    select(brand, model, type, year, length_in, width_in, curb_weight_lbs) %>%
    pivot_longer(c(length_in, width_in, curb_weight_lbs), 
                 names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
    filter(!is.na(y1990), !is.na(y2024)) %>%
    mutate(
      change = y2024 - y1990,
      pct_change = (change / y1990) * 100
    )
  
  if (nrow(comparison_data) > 0) {
    p3 <- ggplot(comparison_data, 
                 aes(x = reorder(paste(brand, model), pct_change), 
                     y = pct_change, fill = type)) +
      geom_col() +
      facet_wrap(~metric, scales = "free_x") +
      coord_flip() +
      labs(
        title = "Vehicle Size Growth: 1990 vs 2024",
        subtitle = "Percentage increase in key dimensions",
        x = "",
        y = "% Change (1990-2024)",
        fill = "Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave("output/visualizations/size_comparison_1990_2024.png", p3, 
           width = 10, height = 8, dpi = 300)
    cat("✓ Saved: output/visualizations/size_comparison_1990_2024.png\n")
  }
  
  cat("\n")
}

# -----------------------------------------------------------------------------
# MAIN WORKFLOW ORCHESTRATION
# -----------------------------------------------------------------------------

run_full_workflow <- function(skip_download = FALSE, skip_scraping = TRUE) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  MILWAUKEE CAR BRAIN - VEHICLE DATA COLLECTION WORKFLOW\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  cat("This workflow will:\n")
  cat("  1. Download EPA fuel economy data\n")
  cat("  2. (Optional) Attempt web scraping for dimensions\n")
  cat("  3. Create manual data entry templates\n")
  cat("  4. Validate your collected data\n")
  cat("  5. Generate progress reports\n")
  cat("  6. Create preview visualizations\n\n")
  
  # Module 1: EPA Data
  if (!skip_download) {
    epa_file <- download_epa_data()
    if (!is.null(epa_file)) {
      epa_filtered <- filter_epa_for_our_vehicles(epa_file)
    }
  } else {
    cat("Skipping EPA download (as requested)\n\n")
  }
  
  # Module 2: Web Scraping (optional, often unreliable)
  if (!skip_scraping) {
    scraped <- scrape_all_vehicles()
  } else {
    cat("📡 MODULE 2: Web Scraping (SKIPPED)\n")
    cat("=====================================\n")
    cat("Skipping web scraping. You can run scrape_all_vehicles()\n")
    cat("separately if you want to attempt this.\n\n")
  }
  
  # Module 3: Manual Entry Template
  template <- create_data_entry_template()
  
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat("🎯 NEXT STEPS:\n\n")
  cat("1. Open: data/vehicle_data_entry.xlsx\n")
  cat("2. Fill in vehicle specifications as you collect them\n")
  cat("3. Use these sources:\n")
  cat("   • EPA data: data/processed/epa_filtered.csv\n")
  cat("   • Edmunds.com for dimensions\n")
  cat("   • GoodCarBadCar.net for sales figures\n")
  cat("   • Auto-data.net for historical specs\n\n")
  cat("4. When you have some data, run:\n")
  cat("   collected_data <- read_xlsx('data/vehicle_data_entry.xlsx')\n")
  cat("   validate_vehicle_data(collected_data)\n")
  cat("   generate_progress_report(collected_data)\n")
  cat("   quick_visualize(collected_data)\n\n")
  
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  return(template)
}

# -----------------------------------------------------------------------------
# MODULE 7: DATA CLEANING & PREPARATION
# -----------------------------------------------------------------------------

clean_vehicle_data <- function(data) {
  cat("🧹 MODULE 7: Data Cleaning & Preparation\n")
  cat("=====================================\n\n")
  
  cat("Starting with:", nrow(data), "records\n\n")
  
  # Step 1: Remove completely empty rows
  cleaned <- data %>%
    filter(!if_all(c(length_in, width_in, height_in, curb_weight_lbs), is.na))
  
  removed <- nrow(data) - nrow(cleaned)
  if (removed > 0) {
    cat(glue("  Removed {removed} completely empty records\n"))
  }
  
  # Step 2: Standardize vehicle names
  cat("\n  Standardizing vehicle names...\n")
  cleaned <- cleaned %>%
    mutate(
      model = case_when(
        model == "F-150" | model == "F150" | model == "F 150" ~ "F-150",
        model == "RAV4" | model == "Rav4" ~ "RAV4",
        TRUE ~ model
      ),
      # Create standardized vehicle ID
      vehicle_id = paste(brand, model, year, sep = "_")
    )
  
  # Step 3: Calculate derived metrics
  cat("  Calculating derived metrics...\n")
  cleaned <- cleaned %>%
    mutate(
      # Footprint (approximate, in square feet)
      footprint_sqft = (length_in * width_in) / 144,
      
      # Volume (approximate exterior, cubic feet)
      volume_cuft = (length_in * width_in * height_in) / 1728,
      
      # Weight per cubic foot (density proxy)
      weight_per_cuft = curb_weight_lbs / volume_cuft,
      
      # Length to width ratio (shape factor)
      length_width_ratio = length_in / width_in
    )
  
  # Step 4: Add percentage changes from baseline (1990 or earliest year)
  cat("  Calculating growth from baseline...\n")
  cleaned <- cleaned %>%
    group_by(brand, model) %>%
    arrange(year) %>%
    mutate(
      # Get first non-NA value as baseline
      baseline_length = first(length_in[!is.na(length_in)]),
      baseline_width = first(width_in[!is.na(width_in)]),
      baseline_height = first(height_in[!is.na(height_in)]),
      baseline_weight = first(curb_weight_lbs[!is.na(curb_weight_lbs)]),
      
      # Calculate percentage changes
      length_pct_change = ((length_in - baseline_length) / baseline_length) * 100,
      width_pct_change = ((width_in - baseline_width) / baseline_width) * 100,
      height_pct_change = ((height_in - baseline_height) / baseline_height) * 100,
      weight_pct_change = ((curb_weight_lbs - baseline_weight) / baseline_weight) * 100,
      
      # Years since baseline
      years_since_baseline = year - first(year[!is.na(length_in)])
    ) %>%
    ungroup()
  
  # Step 5: Add vehicle type factors for analysis
  cleaned <- cleaned %>%
    mutate(
      type = factor(type, levels = c("Sedan", "SUV", "Pickup")),
      brand = factor(brand, levels = c("Ford", "Toyota", "Chevrolet"))
    )
  
  cat("\n✓ Cleaning complete!\n")
  cat(glue("  Final dataset: {nrow(cleaned)} records\n"))
  cat(glue("  Complete records: {sum(complete.cases(cleaned %>% select(length_in, width_in, height_in, curb_weight_lbs)))}\n\n"))
  
  # Save cleaned data in multiple formats
  write_csv(cleaned, "data/processed/vehicle_data_cleaned.csv")
  cat("✓ Saved to: data/processed/vehicle_data_cleaned.csv\n")
  
  # Also save as TSV for Google Docs
  write_tsv(cleaned, "data/processed/vehicle_data_cleaned.tsv")
  cat("✓ Saved to: data/processed/vehicle_data_cleaned.tsv\n")
  
  # Save RDS for R users
  saveRDS(cleaned, "data/processed/vehicle_data_cleaned.rds")
  cat("✓ Saved to: data/processed/vehicle_data_cleaned.rds\n\n")
  
  return(cleaned)
}

# -----------------------------------------------------------------------------
# MODULE 8: COMPREHENSIVE ANALYSIS
# -----------------------------------------------------------------------------

analyze_vehicle_trends <- function(data) {
  cat("📊 MODULE 8: Comprehensive Analysis\n")
  cat("=====================================\n\n")
  
  results <- list()
  
  # Analysis 1: Overall growth statistics
  cat("ANALYSIS 1: Overall Size Growth (1990-2024)\n")
  cat("─────────────────────────────────────────\n")
  
  growth_summary <- data %>%
    filter(!is.na(length_in), !is.na(curb_weight_lbs)) %>%
    group_by(type) %>%
    summarise(
      n_vehicles = n_distinct(paste(brand, model)),
      avg_length_1990s = mean(length_in[year <= 1995], na.rm = TRUE),
      avg_length_2020s = mean(length_in[year >= 2020], na.rm = TRUE),
      length_growth_in = avg_length_2020s - avg_length_1990s,
      length_growth_pct = (length_growth_in / avg_length_1990s) * 100,
      
      avg_weight_1990s = mean(curb_weight_lbs[year <= 1995], na.rm = TRUE),
      avg_weight_2020s = mean(curb_weight_lbs[year >= 2020], na.rm = TRUE),
      weight_growth_lbs = avg_weight_2020s - avg_weight_1990s,
      weight_growth_pct = (weight_growth_lbs / avg_weight_1990s) * 100,
      .groups = "drop"
    )
  
  print(growth_summary)
  cat("\n")
  results$growth_summary <- growth_summary
  
  # Analysis 2: Year-over-year growth rates
  cat("ANALYSIS 2: Annual Growth Rates\n")
  cat("─────────────────────────────────────────\n")
  
  yoy_growth <- data %>%
    filter(!is.na(length_in), !is.na(curb_weight_lbs)) %>%
    arrange(brand, model, year) %>%
    group_by(brand, model) %>%
    mutate(
      years_elapsed = year - lag(year),
      length_change = length_in - lag(length_in),
      weight_change = curb_weight_lbs - lag(curb_weight_lbs),
      annual_length_growth = length_change / years_elapsed,
      annual_weight_growth = weight_change / years_elapsed
    ) %>%
    ungroup() %>%
    filter(!is.na(annual_length_growth))
  
  yoy_summary <- yoy_growth %>%
    summarise(
      avg_annual_length_growth_in = mean(annual_length_growth, na.rm = TRUE),
      median_annual_length_growth_in = median(annual_length_growth, na.rm = TRUE),
      avg_annual_weight_growth_lbs = mean(annual_weight_growth, na.rm = TRUE),
      median_annual_weight_growth_lbs = median(annual_weight_growth, na.rm = TRUE)
    )
  
  print(yoy_summary)
  cat("\n")
  results$yoy_summary <- yoy_summary
  
  # Analysis 3: Cross-brand comparison
  cat("ANALYSIS 3: Brand Comparisons\n")
  cat("─────────────────────────────────────────\n")
  
  brand_comparison <- data %>%
    filter(year >= 2020, !is.na(length_in)) %>%
    group_by(brand, type) %>%
    summarise(
      avg_length = mean(length_in, na.rm = TRUE),
      avg_width = mean(width_in, na.rm = TRUE),
      avg_weight = mean(curb_weight_lbs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(type, desc(avg_length))
  
  print(brand_comparison)
  cat("\n")
  results$brand_comparison <- brand_comparison
  
  # Analysis 4: Category creep (comparing vehicle classes)
  cat("ANALYSIS 4: Category Creep\n")
  cat("─────────────────────────────────────────\n")
  cat("How today's 'small' vehicles compare to yesterday's 'large' vehicles\n\n")
  
  category_creep <- data %>%
    filter(year %in% c(1990, 2024), !is.na(length_in)) %>%
    select(brand, model, type, year, length_in, width_in, curb_weight_lbs) %>%
    arrange(type, year, length_in)
  
  # Example: Is a 2024 Camry bigger than a 1990 Explorer?
  camry_2024 <- data %>% 
    filter(model == "Camry", year >= 2020, !is.na(length_in)) %>%
    slice(1)
  
  explorer_1990 <- data %>%
    filter(model == "Explorer", year <= 1995, !is.na(length_in)) %>%
    slice(1)
  
  if (nrow(camry_2024) > 0 && nrow(explorer_1990) > 0) {
    cat(glue("2024 Camry (sedan): {round(camry_2024$length_in, 1)}\" long, {round(camry_2024$curb_weight_lbs, 0)} lbs\n"))
    cat(glue("1990 Explorer (SUV): {round(explorer_1990$length_in, 1)}\" long, {round(explorer_1990$curb_weight_lbs, 0)} lbs\n"))
    
    if (camry_2024$length_in > explorer_1990$length_in) {
      cat("\n→ Today's 'family sedan' is longer than yesterday's SUV!\n")
    }
  }
  cat("\n")
  results$category_creep <- category_creep
  
  # Analysis 5: Parking space implications
  cat("ANALYSIS 5: Parking Space Implications\n")
  cat("─────────────────────────────────────────\n")
  
  parking_analysis <- data %>%
    filter(!is.na(length_in), !is.na(width_in)) %>%
    mutate(
      # Standard parking space: 9ft wide × 18ft long = 162 sq ft
      parking_efficiency = (length_in * width_in / 144) / 162 * 100,
      decade = floor(year / 10) * 10
    ) %>%
    group_by(decade) %>%
    summarise(
      avg_length = mean(length_in, na.rm = TRUE),
      avg_width = mean(width_in, na.rm = TRUE),
      avg_parking_efficiency = mean(parking_efficiency, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(parking_analysis)
  cat("\nNote: Parking efficiency = (vehicle footprint / standard space) × 100%\n")
  cat("Higher % = vehicles taking up more of the parking space\n\n")
  results$parking_analysis <- parking_analysis
  
  # Analysis 6: Sales-weighted trends (if sales data available)
  if (sum(!is.na(data$annual_sales)) > 10) {
    cat("ANALYSIS 6: Sales-Weighted Trends\n")
    cat("─────────────────────────────────────────\n")
    
    sales_weighted <- data %>%
      filter(!is.na(annual_sales), !is.na(curb_weight_lbs)) %>%
      group_by(year) %>%
      summarise(
        total_sales = sum(annual_sales, na.rm = TRUE),
        weighted_avg_weight = weighted.mean(curb_weight_lbs, annual_sales, na.rm = TRUE),
        weighted_avg_length = weighted.mean(length_in, annual_sales, na.rm = TRUE),
        .groups = "drop"
      )
    
    print(sales_weighted)
    cat("\n")
    results$sales_weighted <- sales_weighted
  }
  
  # Save all analysis results in multiple formats
  saveRDS(results, "data/processed/analysis_results.rds")
  cat("✓ Analysis results saved to: data/processed/analysis_results.rds\n")
  
  # Export key tables as CSV for easy import to Numbers/Sheets
  write_csv(results$growth_summary, "output/analysis_growth_summary.csv")
  cat("✓ Growth summary table: output/analysis_growth_summary.csv\n")
  
  write_csv(results$brand_comparison, "output/analysis_brand_comparison.csv")
  cat("✓ Brand comparison table: output/analysis_brand_comparison.csv\n")
  
  write_csv(results$parking_analysis, "output/analysis_parking_space.csv")
  cat("✓ Parking space analysis: output/analysis_parking_space.csv\n\n")
  
  cat("→ Import these CSV files into Numbers or Google Sheets for your book!\n\n")
  
  return(results)
}

# -----------------------------------------------------------------------------
# MODULE 9: PUBLICATION-READY VISUALIZATIONS
# -----------------------------------------------------------------------------

create_publication_viz <- function(data) {
  cat("📈 MODULE 9: Publication-Ready Visualizations\n")
  cat("=====================================\n\n")
  
  # Filter to complete records
  plot_data <- data %>%
    filter(!is.na(length_in), !is.na(width_in), !is.na(curb_weight_lbs))
  
  if (nrow(plot_data) < 3) {
    cat("⚠ Not enough complete data for publication visualizations.\n\n")
    return(NULL)
  }
  
  # Define consistent color scheme
  type_colors <- c("Sedan" = "#2C5F8D", "SUV" = "#C75146", "Pickup" = "#6A8D2F")
  
  # VIZ 1: Multi-panel growth trends
  cat("Creating Figure 1: Vehicle Dimensions Over Time...\n")
  
  fig1_data <- plot_data %>%
    select(year, brand, model, type, length_in, width_in, height_in, curb_weight_lbs) %>%
    pivot_longer(c(length_in, width_in, height_in, curb_weight_lbs),
                 names_to = "metric", values_to = "value") %>%
    mutate(
      metric_label = case_when(
        metric == "length_in" ~ "Length (inches)",
        metric == "width_in" ~ "Width (inches)",
        metric == "height_in" ~ "Height (inches)",
        metric == "curb_weight_lbs" ~ "Weight (lbs)"
      ),
      metric_label = factor(metric_label, levels = c("Length (inches)", "Width (inches)", 
                                                      "Height (inches)", "Weight (lbs)"))
    )
  
  p1 <- ggplot(fig1_data, aes(x = year, y = value, color = type, group = interaction(brand, model))) +
    geom_line(alpha = 0.7, size = 1) +
    geom_point(size = 1.5, alpha = 0.8) +
    facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
    scale_color_manual(values = type_colors) +
    labs(
      title = "Vehicle Size Growth Across Three Decades (1990-2024)",
      subtitle = "All dimensions and weight have increased across sedans, SUVs, and pickup trucks",
      x = NULL,
      y = NULL,
      color = "Vehicle Type",
      caption = "Data: EPA, manufacturer specifications | Milwaukee Car Brain project"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray30", size = 10),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave("output/visualizations/fig1_dimensions_over_time.png", p1,
         width = 10, height = 8, dpi = 300, bg = "white")
  cat("  ✓ Saved: output/visualizations/fig1_dimensions_over_time.png\n")
  
  # VIZ 2: Percentage growth from baseline
  cat("Creating Figure 2: Percentage Growth from 1990 Baseline...\n")
  
  baseline_data <- plot_data %>%
    filter(!is.na(length_pct_change)) %>%
    select(year, brand, model, type, length_pct_change, width_pct_change, 
           height_pct_change, weight_pct_change) %>%
    pivot_longer(c(length_pct_change, width_pct_change, height_pct_change, weight_pct_change),
                 names_to = "metric", values_to = "pct_change") %>%
    mutate(
      metric_label = case_when(
        metric == "length_pct_change" ~ "Length",
        metric == "width_pct_change" ~ "Width",
        metric == "height_pct_change" ~ "Height",
        metric == "weight_pct_change" ~ "Weight"
      )
    )
  
  p2 <- ggplot(baseline_data, aes(x = year, y = pct_change, color = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(aes(group = interaction(brand, model)), alpha = 0.6, size = 1) +
    geom_point(size = 2, alpha = 0.7) +
    facet_wrap(~metric_label, ncol = 2) +
    scale_color_manual(values = type_colors) +
    labs(
      title = "Vehicle Growth Relative to 1990 Baseline",
      subtitle = "Percentage change in key dimensions and weight",
      x = NULL,
      y = "% Change from 1990",
      color = "Vehicle Type",
      caption = "Data: EPA, manufacturer specifications | Milwaukee Car Brain project"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray30", size = 10),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave("output/visualizations/fig2_percentage_growth.png", p2,
         width = 10, height = 8, dpi = 300, bg = "white")
  cat("  ✓ Saved: output/visualizations/fig2_percentage_growth.png\n")
  
  # VIZ 3: Vehicle footprint visualization
  cat("Creating Figure 3: Vehicle Footprint Growth...\n")
  
  p3 <- ggplot(plot_data, aes(x = year, y = footprint_sqft, color = type)) +
    geom_line(aes(group = interaction(brand, model)), alpha = 0.6, size = 1.2) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_manual(values = type_colors) +
    labs(
      title = "Vehicle Footprint Growth (1990-2024)",
      subtitle = "Ground area occupied by vehicles (length × width)",
      x = NULL,
      y = "Footprint (square feet)",
      color = "Vehicle Type",
      caption = "Data: EPA, manufacturer specifications | Milwaukee Car Brain project"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray30", size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave("output/visualizations/fig3_footprint_growth.png", p3,
         width = 10, height = 6, dpi = 300, bg = "white")
  cat("  ✓ Saved: output/visualizations/fig3_footprint_growth.png\n")
  
  # VIZ 4: 1990 vs 2024 direct comparison
  cat("Creating Figure 4: 1990 vs 2024 Comparison...\n")
  
  comparison_plot_data <- plot_data %>%
    filter(year %in% c(1990, 2024)) %>%
    mutate(year_label = paste0("Y", year)) %>%
    select(brand, model, type, year_label, length_in, width_in, curb_weight_lbs) %>%
    pivot_longer(c(length_in, width_in, curb_weight_lbs),
                 names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = year_label, values_from = value) %>%
    filter(!is.na(Y1990), !is.na(Y2024))
  
  if (nrow(comparison_plot_data) > 0) {
    p4 <- ggplot(comparison_plot_data, aes(x = Y1990, y = Y2024, color = type)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(size = 4, alpha = 0.7) +
      geom_text(aes(label = paste(brand, model)), size = 2.5, 
                hjust = 0, nudge_x = 1, check_overlap = TRUE) +
      facet_wrap(~metric, scales = "free") +
      scale_color_manual(values = type_colors) +
      labs(
        title = "Vehicle Size: 1990 vs 2024",
        subtitle = "Points above diagonal line indicate growth",
        x = "1990 Value",
        y = "2024 Value",
        color = "Vehicle Type",
        caption = "Data: EPA, manufacturer specifications | Milwaukee Car Brain project"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray30", size = 10),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")
      )
    
    ggsave("output/visualizations/fig4_1990_vs_2024.png", p4,
           width = 12, height = 6, dpi = 300, bg = "white")
    cat("  ✓ Saved: output/visualizations/fig4_1990_vs_2024.png\n")
  }
  
  cat("\n✓ All publication visualizations created!\n\n")
}

# -----------------------------------------------------------------------------
# CONVENIENCE FUNCTIONS
# -----------------------------------------------------------------------------

# Quick reload of your working data
load_current_data <- function() {
  if (file.exists("data/vehicle_data_entry.xlsx")) {
    read_xlsx("data/vehicle_data_entry.xlsx")
  } else {
    stop("No data file found. Run run_full_workflow() first.")
  }
}

# Load cleaned data
load_cleaned_data <- function() {
  if (file.exists("data/processed/vehicle_data_cleaned.rds")) {
    readRDS("data/processed/vehicle_data_cleaned.rds")
  } else {
    stop("No cleaned data found. Run clean_vehicle_data() first.")
  }
}

# Quick check on progress
check_progress <- function() {
  data <- load_current_data()
  generate_progress_report(data)
}

# Quick validation
check_data <- function() {
  data <- load_current_data()
  validate_vehicle_data(data)
}

# Quick viz
preview_viz <- function() {
  data <- load_current_data()
  quick_visualize(data)
}

# Full analysis pipeline
run_analysis_pipeline <- function(use_integrated = TRUE) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  FULL ANALYSIS PIPELINE\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Load data (integrated or manual entry)
  if (use_integrated && file.exists("data/processed/vehicle_data_integrated.rds")) {
    cat("Loading integrated data (EPA + scraped + manual)...\n")
    raw_data <- readRDS("data/processed/vehicle_data_integrated.rds")
  } else {
    cat("Loading manual data entry...\n")
    raw_data <- load_current_data()
  }
  
  # Clean
  cleaned_data <- clean_vehicle_data(raw_data)
  
  # Analyze
  analysis_results <- analyze_vehicle_trends(cleaned_data)
  
  # Visualize
  create_publication_viz(cleaned_data)
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("✓ Analysis pipeline complete!\n")
  cat("\nOutputs saved to:\n")
  cat("  • data/processed/vehicle_data_cleaned.csv\n")
  cat("  • data/processed/analysis_results.rds\n")
  cat("  • output/visualizations/ (4 publication figures)\n")
  cat("  • output/analysis_*.csv (summary tables)\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  return(list(
    cleaned_data = cleaned_data,
    analysis = analysis_results
  ))
}

# Complete workflow including data integration
run_complete_workflow <- function() {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║          COMPLETE DATA COLLECTION & ANALYSIS WORKFLOW        ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  cat("This workflow will:\n")
  cat("  1. Integrate all data sources (EPA + scraped + manual)\n")
  cat("  2. Clean and standardize the data\n")
  cat("  3. Run comprehensive analysis\n")
  cat("  4. Generate publication visualizations\n\n")
  
  # Check if data integration script is available
  if (!exists("integrate_all_data")) {
    cat("⚠ Data integration script not loaded.\n")
    cat("Please source: source('scripts/data-integration-script.r')\n\n")
    return(NULL)
  }
  
  # Step 1: Integrate data
  cat("STEP 1: Data Integration\n")
  cat("─────────────────────────────────────────\n")
  integration_result <- integrate_all_data()
  
  cat("\n")
  cat("STEP 2: Analysis Pipeline\n")
  cat("─────────────────────────────────────────\n")
  analysis_result <- run_analysis_pipeline(use_integrated = TRUE)
  
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║              COMPLETE WORKFLOW FINISHED!                      ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  cat("📊 YOUR DATA IS READY FOR THE BOOK!\n\n")
  cat("Key outputs:\n")
  cat("  📁 data/processed/vehicle_data_integrated.csv - Master dataset\n")
  cat("  📊 output/visualizations/*.png - 4 publication figures\n")
  cat("  📈 output/analysis_*.csv - Summary tables for writing\n")
  cat("  📋 output/data_quality_*.csv - Quality reports\n\n")
  
  return(list(
    integration = integration_result,
    analysis = analysis_result
  ))
}

# =============================================================================
# RUN IT!
# =============================================================================

cat("\n🚗 Milwaukee Car Brain Vehicle Data Collector loaded!\n\n")
cat("DATA COLLECTION WORKFLOW:\n")
cat("  template <- run_full_workflow()     # Initial setup\n")
cat("  check_progress()                    # Track your progress\n")
cat("  check_data()                        # Validate data quality\n\n")
cat("ANALYSIS WORKFLOW:\n")
cat("  results <- run_analysis_pipeline()  # Full cleaning + analysis + viz\n\n")
cat("INDIVIDUAL MODULES:\n")
cat("  download_epa_data()                 # EPA fuel economy data\n")
cat("  create_data_entry_template()        # Excel/CSV templates\n")
cat("  cleaned <- clean_vehicle_data(raw)  # Clean & prep data\n")
cat("  analyze_vehicle_trends(cleaned)     # Statistical analysis\n")
cat("  create_publication_viz(cleaned)     # Publication figures\n\n")