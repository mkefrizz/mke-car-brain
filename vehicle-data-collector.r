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
  
  epa_file <- "data/raw/epa_vehicles_all.csv"
  
  if (!file.exists(epa_file) || force_download) {
    cat("Downloading EPA fuel economy data...\n")
    cat("Note: This may take a minute. The file is ~100MB.\n\n")
    
    # EPA provides annual files - we'll need to get multiple years
    # For now, we'll use their bulk download (1984-2024)
    epa_url <- "https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip"
    
    tryCatch({
      # Download and unzip
      temp_zip <- tempfile(fileext = ".zip")
      download.file(epa_url, temp_zip, mode = "wb", quiet = FALSE)
      unzip(temp_zip, exdir = "data/raw")
      unlink(temp_zip)
      
      cat("✓ EPA data downloaded successfully!\n\n")
    }, error = function(e) {
      cat("⚠ Download failed. You can manually download from:\n")
      cat("  https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip\n")
      cat("  and place in data/raw/\n\n")
      cat("Error:", e$message, "\n\n")
    })
  } else {
    cat("✓ EPA data already downloaded.\n\n")
  }
  
  if (file.exists(epa_file)) {
    return(epa_file)
  } else {
    return(NULL)
  }
}

filter_epa_for_our_vehicles <- function(epa_file) {
  cat("Filtering EPA data for our target vehicles...\n")
  
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
  
  # Save as Excel for easy manual entry
  write_xlsx(template, "data/vehicle_data_entry.xlsx")
  
  cat("✓ Created Excel template: data/vehicle_data_entry.xlsx\n\n")
  cat("Template includes:\n")
  cat("  • All 9 vehicles × 8 years = 72 rows\n")
  cat("  • Pre-filled vehicle identification\n")
  cat("  • Empty fields for specifications\n")
  cat("  • Metadata fields for tracking data quality\n\n")
  cat("You can fill this in as you collect data from various sources.\n\n")
  
  return(template)
}

# Also create a CSV version for those who prefer it
create_csv_template <- function() {
  template <- create_data_entry_template()
  write_csv(template, "data/vehicle_data_entry.csv")
  cat("✓ Also created CSV version: data/vehicle_data_entry.csv\n\n")
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

# =============================================================================
# RUN IT!
# =============================================================================

cat("\n🚗 Milwaukee Car Brain Vehicle Data Collector loaded!\n\n")
cat("To start, run:\n")
cat("  template <- run_full_workflow()\n\n")
cat("Or for individual modules:\n")
cat("  download_epa_data()           # Get EPA fuel economy data\n")
cat("  create_data_entry_template()  # Create Excel/CSV templates\n")
cat("  check_progress()              # See how much data you've collected\n")
cat("  check_data()                  # Validate your data\n")
cat("  preview_viz()                 # Generate quick visualizations\n\n")