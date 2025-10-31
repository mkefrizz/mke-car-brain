# =============================================================================
# VEHICLE SPECIFICATIONS WEB SCRAPER
# =============================================================================
# Attempts to scrape vehicle dimensions from multiple sources
# Priority: Wikipedia > Auto-data.net > Edmunds
# =============================================================================

library(tidyverse)
library(rvest)
library(httr)
library(glue)

# -----------------------------------------------------------------------------
# SOURCE 1: WIKIPEDIA (Most Reliable)
# -----------------------------------------------------------------------------

scrape_wikipedia_specs <- function(vehicle_name, year) {
  cat(glue("Trying Wikipedia for {year} {vehicle_name}...\n"))
  
  # Wikipedia URLs are somewhat predictable
  # Example: https://en.wikipedia.org/wiki/Ford_F-Series
  base_urls <- c(
    glue("https://en.wikipedia.org/wiki/{gsub(' ', '_', vehicle_name)}"),
    glue("https://en.wikipedia.org/wiki/{gsub(' ', '_', vehicle_name)}_(car)"),
    glue("https://en.wikipedia.org/wiki/{gsub(' ', '_', vehicle_name)}_(truck)")
  )
  
  for (url in base_urls) {
    tryCatch({
      page <- read_html(url)
      
      # Look for infobox tables (Wikipedia's standard format)
      tables <- page %>% html_table(fill = TRUE)
      
      # Search tables for specifications
      for (table in tables) {
        if (any(grepl("Length|Width|Height|Weight", names(table), ignore.case = TRUE))) {
          cat("  ✓ Found specs table on Wikipedia!\n")
          return(list(source = "wikipedia", url = url, tables = tables))
        }
      }
    }, error = function(e) {
      # Silently continue to next URL
    })
  }
  
  cat("  ✗ No Wikipedia data found\n")
  return(NULL)
}

# -----------------------------------------------------------------------------
# SOURCE 2: AUTO-DATA.NET (Structured Database)
# -----------------------------------------------------------------------------

scrape_autodata_specs <- function(make, model, year) {
  cat(glue("Trying auto-data.net for {year} {make} {model}...\n"))
  
  # auto-data.net has predictable URL structure
  # Example: https://www.auto-data.net/en/ford-f-150-xiii-regular-cab-3.5-v6-375hp-4wd-automatic-2018
  
  make_clean <- tolower(gsub(" ", "-", make))
  model_clean <- tolower(gsub(" ", "-", model))
  
  # Try a few URL patterns
  url_patterns <- c(
    glue("https://www.auto-data.net/en/{make_clean}-{model_clean}-{year}"),
    glue("https://www.auto-data.net/en/search/?q={make}+{model}+{year}")
  )
  
  for (url in url_patterns) {
    tryCatch({
      page <- read_html(url)
      
      # Look for specifications sections
      specs <- page %>%
        html_elements(".type") %>%
        html_text2()
      
      if (length(specs) > 0) {
        cat("  ✓ Found specs on auto-data.net!\n")
        
        # Extract dimensions (this would need refinement based on actual site structure)
        dimensions <- list(
          length = page %>% html_element(xpath = "//td[contains(text(), 'Length')]/following-sibling::td") %>% html_text2(),
          width = page %>% html_element(xpath = "//td[contains(text(), 'Width')]/following-sibling::td") %>% html_text2(),
          height = page %>% html_element(xpath = "//td[contains(text(), 'Height')]/following-sibling::td") %>% html_text2(),
          weight = page %>% html_element(xpath = "//td[contains(text(), 'Curb weight')]/following-sibling::td") %>% html_text2()
        )
        
        return(list(source = "auto-data.net", url = url, specs = dimensions))
      }
    }, error = function(e) {
      # Continue to next pattern
    })
  }
  
  cat("  ✗ No auto-data.net data found\n")
  return(NULL)
}

# -----------------------------------------------------------------------------
# SOURCE 3: EDMUNDS (Commercial Site - Less Reliable)
# -----------------------------------------------------------------------------

scrape_edmunds_specs <- function(make, model, year) {
  cat(glue("Trying Edmunds for {year} {make} {model}...\n"))
  
  make_clean <- tolower(gsub(" ", "-", make))
  model_clean <- tolower(gsub(" ", "-", model))
  
  # Edmunds URL pattern
  url <- glue("https://www.edmunds.com/{make_clean}/{model_clean}/{year}/features-specs/")
  
  tryCatch({
    # Need to simulate a browser (Edmunds blocks basic scrapers)
    response <- GET(url, user_agent("Mozilla/5.0"))
    
    if (status_code(response) == 200) {
      page <- read_html(response)
      
      # Look for specs sections (structure changes frequently)
      specs <- page %>%
        html_elements(".specs-list") %>%
        html_text2()
      
      if (length(specs) > 0) {
        cat("  ✓ Found specs on Edmunds!\n")
        return(list(source = "edmunds", url = url, data = specs))
      }
    }
  }, error = function(e) {
    cat(glue("  ✗ Edmunds error: {e$message}\n"))
  })
  
  cat("  ✗ No Edmunds data found\n")
  return(NULL)
}

# -----------------------------------------------------------------------------
# SOURCE 4: MANUFACTURER PRESS RELEASES (Archive.org)
# -----------------------------------------------------------------------------

scrape_manufacturer_archive <- function(make, model, year) {
  cat(glue("Trying manufacturer archives for {year} {make} {model}...\n"))
  
  # Many manufacturers have press release archives
  # Example: https://media.ford.com/content/fordmedia/fna/us/en/news/2024/01/09/all-new-2024-ford-mustang-officially-begins-production.html
  
  archive_urls <- list(
    "Ford" = "https://media.ford.com",
    "Toyota" = "https://pressroom.toyota.com",
    "Chevrolet" = "https://media.chevrolet.com"
  )
  
  base_url <- archive_urls[[make]]
  
  if (!is.null(base_url)) {
    # Search their site (this is very brand-specific)
    search_url <- glue("{base_url}/search?q={model}+{year}+specifications")
    
    tryCatch({
      # This would need significant customization per manufacturer
      cat("  ⚠ Manufacturer archives require manual inspection\n")
      cat(glue("    Visit: {search_url}\n"))
    }, error = function(e) {})
  }
  
  return(NULL)
}

# -----------------------------------------------------------------------------
# MASTER SCRAPING FUNCTION
# -----------------------------------------------------------------------------

scrape_vehicle_specs <- function(brand, model, year) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(glue("SCRAPING: {year} {brand} {model}\n"))
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  results <- list(
    brand = brand,
    model = model,
    year = year,
    data = NULL,
    source = NULL,
    success = FALSE
  )
  
  # Try sources in order of reliability
  
  # 1. Wikipedia (most stable)
  wiki_data <- scrape_wikipedia_specs(glue("{brand} {model}"), year)
  if (!is.null(wiki_data)) {
    results$data <- wiki_data
    results$source <- "wikipedia"
    results$success <- TRUE
    return(results)
  }
  
  # 2. Auto-data.net (structured database)
  Sys.sleep(2)  # Be polite
  autodata <- scrape_autodata_specs(brand, model, year)
  if (!is.null(autodata)) {
    results$data <- autodata
    results$source <- "auto-data.net"
    results$success <- TRUE
    return(results)
  }
  
  # 3. Edmunds (less reliable)
  Sys.sleep(2)  # Be polite
  edmunds <- scrape_edmunds_specs(brand, model, year)
  if (!is.null(edmunds)) {
    results$data <- edmunds
    results$source <- "edmunds"
    results$success <- TRUE
    return(results)
  }
  
  # 4. Manufacturer archives (requires manual follow-up)
  scrape_manufacturer_archive(brand, model, year)
  
  cat("\n⚠ Unable to scrape data automatically.\n")
  cat("Recommended manual sources:\n")
  cat(glue("  1. Wikipedia: https://en.wikipedia.org/wiki/{gsub(' ', '_', brand)}_{gsub(' ', '_', model)}\n"))
  cat(glue("  2. Auto-data.net: https://www.auto-data.net/en/search/?q={brand}+{model}+{year}\n"))
  cat(glue("  3. Edmunds: https://www.edmunds.com/{tolower(brand)}/{tolower(gsub(' ', '-', model))}/{year}/features-specs/\n"))
  cat("\n")
  
  return(results)
}

# -----------------------------------------------------------------------------
# BATCH SCRAPING FUNCTIONS
# -----------------------------------------------------------------------------

scrape_all_vehicles <- function(vehicles_df, years) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  BATCH VEHICLE SCRAPING\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  all_results <- list()
  
  for (i in 1:nrow(vehicles_df)) {
    vehicle <- vehicles_df[i, ]
    
    for (year in years) {
      key <- glue("{vehicle$brand}_{vehicle$model}_{year}")
      
      result <- scrape_vehicle_specs(vehicle$brand, vehicle$model, year)
      all_results[[key]] <- result
      
      # Be very polite - wait between requests
      Sys.sleep(3)
    }
  }
  
  # Save results
  saveRDS(all_results, "data/raw/scraped_specs.rds")
  
  # Create summary
  summary <- map_df(all_results, function(x) {
    tibble(
      brand = x$brand,
      model = x$model,
      year = x$year,
      source = x$source %||% "none",
      success = x$success
    )
  })
  
  write_csv(summary, "data/raw/scraping_summary.csv")
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("SCRAPING COMPLETE\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat(glue("Success rate: {sum(summary$success)}/{nrow(summary)} ({round(sum(summary$success)/nrow(summary)*100, 1)}%)\n\n"))
  cat("Results saved to:\n")
  cat("  • data/raw/scraped_specs.rds (full data)\n")
  cat("  • data/raw/scraping_summary.csv (summary)\n\n")
  
  return(list(results = all_results, summary = summary))
}

# -----------------------------------------------------------------------------
# HELPER: EXTRACT NUMBERS FROM SCRAPED TEXT
# -----------------------------------------------------------------------------

extract_dimension <- function(text, unit = "in") {
  # Extract number from text like "Length: 231.7 in" or "231.7"
  if (is.null(text) || is.na(text) || text == "") return(NA)
  
  # Remove commas, extract numbers
  num <- str_extract(text, "\\d+\\.?\\d*")
  
  if (is.na(num)) return(NA)
  
  value <- as.numeric(num)
  
  # Convert if needed (mm to inches, kg to lbs, etc.)
  if (grepl("mm", text, ignore.case = TRUE) && unit == "in") {
    value <- value / 25.4
  } else if (grepl("kg", text, ignore.case = TRUE) && unit == "lbs") {
    value <- value * 2.20462
  }
  
  return(value)
}

# -----------------------------------------------------------------------------
# USAGE EXAMPLES
# -----------------------------------------------------------------------------

cat("\n🕷️ Vehicle Specs Web Scraper loaded!\n\n")
cat("USAGE:\n\n")
cat("# Test a single vehicle:\n")
cat("result <- scrape_vehicle_specs('Ford', 'F-150', 2024)\n\n")
cat("# Scrape all your target vehicles:\n")
cat("VEHICLES <- tribble(\n")
cat("  ~brand, ~model,\n")
cat("  'Ford', 'F-150',\n")
cat("  'Toyota', 'Camry',\n")
cat("  'Chevrolet', 'Silverado'\n")
cat(")\n")
cat("YEARS <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)\n")
cat("results <- scrape_all_vehicles(VEHICLES, YEARS)\n\n")
cat("NOTES:\n")
cat("• Scraping success rate will vary (expect 30-60%)\n")
cat("• Wikipedia is most reliable but may not have all years\n")
cat("• Always verify scraped data manually\n")
cat("• Be patient - includes delays to be polite to servers\n")
cat("• Some sites may block scrapers or change structure\n\n")