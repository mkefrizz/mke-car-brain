# =============================================================================
# MILWAUKEE CAR BRAIN - PROJECT SETUP SCRIPT
# =============================================================================
# Run this ONCE to set up your project directory structure
# =============================================================================

# Set your project directory (iCloud Drive)
PROJECT_DIR <- "~/Library/Mobile Documents/com~apple~CloudDocs/Milwaukee Car Brain Project"

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  MILWAUKEE CAR BRAIN - PROJECT SETUP\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Check if directory exists
if (!dir.exists(PROJECT_DIR)) {
  cat("⚠ Project directory not found at:\n")
  cat(glue("  {PROJECT_DIR}\n\n"))
  cat("Creating it now...\n")
  dir.create(PROJECT_DIR, recursive = TRUE)
}

# Set working directory
setwd(PROJECT_DIR)
cat(glue("✓ Working directory set to:\n  {getwd()}\n\n"))

# Create directory structure
cat("Creating directory structure...\n")

dirs_to_create <- c(
  "data",
  "data/raw",
  "data/processed",
  "output",
  "output/visualizations",
  "docs",
  "scripts"
)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(glue("  ✓ Created: {dir}/\n"))
  } else {
    cat(glue("  ✓ Exists: {dir}/\n"))
  }
}

cat("\n")

# Create .gitignore if it doesn't exist
gitignore_path <- ".gitignore"
if (!file.exists(gitignore_path)) {
  cat("Creating .gitignore file...\n")
  
  gitignore_content <- "# R specific
.Rproj.user
.Rhistory
.RData
.Ruserdata
*.Rproj

# Data files (keep raw data out of Git)
data/raw/*.csv
data/raw/*.zip
data/raw/*.rds

# Large EPA files
data/raw/vehicles.csv
data/raw/epa_vehicles_all.csv

# Excel data files
# UNCOMMENT these lines if you want to backup your collected data in Git:
# data/vehicle_data_entry.xlsx
# data/vehicle_data_entry.csv

# Output files
output/visualizations/*.png
output/visualizations/*.pdf
output/*.html

# System files
.DS_Store
Thumbs.db
*.swp
*.swo
*~

# Temporary files
*.tmp
*.bak
*.log

# macOS
.DS_Store
.AppleDouble
.LSOverride

# iCloud
*.icloud
"
  
  writeLines(gitignore_content, gitignore_path)
  cat("  ✓ Created: .gitignore\n\n")
} else {
  cat("  ✓ .gitignore already exists\n\n")
}

# Create README if it doesn't exist
readme_path <- "README.md"
if (!file.exists(readme_path)) {
  cat("Creating README.md file...\n")
  
  readme_content <- "# Milwaukee Car Brain

Quantitative research to inform local officials about the costs and issues of auto-dependence in Milwaukee.

## Project Overview

This book examines auto-dependence from multiple angles:
- Costs to people, local government, and society
- Vehicle size trends and cascading impacts
- Milwaukee's unique \"danger zone\" density (~6,000 people/sq mi)

## Quick Start

```r
# Set working directory
setwd(\"~/Library/Mobile Documents/com~apple~CloudDocs/Milwaukee Car Brain Project\")

# Load the main workflow
source(\"scripts/vehicle_data_collector.R\")

# Run initial setup
template <- run_full_workflow()

# After collecting data, run analysis
results <- run_analysis_pipeline()
```

## Project Structure

```
Milwaukee Car Brain Project/
├── data/
│   ├── raw/              # Raw data downloads (not in Git)
│   ├── processed/        # Cleaned data
│   └── vehicle_data_entry.xlsx  # Main data collection
├── output/
│   └── visualizations/   # Generated charts
├── scripts/
│   └── vehicle_data_collector.R  # Main R workflow
├── docs/                 # Documentation and notes
└── README.md
```

## Data Collection Target

**9 Vehicles** across **8 Years** (1990-2024):
- Ford: Taurus, Explorer, F-150
- Toyota: Camry, RAV4, Tundra
- Chevrolet: Malibu, Equinox, Silverado

## Key Findings

- Milwaukee city density: ~6,000 people/sq mi (critical threshold)
- Vehicle footprint increased 6% since 2008
- Model year 2022: 81% of new vehicles were \"trucks\" (SUVs, pickups)

## Book Structure

1. What is Car Brain?
2. How Milwaukee Got Here
3. The Vehicle Size Spiral ← *Current focus*
4. Personal Cascades
5. Municipal Cascades
6. Social Cascades
7. The Evidence for Change
8. Tools for Elected Officials

## Sources

- EPA Automotive Trends Reports
- \"The Geometry Problem\" - postsuburban.substack.com
- Robert Putnam's \"Bowling Alone\"

## Repository

https://github.com/mkefrizz/mke-car-brain.git
"
  
  writeLines(readme_content, readme_path)
  cat("  ✓ Created: README.md\n\n")
} else {
  cat("  ✓ README.md already exists\n\n")
}

# Check if vehicle_data_collector.R exists, if not guide user
collector_path <- "scripts/vehicle_data_collector.R"
if (!file.exists(collector_path)) {
  cat("⚠ vehicle_data_collector.R not found in scripts/\n")
  cat("\nNext steps:\n")
  cat("  1. Save your vehicle_data_collector.R to:\n")
  cat(glue("     {PROJECT_DIR}/scripts/vehicle_data_collector.R\n"))
  cat("  2. Then run: source('scripts/vehicle_data_collector.R')\n\n")
} else {
  cat("✓ vehicle_data_collector.R found in scripts/\n\n")
}

# Initialize Git if not already done
if (!dir.exists(".git")) {
  cat("Git repository not initialized.\n")
  cat("\nTo initialize Git, run these commands in Terminal:\n")
  cat(glue("  cd '{PROJECT_DIR}'\n"))
  cat("  git init\n")
  cat("  git remote add origin https://github.com/mkefrizz/mke-car-brain.git\n")
  cat("  git add .\n")
  cat("  git commit -m 'Initial commit: Project setup'\n")
  cat("  git branch -M main\n")
  cat("  git push -u origin main\n\n")
} else {
  cat("✓ Git repository already initialized\n\n")
}

cat("═══════════════════════════════════════════════════════════════\n")
cat("✓ PROJECT SETUP COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Your project structure:\n")
cat(glue("  {PROJECT_DIR}/\n"))
cat("  ├── data/\n")
cat("  │   ├── raw/\n")
cat("  │   └── processed/\n")
cat("  ├── output/\n")
cat("  │   └── visualizations/\n")
cat("  ├── scripts/\n")
cat("  ├── docs/\n")
cat("  ├── .gitignore\n")
cat("  └── README.md\n\n")

cat("Next steps:\n")
cat("  1. Move vehicle_data_collector.R to scripts/ folder\n")
cat("  2. Run: source('scripts/vehicle_data_collector.R')\n")
cat("  3. Initialize Git (see commands above)\n")
cat("  4. Start collecting vehicle data!\n\n")

# Print current directory contents
cat("Current directory contents:\n")
print(list.files(all.files = TRUE))
cat("\n")
