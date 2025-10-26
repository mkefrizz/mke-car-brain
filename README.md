# Milwaukee Car Brain

Quantitative research to inform local officials about the costs and issues of auto-dependence in Milwaukee.

## Project Overview

This book examines auto-dependence from multiple angles:
- Costs to people, local government, and society
- Vehicle size trends and cascading impacts
- Milwaukee's unique "danger zone" density (~6,000 people/sq mi)

## Quick Start

```r
# Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Milwaukee Car Brain Project")

# Load the main workflow
source("scripts/vehicle_data_collector.R")

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
- Model year 2022: 81% of new vehicles were "trucks" (SUVs, pickups)

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
- "The Geometry Problem" - postsuburban.substack.com
- Robert Putnam's "Bowling Alone"

## Repository

https://github.com/mkefrizz/mke-car-brain.git

