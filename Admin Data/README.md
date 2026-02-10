# Disconnected Landlords: Energy Efficiency Analysis

## Overview
Analysis of energy efficiency in rental properties by ownership type using EPC and Land Registry data.

## Quick Start
1. Clone or download this repository
2. Install R (>= 4.0.0) and RStudio
3. Open `disconnected-landlords.Rproj` in RStudio
4. Install required packages: `renv::restore()`
5. Update configuration in `00_setup.R`, `treatment_definitions.R`, `model_specifications.R` if needed
6. Run the analysis: `source("scripts/run_all.R")` or run scripts one by one. 

## Project Structure
- `scripts/`: R analysis scripts (run in numerical order). Shared treatment variables are defined in `scripts/treatment_definitions.R`
- `data/`: Data storage (raw, processed, sample data)
- `output/`: Analysis outputs (tables, figures, matched datasets)
- `docs/`: Documentation

## Data Requirements
This project requires access to:
- Land Registry CCOD/OCOD data (API key required)
- EPC (Energy Performance Certificate) data
- Property price paid data (PPD)
- Council tax valuation data (VOA)

See `docs/data_dictionary.md` for detailed data descriptions.

## Configuration
Edit `00_setup.R` to adjust:
- Dataset versions
- Sample sizes for testing
- Analysis parameters

Edit `treatment_definitions.R` to adjust:
- treatment definitions from dataset

Edit `model_specifications.R` to adjust:
- OLS and PSM model specifications (variables used, dataset cores, etc.)

## License
[Add your license information here]

## Citation
[Add citation information for academic use]

