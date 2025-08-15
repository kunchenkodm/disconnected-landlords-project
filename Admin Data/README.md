# Disconnected Landlords: Energy Efficiency Analysis

## Overview
Analysis of energy efficiency in rental properties by ownership type using EPC and Land Registry data.

## Quick Start
1. Clone or download this repository
2. Install R (>= 4.0.0) and RStudio
3. Open `disconnected-landlords.Rproj` in RStudio
4. Install required packages: `renv::restore()`
5. Add your API credentials in  `.Renviron` 
6. Update configuration in `config/config.yml` if needed
7. Run the analysis: `source("scripts/run_all.R")`

## Project Structure
- `scripts/`: R analysis scripts (run in numerical order)
- `data/`: Data storage (raw, processed, sample data)
- `output/`: Analysis outputs (tables, figures, matched datasets)
- `config/`: Configuration files
- `functions/`: Reusable R functions
- `docs/`: Documentation
- `tests/`: Data validation and tests

## Data Requirements
This project requires access to:
- Land Registry CCOD/OCOD data (API key required)
- EPC (Energy Performance Certificate) data
- Property price paid data (PPD)
- Council tax valuation data (VOA)

See `docs/data_dictionary.md` for detailed data descriptions.

## Configuration
Edit `config/config.yml` to adjust:
- Dataset versions
- Sample sizes for testing
- Analysis parameters

## License
[Add your license information here]

## Citation
[Add citation information for academic use]

