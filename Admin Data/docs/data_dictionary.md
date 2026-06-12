# Data Dictionary

## Raw Data Sources

### Land Registry Data
- **CCOD (Commercial and Corporate Ownership Data)**: Corporate ownership information
- **OCOD (Overseas Companies Ownership Data)**: Foreign company ownership information  
- **UPRN Lookup**: Property reference numbers linking titles to addresses

### EPC Data
- **Certificates**: Energy performance ratings and property characteristics
- **Recommendations**: Energy efficiency improvement suggestions

### Supporting Datasets
- **PPD (Price Paid Data)**: Property transaction prices
- **VOA (Valuation Office Agency)**: Council tax bands
- **LA Lookup**: Local authority to region mappings

## Key Variables

### Treatment Variables
Defined in `scripts/treatment_definitions.R`.
- `treat_for_profit`: For-profit ownership vs private rental
- `treat_uk_for_profit`: UK for-profit vs private rental  
- `treat_foreign_for_profit`: Foreign for-profit vs private rental

### Outcome Variables
- `bad_epc_c`: **Main** bad-EPC indicator — 1 if below EPC C (`current_energy_efficiency < 69`), the **incoming** MEES regulatory bound.
- `bad_epc_e`: Binary, 1 if below EPC E (score < 39) — the **present** regulatory minimum.
- `energy_efficiency_c_gap`: Efficiency-score distance to the C cutoff (`current_energy_efficiency - 68`). Alias of `energy_efficiency_bad_epc_gap`.
- `energy_efficiency_e_gap`: Efficiency-score distance to the E cutoff (`current_energy_efficiency - 38`).
- `borderline_good_epc_c`: Binary, 1 if just above the C cutoff (bunching at incoming MEES bound). Alias of `borderline_good_epc`.
- `borderline_good_epc_e`: Binary, 1 if just above the E cutoff (bunching at present regulatory minimum).
- `current_energy_rating`: Energy efficiency rating (A-G scale)

### Matching Variables
- `number_habitable_rooms`: Number of rooms available for occupation
- `total_floor_area`: Total floor area in square meters
- `property_type`: House, flat, bungalow, etc.
- `construction_age_band`: Age category of building


