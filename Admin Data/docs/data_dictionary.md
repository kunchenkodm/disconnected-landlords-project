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
- `treat_for_profit`: For-profit ownership vs private rental
- `treat_uk_for_profit`: UK for-profit vs private rental  
- `treat_foreign_for_profit`: Foreign for-profit vs private rental

### Outcome Variables
- `bad_epc`: Binary indicator for poor energy rating (D, E, F, G)
- `current_energy_rating`: Energy efficiency rating (A-G scale)

### Matching Variables
- `number_habitable_rooms`: Number of rooms available for occupation
- `total_floor_area`: Total floor area in square meters
- `property_type`: House, flat, bungalow, etc.
- `construction_age_band`: Age category of building


