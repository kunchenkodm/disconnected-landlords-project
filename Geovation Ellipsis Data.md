
### Breakdown of the Ellipsis Drive Contents Provided by Geovation:

There are two main data providers: **Land Registry** and **Ordnance Survey (OS)**.

---

#### 1) **Land Registry**

**Navigation**: Land Registry -> National Polygon Service -> Full

There are three datasets in this folder:

1. **<LR_TENURE_FULL_MON_YEAR.csv> - Title Descriptor Dataset**  
   Contains data for each registered estate and interest in England and Wales, including:  
   - Title Number  
   - Estate Interest  
   - Class of Title  
   - Registered Status  
   - Change Indicator  

   [Link to technical specification](https://use-land-property-data.service.gov.uk/datasets/nps/tech-spec/3)

2. **<LR_UPRN_FULL_MON_YEAR.csv> - Title Number and UPRN Lookup Dataset**  
   Contains title numbers from HM Land Registry and Unique Property Reference Numbers (UPRN) from Ordnance Survey’s AddressBase. Use the UPRN to link titles across different datasets.

   [Link to technical specification](https://use-land-property-data.service.gov.uk/datasets/nps/tech-spec/2)

3. **<LR_POLY_FULL_MON_YEAR_X> - National Polygon Dataset** (under "Latest")  
   Contains index polygons showing relative locations and boundaries for registered titles. Use the title number to link across datasets.

   [Link to technical specification](https://use-land-property-data.service.gov.uk/datasets/nps/tech-spec/1)

---

#### 2) **Ordnance Survey (OS)**

There are other datasets in the OS folder, but to date, we have only worked with those in the **National Geographic Database (NGD)** folder.

**Navigation**: OS -> National Geographic Database

The NGD provides datasets on UK building stock, organised into eight themes:
1. **Address**  
2. **Administrative & Statistical Units**  
3. **Buildings**  
4. **Geographical Names**  
5. **Land**  
6. **Land Use**  
7. **Structures**  
8. **Transport**  
9. **Water**

For detailed information on the datasets within each theme, refer to the [OS NGD Documentation](https://docs.os.uk/osngd).

---

#### **Managing GPKG Files**

Most files are in the GPKG format, which can be managed using the `sf` library in R. Below is an example workflow:

```R
library(sf)

# Load and extract Newham geographical boundary data
london_boroughs <- st_read("/path/to/London_Boroughs.gpkg")
newham <- london_boroughs[london_boroughs$name == "Newham", ]

# Load Geovation data (e.g., Buildings)
bld_fts_building <- st_read(
    "/path/to/bld_fts_building.gpkg",
    query = "SELECT * FROM bld_fts_building LIMIT 1000"  # Use for sampling if working locally
)

# Check if datasets share the same coordinate reference system (CRS)
st_crs(newham)
st_crs(bld_fts_building)

# Perform spatial intersection to extract buildings in Newham
newham_buildings <- st_intersection(bld_fts_building, newham)

# Plot the resulting geometry
quartz()  # Opens a new plotting window (macOS specific, use alternatives if needed)
plot(st_geometry(newham_buildings))
```

---

### **Notes on Address Data and Geovation Samples**

1. **Address Theme**:
   - Geovation provides **extracts/sample areas** of OS data within the Address theme.
   - These extracts are limited to specific geographical areas, visible in the **Coverage of Exploration Data** map in the top-level folder of the OS data on Ellipsis.
   - The Address theme includes granular data, such as full residential addresses, but geographical coverage is limited and may not include your area of interest.

2. **Known Issues**:
   - The **Coverage of Exploration Data** polygon provided in the OS folder is not always accurate. Some areas marked as covered may not have data available.
   - This limitation affects detailed extracts from the Address theme. Verify availability before planning analyses for specific areas.

--- 
