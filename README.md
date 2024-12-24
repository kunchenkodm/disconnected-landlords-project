# disconnected-landlords-project

## Overview
The **Admin Stream** work to date focuses on detailed property ownership analysis, including corporate and public sector entities. It builds upon datasets enriched with additional metadata, such as Persons of Significant Control (PSC) information, to identify potential subsidiary holdings and local authority involvement.

---

## Key Files and Folders

1. **`Geovation Ellipsis Data`**:
   - Text files that describe data sourced from Geovation, providing insights into land and property ownership. Includes notes and examples for working with these datasets.

2. **`Public Data`**:
   - Text files describing publicly available datasets. These include details on the datasets and sample code to demonstrate how to process and analyse them.

3. **`company_codes_and_psc_info.csv.zip`**:
   - A zipped CSV containing a list of company registration numbers from CCOD along with their associated PSC (Person of Significant Control) information.
   - Useful for investigating corporate subsidiary holdings (including but not limited to local authority-related entities).

4. **`Server Access Notes.txt`**:
   - Documentation for accessing and utilising the Warwick SCRTP Linux server, useful for large-scale data processing.

---

## Data Integration and Output

Following cleaning the CCOD and using Companies House to fetch the PSC information, companies that are local authority subsidiaries were identified. This information has been linked to the **Title Descriptor dataset** (November 2024 version), creating a supplementary column for **LA Ownership** (whether direct or subsidiary). 

The amended dataset is available for download:  
[Google Drive Link](https://drive.google.com/file/d/1Wnsl13eZQJezvWN4BpablvTncqAax5eo/view?usp=sharing)

