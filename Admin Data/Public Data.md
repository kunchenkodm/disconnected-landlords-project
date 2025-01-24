# Overview of Publicly Available Datasets

An overview of some publicly available datasets used in the project so far:

> **Note**: These datasets tend to be noisier and messier than Geovation data, requiring more time and effort to clean.

---

## Key Datasets

### 1. **CCOD** - UK Companies that Own Property in England and Wales
- [Technical Specification](https://use-land-property-data.service.gov.uk/datasets/ccod/historical/tech-spec)

### 2. **OCOD** - Overseas Companies that Own Property in England and Wales
- [Technical Specification](https://use-land-property-data.service.gov.uk/datasets/ocod/tech-spec)
- Useful GitHub Project on Cleaning:  
  [Inspecting the Laundromat](https://github.com/JonnoB/inspecting_the_laundromat)

### 3. **Price Paid Data**
- [Price Paid Data Download](https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads)

### 4. **Energy Performance of Buildings Data**
- [EPC Data Download](https://epc.opendatacommunities.org/domestic/search)
- [EPC Documentation](https://epc.opendatacommunities.org/docs/index)
---

## Additional Resources

Below are links that may be useful when working with the **OCOD** and **CCOD** datasets:

- [The Companies & Corporate Bodies Who Own England & Wales](https://whoownsengland.org/2017/11/14/the-companies-corporate-bodies-who-own-a-third-of-england-wales/)
- [LandOwnership GitHub Project](https://github.com/mem48/LandOwnership/blob/main/README.md)
- [Private Eye Registry](https://www.private-eye.co.uk/registry)
- [Companies House Relationship Grapher](https://github.com/zeph1rus/companies_house_relationship_grapher/blob/main/examples/WesleyPaulWilliamSTREETING.gv.svg)

---

# Using the Companies House API

The **Companies House API** allows you to retrieve additional information about company codes listed in the CCOD or OCOD datasets.

### 1. **Setting Up**
- Obtain an API key by signing up for a Companies House Developer Account: [https://developer.companieshouse.gov.uk](https://developer.companieshouse.gov.uk).

### 2. **API Integration in Code**
- Use the `requests` library in Python to make API calls.
- Base URL: `https://api.company-information.service.gov.uk/`.
- Authentication: Use Basic Auth, where the API key is the username, and the password can be left blank.

Example code:

```python
import requests

API_KEY = "your_api_key_here"
BASE_URL = "https://api.company-information.service.gov.uk/"

def get_company_info(company_number):
    url = f"{BASE_URL}company/{company_number}"
    response = requests.get(url, auth=(API_KEY, ''))
    if response.status_code == 200:
        return response.json()
    else:
        return {"error": response.status_code, "message": response.text}
```

### 3. **Handling Rate Limits**
- Companies House API allows up to 600 requests per 5 minutes. It is recommended to implement throttling techniques (limiting how often a function is called or how much data is transmitted within a specific time period) when working with large datasets to avoid exceeding this limit.

Example code:
```python
import time

# Delay to avoid exceeding rate limit
time.sleep(0.5)  # 0.5 seconds per request = 120 requests per minute
```

### 4. **Example uses in Datasets**
- Extract unique company codes from the OCOD or CCOD datasets.
- Validate company codes (e.g., remove invalid or padded numbers).
- Use the API to retrieve Person with Significant Control (PSC) information for each company.
- Saved results to a CSV file for further analysis.

### 5. **Error Handling**
The API has a limit rate, 600 requests per five-minute period, which can often be hit and not realised if you are submitting to server. To handle this error, identified by status code 429 (Too Many Requests), implement a retry mechanism with a delay to comply with the API’s rate limits. For example, you can pause the script for the required duration before resending the request:

Example code:

```python
if response.status_code == 429:
    print("Rate limit reached. Waiting...")
    time.sleep(300)  # Wait 5 minutes before retrying
```

Also some direction on this link which to help for further clarification:
https://carmen-aguilar-garcia.medium.com/how-to-use-opencorporates-and-companies-house-apis-79ba0647d0d0
