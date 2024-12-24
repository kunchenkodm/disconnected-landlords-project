An overview of some publicy available datasets used in the Project so far:
Note - these tend to be a bit more noisy/messy than Goevation time and take a bit longer to clean

1) CCOD - UK companies that own property in England and Wales

    Technical specification: https://use-land-property-data.service.gov.uk/datasets/ccod/historical/tech-spec

2) OCOD - Overseas companies that own property in England and Wales

    Technical specification: https://use-land-property-data.service.gov.uk/datasets/ocod/tech-spec
    Useful GitHub project on cleaning: https://github.com/JonnoB/inspecting_the_laundromat   

Some further links that also may be useful for when looking into the OCOD and CCOD sets:
    https://whoownsengland.org/2017/11/14/the-companies-corporate-bodies-who-own-a-third-of-england-wales/
    https://github.com/mem48/LandOwnership/blob/main/README.md
    https://www.private-eye.co.uk/registry
    https://github.com/zeph1rus/companies_house_relationship_grapher/blob/main/examples/WesleyPaulWilliamSTREETING.gv.svg 

3) Price Paid Data - https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads

# -----------------------------------------------------------------------------------------------------------

**Using Companies House API**
Company codes listed in the CCOD can be linked to find further information on the ocmpany using ocmpanies house
To analyse the OCOD and CCOD datasets, you will use the Companies House API. Below are key notes for setting it up and using it in code:

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
- Companies House API allows up to **600 requests per 5 minutes**. Implement throttling if working with large datasets to avoid hitting the limit.
- Example of a delay:
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
The API has a limit rate, 600 requests per five-minute period, which can often be hit and not realised if you are submitting to server. To avoid:
- Handle HTTP status codes such as:
  - `200`: Success
  - `429`: Too Many Requests (rate limit exceeded)
  - Other errors: Log and retry if necessary.

Example:

```python
if response.status_code == 429:
    print("Rate limit reached. Waiting...")
    time.sleep(300)  # Wait 5 minutes before retrying
```

Also some direction on this link which to help for further clarification:
https://carmen-aguilar-garcia.medium.com/how-to-use-opencorporates-and-companies-house-apis-79ba0647d0d0