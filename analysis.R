library(fixest)
library(car)
library(httr)
library(jsonlite)
library(data.table)


load("~/disconnected-landlords-project/EPC_matched_combined.RData")



# Baseline models 
EPC_matched_combined[, source := relevel(factor(source), ref = "Unknown")]
model1 <- feols(bad_EPC ~ source | PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster = ~postcode_sector)
model1
linearHypothesis(model1, "sourceCCOD = sourceOCOD", white.adjust = "hc1")

EPC_matched_combined[, source := relevel(factor(source), ref = "CCOD")]
model2 <- feols(bad_EPC ~ source | PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster = ~postcode_sector)
model2

feols(bad_EPC ~ source | concatenation + postcode_sector, data = EPC_matched_combined, cluster = ~postcode_sector)
table(EPC_matched_combined$concatenation)


# feols(bad_EPC~factor(source), data = EPC_matched_combined, cluster=~postcode_short)
# feols(bad_EPC~factor(relevel(source, ref="Unknown"))| PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster=~postcode_sector)
# 
# feols(bad_EPC~factor(source)+tenure| PROPERTY_TYPE+TENURE, data = EPC_matched_combined, cluster=~postcode_sector)

## Tenure
model_tenure <- feols(bad_EPC ~ source + tenure | PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster = ~postcode_sector)
model_tenure

## Duplicate Listings

model_duplicates <- feols(bad_EPC ~ source + has_duplicates | PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster = ~postcode_sector)
model_duplicates





#### Replicating WP654
### Continuous property characteristic

cont_chars <- c("TOTAL_FLOOR_AREA", "NUMBER_HABITABLE_ROOMS")  
model00 <- feols(bad_EPC ~
                   source 
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                   | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + TENURE
                   , data = EPC_matched_combined, cluster = ~postcode_sector)
model00

model01 <- feols(bad_EPC ~
                   source 
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                 | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + BUILT_FORM + TENURE + MAIN_FUEL # + is listed?
                 , data = EPC_matched_combined, cluster = ~postcode_sector)
model01

model02 <- feols(bad_EPC ~
                   source 
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                 | CONSTRUCTION_AGE_BAND^PROPERTY_TYPE^BUILT_FORM^TENURE^MAIN_FUEL # Getting main fuel to interact -> run out of memory + is listed?
                 , data = EPC_matched_combined, cluster = ~postcode_sector)
model02
feols(bad_EPC ~
        source 
      + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
      | CONSTRUCTION_AGE_BAND^PROPERTY_TYPE^BUILT_FORM^TENURE # Getting main fuel to interact -> run out of memory + is listed?
      , data = EPC_matched_combined, cluster = ~postcode_sector)

model03 <- feols(bad_EPC ~
                   source 
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                 | CONSTRUCTION_AGE_BAND^PROPERTY_TYPE^BUILT_FORM^TENURE^MAIN_FUEL + POSTCODE # Getting main fuel to interact -> run out of memory + is listed?
                 , data = EPC_matched_combined, cluster = ~postcode_sector)
model03

model01a <- feols(bad_EPC ~
                   source + proprietorship_category_1
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                 | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + BUILT_FORM + TENURE + MAIN_FUEL # + is listed?
                 , data = EPC_matched_combined, cluster = ~postcode_sector)
model01a
model01b <- feols(bad_EPC ~
                    source + proprietorship_category_1
                  | PROPERTY_TYPE + TENURE # + is listed?
                  , data = EPC_matched_combined, cluster = ~postcode_sector)
model01b


model_countries <- feols(bad_EPC ~
                    relevel(factor(country_incorporated_1), ref = "UNITED KINGDOM")+ proprietorship_category_1
                  + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + lodgement_year
                  | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + BUILT_FORM + TENURE  # + is listed?
                  , data = EPC_matched_combined, cluster = ~postcode_sector)
model_countries

cont_chars <- c("TOTAL_FLOOR_AREA", "NUMBER_HABITABLE_ROOMS")  
model_intlodg <- feols(bad_EPC ~
                   source*lodgement_year
                 + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS 
                 | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + TENURE
                 , data = EPC_matched_combined, cluster = ~postcode_sector)
model_intlodg

model_intint <- feols(bad_EPC ~
                         source*lodgement_year*TENURE
                       + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS 
                       | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE
                       , data = EPC_matched_combined, cluster = ~postcode_sector)
model_intint



model_intlodtax <- feols(bad_EPC ~
                           source*country_incorporated_tax_haven
                         + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS 
                         | CONSTRUCTION_AGE_BAND + PROPERTY_TYPE + TENURE
                         , data = EPC_matched_combined, cluster = ~postcode_sector)

# load("C:/Users/Kunch/Documents/RA_local/Postcode_Level/el_postcode.rdata")
# load("C:/Users/Kunch/Documents/RA_local/Postcode_Level/gas_postcode.rdata")













## 
