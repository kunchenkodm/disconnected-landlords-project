library(readr)
library(data.table)
library(dplyr)
UPRN_dt <- data.table(read_csv("~/RA_local/LR_UPRN_FULL_JAN_2025.csv", 
                               col_names = FALSE))
setnames(UPRN_dt, c("X1", "X2"), c("Title_No","UPRN"))
UPRN_dt <- select(UPRN_dt, -X3)
setkey(UPRN_dt,Title_No)

dt

setDT(combined)
setkey(combined,Title.Number)
tables()
# TODO: FIND PROPERTIES WITH MULTIPLE UPRNs to a Title Number. Add them as separate entities in the combined dayasey. 
# Start with a subset of EPCs (newham-coventry) and match epcs to UPRNS. 
# Drop empty UPRNS.