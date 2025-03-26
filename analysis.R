library(fixest)

EPC_matched_combined[is.na(source), source := "Unknown"]
EPC_matched_combined[is.na(tenure), tenure := "Not in OCOD, CCOD"]
EPC_matched_combined[, postcode_area := sub(" .*", "", POSTCODE)]
EPC_matched_combined[, postcode_sector := sub("^([^ ]+ [A-Z0-9]).*", "\\1", POSTCODE)]
EPC_matched_combined[, concatenation := paste0(PROPERTY_TYPE,TENURE)]

EPC_matched_combined[, source_factor := relevel(factor(source), ref = "CCOD")]

EPC_matched_combined[, source_factor := relevel(factor(source), ref = "Unknown")]

model1 <- feols(bad_EPC ~ source_factor | PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster = ~postcode_sector)
model1
linearHypothesis(model1, "source_factorCCOD = source_factorOCOD", white.adjust = "hc1")



feols(bad_EPC ~ source_factor | concatenation + postcode_sector, data = EPC_matched_combined, cluster = ~postcode_sector)


table(EPC_matched_combined$EPC_bad, EPC_matched_combined$source, EPC_matched_combined$tenure, useNA = "ifany")

feols(bad_EPC~factor(source), data = EPC_matched_combined, cluster=~postcode_short)
feols(bad_EPC~factor(relevel(source, ref="Unknown"))| PROPERTY_TYPE + TENURE, data = EPC_matched_combined, cluster=~postcode_sector)

feols(bad_EPC~factor(source)+tenure| PROPERTY_TYPE+TENURE, data = EPC_matched_combined, cluster=~postcode_sector)
