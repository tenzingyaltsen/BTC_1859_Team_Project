library(dplyr)

# Make a dataset for sleep disturbance predictor ESS and quality of life response variable SF36.PCS
ess_pcs <- subset(clean_data2,
  select = c(ESS, SF36.PCS))

ess_pcs_0 <- ess_pcs %>%
  filter(ESS == 0)

ess_pcs_1 <- ess_pcs %>%
  filter(ESS == 1)

t.test(ess_pcs_0$SF36.PCS, ess_pcs_1$SF36.PCS)


