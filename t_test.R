library(dplyr)

# H0: μ1 = μ2 
# Where μ1 is the mean QOL of not sleep disturbed individuals and μ2 is the mean QOL of sleep disturbed individuals
# H1: μ1 ≠ μ2

# Make a dataset for sleep disturbance predictor ESS and quality of life response variable SF36.PCS
ess_pcs <- subset(clean_data2,
  select = c(ESS, SF36.PCS))

# Filter for not sleep disturbance (0)
ess_pcs_0 <- ess_pcs %>%
  filter(ESS == 0)

# Filter for sleep disturbance (1)
ess_pcs_1 <- ess_pcs %>%
  filter(ESS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.PCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(ess_pcs_0$SF36.PCS, ess_pcs_1$SF36.PCS)


# Make a dataset for sleep disturbance predictor ESS and quality of life response variable SF36.MCS
ess_mcs <- subset(clean_data2,
                  select = c(ESS, SF36.MCS))

# Filter for not sleep disturbance (0)
ess_mcs_0 <- ess_mcs %>%
  filter(ESS == 0)

# Filter for sleep disturbance (1)
ess_mcs_1 <- ess_mcs %>%
  filter(ESS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.MCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(ess_mcs_0$SF36.MCS, ess_mcs_1$SF36.MCS)

# Make a dataset for sleep disturbance predictor PSQIS and quality of life response variable SF36.PCS
psqis_pcs <- subset(clean_data2,
                  select = c(PSQIS, SF36.PCS))

# Filter for not sleep disturbance (0)
psqis_pcs_0 <- psqis_pcs %>%
  filter(PSQIS == 0)

# Filter for sleep disturbance (1)
psqis_pcs_1 <- psqis_pcs %>%
  filter(PSQIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.PCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(psqis_pcs_0$SF36.PCS, psqis_pcs_1$SF36.PCS)


# Make a dataset for sleep disturbance predictor PSQIS and quality of life response variable SF36.MCS
psqis_mcs <- subset(clean_data2,
                  select = c(PSQIS, SF36.MCS))

# Filter for not sleep disturbance (0)
psqis_mcs_0 <- psqis_mcs %>%
  filter(PSQIS == 0)

# Filter for sleep disturbance (1)
psqis_mcs_1 <- psqis_mcs %>%
  filter(PSQIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.MCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(psqis_mcs_0$SF36.MCS, psqis_mcs_1$SF36.MCS)


# Make a dataset for sleep disturbance predictor PSQIS and quality of life response variable SF36.PCS
psqis_pcs <- subset(clean_data2,
                  select = c(PSQIS, SF36.PCS))

# Filter for not sleep disturbance (0)
psqis_pcs_0 <- psqis_pcs %>%
  filter(PSQIS == 0)

# Filter for sleep disturbance (1)
psqis_pcs_1 <- psqis_pcs %>%
  filter(PSQIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.PCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(psqis_pcs_0$SF36.PCS, psqis_pcs_1$SF36.PCS)


# Make a dataset for sleep disturbance predictor PSQIS and quality of life response variable SF36.MCS
psqis_mcs <- subset(clean_data2,
                  select = c(PSQIS, SF36.MCS))

# Filter for not sleep disturbance (0)
psqis_mcs_0 <- psqis_mcs %>%
  filter(PSQIS == 0)

# Filter for sleep disturbance (1)
psqis_mcs_1 <- psqis_mcs %>%
  filter(PSQIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.MCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(psqis_mcs_0$SF36.MCS, psqis_mcs_1$SF36.MCS)

# Make a dataset for sleep disturbance predictor AIS and quality of life response variable SF36.PCS
ais_pcs <- subset(clean_data2,
                    select = c(AIS, SF36.PCS))

# Filter for not sleep disturbance (0)
ais_pcs_0 <- ais_pcs %>%
  filter(AIS == 0)

# Filter for sleep disturbance (1)
ais_pcs_1 <- ais_pcs %>%
  filter(AIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.PCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(ais_pcs_0$SF36.PCS, ais_pcs_1$SF36.PCS)

# Make a dataset for sleep disturbance predictor AIS and quality of life response variable SF36.MCS
ais_mcs <- subset(clean_data2,
                    select = c(AIS, SF36.MCS))

# Filter for not sleep disturbance (0)
ais_mcs_0 <- ais_mcs %>%
  filter(AIS == 0)

# Filter for sleep disturbance (1)
ais_mcs_1 <- ais_mcs %>%
  filter(AIS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.MCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(ais_mcs_0$SF36.MCS, ais_mcs_1$SF36.MCS)

# Make a dataset for sleep disturbance predictor BSS and quality of life response variable SF36.PCS
bss_pcs <- subset(clean_data2,
                  select = c(BSS, SF36.PCS))

# Filter for not sleep disturbance (0)
bss_pcs_0 <- bss_pcs %>%
  filter(BSS == 0)

# Filter for sleep disturbance (1)
bss_pcs_1 <- bss_pcs %>%
  filter(BSS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.PCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(bss_pcs_0$SF36.PCS, bss_pcs_1$SF36.PCS)


# Make a dataset for sleep disturbance predictor BSS and quality of life response variable SF36.MCS
bss_mcs <- subset(clean_data2,
                  select = c(BSS, SF36.MCS))

# Filter for not sleep disturbance (0)
bss_mcs_0 <- bss_mcs %>%
  filter(BSS == 0)

# Filter for sleep disturbance (1)
bss_mcs_1 <- bss_mcs %>%
  filter(BSS == 1)

# Run a two sample t.test to compare the mean of quality of life (SF36.MCS) from a sleep disturbed and with and without sleep disturbance samples
t.test(bss_mcs_0$SF36.MCS, bss_mcs_1$SF36.MCS)

# All sleep disturbance scales, have a p value < the significance level (0.05), as evidence suggests the mean of quality of 
#life for individuals with and without sleep disturbance are not the same.
# Evidence suggests there is a significant difference between the mean QOL of individuals experience sleep disturbance and those who do not 
# experience sleep disturbance.
