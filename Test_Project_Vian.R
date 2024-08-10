#load and attach add-on packages, install them if necessary.
library(dplyr)
library(mice)
library(car)
library(MASS)
library(funModeling)
library(ggplot2)

# Import data.
raw_data <- read.csv("project_data.csv")
# View(raw_data)

# Explore data.
summary(raw_data)
str(raw_data)
dim(raw_data)
names(raw_data)

# Extract variables of interest.
working_data <- raw_data[,c(1,2,3,9,16,18,19,20,24,42,46,58,70,75,77,85,86,87)]

# Rename variables.
working_data1 <- working_data %>%
  rename(TFT = "Time.from.transplant", 
         Liver.diag = "Liver.Diagnosis",
         RoD = "Recurrence.of.disease",
         RGD = "Rejection.graft.dysfunction",
         Any.fibro = "Any.fibrosis",
         Renal.fail = "Renal.Failure",
         ESS = "Epworth.Sleepiness.Scale",
         PSQIS = "Pittsburgh.Sleep.Quality.Index.Score",
         AIS = "Athens.Insomnia.Scale",
         BSS = "Berlin.Sleepiness.Scale")

glimpse(working_data1)
freq(working_data1)
# plot_num generates all plots in one
plot_num(working_data1)
# Create a function that extracts descriptive statistics for a variable and
# displays them in a graph
descriptive <- function(var, data){
  for (i in var) {
    var_data <- data[[i]]
    unique_values <- length(unique(var_data))
    # Set a threshold to determine whether to treat the variable as categorical
    # or continuous
    if (unique_values < 10) {
      cat(paste0("\nFrequency counts for ", i, ":\n"))
      print(table(var_data))
      graph <- ggplot(data, aes_string(x = i)) +
        geom_bar(col = "black", fill = "lightblue") +
        labs(title = paste0("Distribution of ", i), x = i, y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      print(graph)
    } else {
      cat(paste0("\nSummary Statistics for ", i, ":\n"))
      print(summary(var_data))
      cat(paste0("\nStandard deviation of ", i, ":\n"))
      print(sd(var_data, na.rm = T))
      mean_var <- mean(var_data, na.rm = T)
      graph <- ggplot(data, aes_string(x = i)) +
        geom_histogram(binwidth = 1, col = "black", fill = "lightblue") +
        geom_vline(aes_string(xintercept = mean_var), 
                   color="blue", linetype="dashed", size=1) +
        labs(title = paste0("Distribution of ", i), x = i, y = "Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      print(graph)
    }
  }
}

# Specify the variables needed to conduct summary statistics
vars <- names(working_data1)
# Conduct descriptive analysis
descriptive(vars, working_data1)
describe(working_data1)

# There are NA's found in the dataset. Check if there are any empty strings.
which(working_data1 == "")
# No blanks in data set.

# Count the number of NA's in the dataset.
apply(is.na(working_data1),2,sum) 

# Check the number of 0's and NA's, and its respective frequencies
status(working_data1)

# Create a new dataframe with the variables and create a new column indicating
# 0 if PSQI is not missing, and 1 if PSQI is missing.
NA_check_data <- raw_data %>%
  mutate(missing_PSQI = 
           ifelse(is.na(Pittsburgh.Sleep.Quality.Index.Score), 1, 0))

NA_check_data <- NA_check_data[,c(2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 95)]
NA_check_data <- NA_check_data %>%
  mutate(across(c("Gender", "Employment", "Marital.Status", "Education", "Ethnicity",
                  "Household.income", "missing_PSQI"), as.factor))

# Fit a logistic regression model to see if missingness of PSQI is related to 
# observed variables
PSQI_NA_model <- glm(missing_PSQI ~., 
                     data = NA_check_data, family = binomial)
summary(PSQI_NA_model)

# Imputation with stochastic regression.
imp_stoch <- mice(working_data1, method = "norm.nob", seed = 32,
                  m = 1, print = FALSE)
any(is.na(imp_stoch))
summary(imp_stoch)
# Plot the newly imputed data. 
xyplot(imp_stoch, PSQIS ~ Age)
xyplot(imp_stoch, Age ~ Subject)
xyplot(imp_stoch, BMI ~ Age)
xyplot(imp_stoch, ESS ~ Age)
xyplot(imp_stoch, AIS ~ Age)
xyplot(imp_stoch, BSS ~ Age) # >0.5 will count as 1 and <0.5 will count as 0
xyplot(imp_stoch, SF36.PCS ~ Age)
xyplot(imp_stoch, SF36.MCS ~ Age)

# Convert from mids to dataframe.
imp_stoch_df <- complete(imp_stoch)

# Convert sleep disturbance measurements to binary.
clean_data1 <- imp_stoch_df %>%
  mutate(ESS = ifelse(ESS > 10, 1, 0)) %>%
  mutate(PSQIS = ifelse(PSQIS > 4, 1, 0)) %>%
  mutate(AIS = ifelse(AIS > 5, 1, 0)) %>%
  # 4 of the imputed variables are above 0.5; 2 of the imputed variables are
  # below 0.5
  mutate(BSS = ifelse(BSS > 0.5, 1, 0))

# Change categorical variables to factors.
clean_data2 <- clean_data1 %>% 
  mutate(across(c("Gender", "Liver.diag", "RoD", "RGD", "Any.fibro",
                  "Renal.fail", "Depression", "Corticoid", "ESS", 
                  "PSQIS", "AIS", "BSS"), as.factor))

# Estimate prevalence of sleep disturbance
# Create a function that estimates prevalence of sleep disturbance for each scale
disturbed_sleep_preval <- function(scale, data) {
  for (i in scale) {
    freq <- table(data[[i]])
    prop <- prop.table(freq)
    print(i)
    print(prop)
    print(paste0("Prevalence of sleep disturbance according to ", 
                 i, ": ", prop["1"]))
    
  }
}

# Specify scales needed to estimate prevalence of sleep disturbance.
scales <- c("ESS", "PSQIS", "AIS", "BSS")
# Calculate prevalence of sleep disturbance
disturbed_sleep_preval(scales, clean_data2)

# Calculate the prevalence if they received a 1 (sleep disturbance) in at least one of the questionnaires.
prev_any <- (length(which(clean_data2$ESS == 1 | clean_data2$PSQIS == 1 |
                            clean_data2$AIS == 1 | clean_data2$BSS == 1))) / nrow(clean_data2)
print(paste0("Prevalence of sleep disturbance according to any questionnaire: ", prev_any))

# Based on literature, these predictor, have the greatest influence on sleep disturbance. 
# Corticosteroid (1), depression (1), BMI (1), liver diagnosis (4), 
# graft dysfunction/rejection (1), fibrosis (1), renal failure (1), gender (1)
# There is a total of 11 degree of freedom (df) from the predictors above.


########################################################
#                                                      #
# Create logistic regression model for ESS as response.#
#                                                      #
########################################################

#' Manually create a model to predict ESS without using step-wise function.
#' Create starting data set with pool of variables to start with, based 
#' on literature.
clean_data_ess_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, 
                                        TFT, ESS))

table(clean_data2$ESS)
#' There are 74 participants who experience sleep disturbance 
#' according to ESS and 194 participants who do not. Using p < m/10:
74/10
# 7 predictors/degrees of freedom that can be used in the ESS model.

# Create our full model based on above variables selected.
ess_glm_mod_full <- glm(ESS ~., data = clean_data_ess_model, family="binomial")
#' Find which variable to remove using highest p-value from summary(). Also
#' check collinearity.
summary(ess_glm_mod_full)
vif(ess_glm_mod_full)
# All values are less than 5, passes collinearity check.

# Remove Renal.fail since it has the highest p-value.
ess_glm_mod_1 <- glm(ESS ~ Corticoid + Depression + BMI + Liver.diag + 
                            RGD + Any.fibro + Gender + TFT, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_1)
# Compare this model without Renal.fail with the full model.
AIC(ess_glm_mod_full)
AIC(ess_glm_mod_1)
anova(ess_glm_mod_1,ess_glm_mod_full, test = "Chisq")
# Smaller model is favoured by ANOVA, though having a slightly higher AIC.

# Remove BMI as it had the next highest p-value.
ess_glm_mod_2 <- glm(ESS ~ Corticoid + Depression + Liver.diag + 
                            RGD + Any.fibro + Gender +TFT, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_2)
# Compare this model without BMI with the previous model.
AIC(ess_glm_mod_1)
AIC(ess_glm_mod_2)
anova(ess_glm_mod_2, ess_glm_mod_1, test = "Chisq")

# AIC and ANOVA showed that the simpler model is better. Remove TFT as a
# variable from the model.
ess_glm_mod_3 <- glm(ESS ~ Corticoid + Depression + Liver.diag + Any.fibro +
                            RGD + Gender, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_3)
# Compare this model without TFT with the previous model.
AIC(ess_glm_mod_2)
AIC(ess_glm_mod_3)
anova(ess_glm_mod_3, ess_glm_mod_2, test = "Chisq")

# The simpler model is better. Remove Any.fibro next.
ess_glm_mod_4 <- glm(ESS ~ Corticoid + Depression + Liver.diag + 
                            RGD + Gender, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_4)
# Compare this model without Any.fibro with the previous model.
AIC(ess_glm_mod_3)
AIC(ess_glm_mod_4)
anova(ess_glm_mod_4, ess_glm_mod_3, test = "Chisq")

# The simpler model is better according to both AIC and ANOVA, Two of the
# levels of Liver.diag had the highest p-values. Let's remove Gender.
ess_glm_mod_5 <- glm(ESS ~ Corticoid + Depression + Liver.diag + RGD, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_5)
# Compare this model without Gender with the previous model.
AIC(ess_glm_mod_4)
AIC(ess_glm_mod_5)
anova(ess_glm_mod_5, ess_glm_mod_4, test = "Chisq")

# The simpler model was better. Remove Depression.
ess_glm_mod_6 <- glm(ESS ~ Corticoid + Liver.diag + RGD, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_6)
# Compare this model without Depression with the previous model.
AIC(ess_glm_mod_5)
AIC(ess_glm_mod_6)
anova(ess_glm_mod_6, ess_glm_mod_5, test = "Chisq")

# Simpler model is better. Try removing RGD next.
ess_glm_mod_7 <- glm(ESS ~ Corticoid + Liver.diag, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_7)
# Compare this model without Depression with the previous model.
AIC(ess_glm_mod_6)
AIC(ess_glm_mod_7)
anova(ess_glm_mod_7, ess_glm_mod_6, test = "Chisq")

#' Simpler model was better as per ANOVA, despite slightly higher AIC. 
#' Try removing Corticoid.
ess_glm_mod_8 <- glm(ESS ~ Liver.diag, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_8)
# Compare this model without Corticoid with the previous model.
AIC(ess_glm_mod_7)
AIC(ess_glm_mod_8)
anova(ess_glm_mod_8, ess_glm_mod_7, test = "Chisq")

# More complex is better. Remove Liver.diag instead next.
ess_glm_mod_9 <- glm(ESS ~ Corticoid, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_9)
# Compare this model without Liver.diag with the previous model.
AIC(ess_glm_mod_7)
AIC(ess_glm_mod_9)
anova(ess_glm_mod_9, ess_glm_mod_7, test = "Chisq")
# More complex is better. Proceed with this model (model 7).

# Find best fitted logistic regression model using stepAIC.
ess_glm_step_back <- stepAIC(ess_glm_mod_full,trace = F)
summary(ess_glm_step_back)
#' Compare the final model, ess_glm_mod_back_7, with the best fitted 
#' step-wise model.
AIC(ess_glm_mod_7)
AIC(ess_glm_step_back)
anova(ess_glm_mod_7, ess_glm_step_back, test = "Chisq")
#' The final model does have a slightly higher AIC, but ANOVA indicates 
#' that the simpler model, ess_glm_mod_7, is better. Both models fit 
#' within the 7 predictor limit. Proceed with the final model (model 7).

# Call the model.
ess_glm_mod_7
# Get beta coefficients and p-values of the AIS model.
summary(ess_glm_mod_7)
# Get the odds ratios of the ESS model.
round(exp(ess_glm_mod_7$coefficients),2)
# Get the confidence intervals of the odds ratios of the ESS model.
round(exp(confint(ess_glm_mod_7)),2)

########################################################
#                                                      #
# End of logistic regression model for ESS as response.#
#                                                      #
########################################################


# Logistic regression for PSQIS

# Select relevant columns for logistic regression model
clean_data_psqis_model <- subset(clean_data2, 
                                 select=c(Corticoid, Depression, BMI, Liver.diag,
                                          RGD, Any.fibro, Renal.fail, Gender, PSQIS))

# Based the relevant predictors based on literature, use stepwise variable selection to obtain a model with optimal fit
psqis_glm_mod_full <- glm(PSQIS ~., data = clean_data_psqis_model, family="binomial")
psqis_glm_step_back <- stepAIC(psqis_glm_mod_full,trace = F)
summary(psqis_glm_step_back)

# Restricting the number of predictors by following the rule of thumb of m/15
# m/15 for PSQIS
table(clean_data2$PSQIS)
# There are 183 participants who experience sleep disturbance according to ESS.
# Whereas, there are 85 participants who do not experience sleep disturbance.
85 / 15
# 5 predictors/degrees of freedom that can be used in the PSQIS model

#NOTE 
# Get the Odds Ratio of the PSQIS model
round(exp(psqis_glm_step_back$coefficients),2)

# Get the confidence interval of the Odds Ratio of the PSQIS model
round(exp(confint(psqis_glm_step_back)),2)


########################################################
#                                                      #
# Create logistic regression model for AIS as response.#
#                                                      #
########################################################

#' Manually create a model to predict AIS without using step-wise function.
#' Create starting data set with pool of variables to start with, based 
#' on literature.
clean_data_ais_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, 
                                        Liver.diag, RGD, Any.fibro, 
                                        Renal.fail, Gender, AIS, TFT))

table(clean_data2$AIS)
#' There are 151 participants who experience sleep disturbance 
#' according to AIS and 117 participants who do not. Using p < m/10:
117/10
# 11 predictors/degrees of freedom that can be used in the AIS model.

# Create our full model based on above variables selected.
ais_glm_mod_full <- glm(AIS ~., data = clean_data_ais_model, family="binomial")
#' Find which variable to remove using highest p-value from summary(). Also
#' check collinearity.
summary(ais_glm_mod_full)
vif(ais_glm_mod_full)
# All values are less than 5, passes collinearity check.

#' Remove Renal.fail since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value).
ais_glm_mod_1_data <- subset(clean_data2, 
                             select=c(Corticoid, Depression, BMI, 
                                      Liver.diag, RGD, Any.fibro, 
                                      Gender, AIS, TFT))
ais_glm_mod_1 <- glm(AIS ~., data = ais_glm_mod_1_data, family="binomial")
# Compare summaries of the models.
summary(ais_glm_mod_full)
summary(ais_glm_mod_1)
# Same variables are significant. Next to remove would be Gender.
anova(ais_glm_mod_1,ais_glm_mod_full, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_full)
AIC(ais_glm_mod_1)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Gender since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value).
ais_glm_mod_2_data <- subset(clean_data2, 
                             select=c(Corticoid, Depression, BMI, Liver.diag,
                                      RGD, Any.fibro, AIS, TFT))
ais_glm_mod_2 <- glm(AIS ~., data = ais_glm_mod_2_data, family="binomial")
summary(ais_glm_mod_1)
summary(ais_glm_mod_2)
# Same variables are significant. Next to remove would be BMI.
anova(ais_glm_mod_2,ais_glm_mod_1, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_1)
AIC(ais_glm_mod_2)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove BMI since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value).
ais_glm_mod_3_data <- subset(clean_data2, 
                             select=c(Corticoid, Depression, Liver.diag,
                                      RGD, Any.fibro, AIS, TFT))
ais_glm_mod_3 <- glm(AIS ~., data = ais_glm_mod_3_data, family="binomial")
summary(ais_glm_mod_2)
summary(ais_glm_mod_3)
# Same variables are significant. Next to remove would be RGD.
anova(ais_glm_mod_3,ais_glm_mod_2, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_2)
AIC(ais_glm_mod_3)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove RGD since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value). 
ais_glm_mod_4_data <- subset(clean_data2, 
                             select=c(Corticoid, Depression, Liver.diag,
                                      Any.fibro, AIS, TFT))
ais_glm_mod_4 <- glm(AIS ~., data = ais_glm_mod_4_data, family="binomial")
summary(ais_glm_mod_3)
summary(ais_glm_mod_4)
# Any.fibro is no longer significant. Next to remove would be Corticoid
anova(ais_glm_mod_4,ais_glm_mod_3, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_3)
AIC(ais_glm_mod_4)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Corticoid since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value). 
ais_glm_mod_5_data <- subset(clean_data2, 
                             select=c(Depression, Liver.diag,
                                      Any.fibro, AIS, TFT))
ais_glm_mod_5 <- glm(AIS ~., data = ais_glm_mod_5_data, family="binomial")
summary(ais_glm_mod_4)
summary(ais_glm_mod_5)
#' Any.fibro is back to being significant and TFT is newly significant. 
#' Next to remove would be Liver.diag.
anova(ais_glm_mod_5,ais_glm_mod_4, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_4)
AIC(ais_glm_mod_5)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Liver.diag since it has the highest p-value.
ais_glm_mod_6_data <- subset(clean_data2, 
                             select=c(Depression,
                                      Any.fibro, AIS, TFT))
ais_glm_mod_6 <- glm(AIS ~., data = ais_glm_mod_6_data, family="binomial")
summary(ais_glm_mod_5)
summary(ais_glm_mod_6)
#' TFT no longer significant. #' Next to remove would be TFT.
anova(ais_glm_mod_6,ais_glm_mod_5, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_5)
AIC(ais_glm_mod_6)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove TFT since it has the highest p-value.
ais_glm_mod_7_data <- subset(clean_data2, 
                             select=c(Depression,
                                      Any.fibro, AIS))
ais_glm_mod_7 <- glm(AIS ~., data = ais_glm_mod_7_data, family="binomial")
summary(ais_glm_mod_6)
summary(ais_glm_mod_7)
#' Any.fibro no longer significant. 
#' Next to remove would be Any.fibro.
anova(ais_glm_mod_7,ais_glm_mod_6, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(ais_glm_mod_6)
AIC(ais_glm_mod_7)
# Smaller model has higher AIC. Let's go back to previous model (model 6).

# Using step-wise and comparing it with manual model for AIS.
ais_glm_step_back <- stepAIC(ais_glm_mod_full,trace = F)
summary(ais_glm_step_back)

anova(ais_glm_mod_6, ais_glm_step_back)
AIC(ais_glm_step_back)
AIC(ais_glm_mod_6)
# They're the same model! Proceed with this model.

# Call the model.
ais_glm_mod_6
# Get beta coefficients and p-values of the AIS model.
summary(ais_glm_mod_6)
# Get the odds ratios of the AIS model.
round(exp(ais_glm_mod_6$coefficients),2)
# Get the confidence intervals of the odds ratios of the AIS model.
round(exp(confint(ais_glm_mod_6)),2)

#################################################################
#                                                               #
# End of logistic regression model creation for AIS as response.#
#                                                               #
#################################################################


########################################################
#                                                      #
# Create logistic regression model for BSS as response.#
#                                                      #
########################################################

#' Manually create a model to predict BSS without using step-wise function.
#' Create starting data set with pool of variables to start with, based 
#' on literature.
clean_data_bss_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, 
                                        Liver.diag, RGD, Any.fibro, 
                                        Renal.fail, Gender, BSS, TFT))

table(clean_data2$BSS)
#' There are 106 participants who experience sleep disturbance 
#' according to BSS and 162 participants who do not. Using p < m/10:
106/10
# 10 predictors/degrees of freedom that can be used in the BSS model.

# Create our full model based on above variables selected.
bss_glm_mod_full <- glm(BSS ~., data = clean_data_bss_model, family="binomial")
#' Find which variable to remove using highest p-value from summary(). Also
#' check collinearity.
summary(bss_glm_mod_full)
vif(bss_glm_mod_full)
# All values are less than 5, passes collinearity check.

#' Remove Renal.fail since it has the highest p-value.
bss_glm_mod_1_data <- subset(clean_data2, 
                             select=c(Corticoid, Depression, BMI, 
                                      Liver.diag, RGD, Any.fibro, 
                                      Gender, BSS, TFT))
bss_glm_mod_1 <- glm(BSS ~., data = bss_glm_mod_1_data, family="binomial")
# Compare summaries of the models.
summary(bss_glm_mod_full)
summary(bss_glm_mod_1)
# Same variables are significant. Next to remove would be Depression.
anova(bss_glm_mod_1,bss_glm_mod_full, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_full)
AIC(bss_glm_mod_1)
#' Smaller model has slightly higher AIC, but due to p < m/10 and non- 
#' significant analysis of deviance, we proceed with next variable removal.

#' Remove Depression since it has the highest p-value.
bss_glm_mod_2_data <- subset(clean_data2, 
                             select=c(Corticoid, BMI, Liver.diag,
                                      RGD, Any.fibro, BSS, Gender, TFT))
bss_glm_mod_2 <- glm(BSS ~., data = bss_glm_mod_2_data, family="binomial")
summary(bss_glm_mod_1)
summary(bss_glm_mod_2)
# Same variables are significant. Next to remove would be Any.fibro.
anova(bss_glm_mod_2,bss_glm_mod_1, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_1)
AIC(bss_glm_mod_2)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Any.fibro since it has the highest p-value (do not count 
#' Liver.diag since one of the levels has a lower p-value).
bss_glm_mod_3_data <- subset(clean_data2, 
                             select=c(Corticoid, BMI, Liver.diag,
                                      RGD, BSS, Gender, TFT))
bss_glm_mod_3 <- glm(BSS ~., data = bss_glm_mod_3_data, family="binomial")
summary(bss_glm_mod_2)
summary(bss_glm_mod_3)
# Same variables are significant. Next to remove would be Gender.
anova(bss_glm_mod_3,bss_glm_mod_2, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_2)
AIC(bss_glm_mod_3)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Gender since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value). 
bss_glm_mod_4_data <- subset(clean_data2, 
                             select=c(Corticoid, BMI, Liver.diag,
                                      RGD, BSS, TFT))
bss_glm_mod_4 <- glm(BSS ~., data = bss_glm_mod_4_data, family="binomial")
summary(bss_glm_mod_3)
summary(bss_glm_mod_4)
# Same variables are significant. Next to remove would be Corticoid.
anova(bss_glm_mod_4,bss_glm_mod_3, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_3)
AIC(bss_glm_mod_4)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Corticoid since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value). 
bss_glm_mod_5_data <- subset(clean_data2, 
                             select=c(BMI, Liver.diag, RGD, BSS, TFT))
bss_glm_mod_5 <- glm(BSS ~., data = bss_glm_mod_5_data, family="binomial")
summary(bss_glm_mod_4)
summary(bss_glm_mod_5)
#' Same variables are significant. Next to remove would be RGD. 
#' Next to remove would be Liver.diag.
anova(bss_glm_mod_5,bss_glm_mod_4, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_4)
AIC(bss_glm_mod_5)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove RGD since it has the highest p-value (do not count Liver.diag
#' since one of the levels has a lower p-value). 
bss_glm_mod_6_data <- subset(clean_data2, 
                             select=c(BMI, Liver.diag, BSS, TFT))
bss_glm_mod_6 <- glm(BSS ~., data = bss_glm_mod_6_data, family="binomial")
summary(bss_glm_mod_5)
summary(bss_glm_mod_6)
#' Same variables are significant. Next to remove would be Liver.diag. 
anova(bss_glm_mod_6,bss_glm_mod_5, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_5)
AIC(bss_glm_mod_6)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove Liver.diag since it has the highest p-value.
bss_glm_mod_7_data <- subset(clean_data2, 
                             select=c(BMI, BSS, TFT))
bss_glm_mod_7 <- glm(BSS ~., data = bss_glm_mod_7_data, family="binomial")
summary(bss_glm_mod_6)
summary(bss_glm_mod_7)
#' Same variables are significant. Next to remove would be TFT. 
anova(bss_glm_mod_7,bss_glm_mod_6, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_6)
AIC(bss_glm_mod_7)
# Smaller model has lower AIC, proceed with next variable removal.

#' Remove TFT since it has the highest p-value.
bss_glm_mod_8_data <- subset(clean_data2, 
                             select=c(BMI, BSS))
bss_glm_mod_8 <- glm(BSS ~., data = bss_glm_mod_8_data, family="binomial")
summary(bss_glm_mod_7)
summary(bss_glm_mod_8)
#' Same variables are significant. No more predictors to remove. 
anova(bss_glm_mod_8,bss_glm_mod_7, test = "Chisq")
# ANOVA reveals no difference, smaller model better.
AIC(bss_glm_mod_7)
AIC(bss_glm_mod_8)
# Smaller model has higher AIC. Let's go back to previous model (model 7).

# Using step-wise and comparing it with manual model for BSS.
bss_glm_step_back <- stepAIC(bss_glm_mod_full,trace = F)
summary(bss_glm_step_back)

anova(bss_glm_mod_7, bss_glm_step_back)
# ANOVA is significant, meaning larger step-wise model is better.
AIC(bss_glm_step_back)
AIC(bss_glm_mod_7)
#' Larger step-wise model has lower AIC and has less than 10 predictors, 
#' as per the rule of thumb. Proceed with this model.

# Call the model.
bss_glm_step_back
# Get beta coefficients and p-values of the BSS model.
summary(bss_glm_step_back)
# Get the odds ratios of the BSS model.
round(exp(bss_glm_step_back$coefficients),2)
# Get the confidence intervals of the odds ratios of the BSS model.
round(exp(confint(bss_glm_step_back)),2)

#################################################################
#                                                               #
# End of logistic regression model creation for BSS as response.#
#                                                               #
#################################################################


#Two Sample T-Test For the Mean for QOL
#two samples - individuals with and without sleep disturbance

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

#This will help prepare for the linear regression model.

#Linear Regression Model 

# Create a linear regression model for sleep disturbance and QOL
sleep_mcs <- lm(SF36.MCS ~ ESS + PSQIS + AIS + BSS, data = clean_data2)
sleep_pcs <- lm(SF36.PCS ~ ESS + PSQIS + AIS + BSS, data = clean_data2)
summary(sleep_mcs)
summary(sleep_pcs)
#' Create a linear regression model for sleep disturbance and QOL 
#' (WITHOUT PSQIS).
sleep_mcs_no_psqis <- lm(SF36.MCS ~ ESS + AIS + BSS, data = clean_data2)
sleep_pcs_no_psqis <- lm(SF36.PCS ~ ESS + AIS + BSS, data = clean_data2)
summary(sleep_mcs_no_psqis)
#' PSQIS was significant in full model, both models have signficance for
#' same variables.
summary(sleep_pcs_no_psqis)
#' PSQIS not significant in full, both models have significance for
#' same variables.

# Get the confidence intervals of the beta coefficient of each model.
confint(sleep_mcs)
confint(sleep_pcs)
#' Get the confidence intervals of the beta coefficient of each model.
#' (WITHOUT PSQIS).
confint(sleep_mcs_no_psqis)
confint(sleep_pcs_no_psqis)

# Visualize the residuals of the linear regression models.
hist(resid(sleep_mcs))
hist(resid(sleep_pcs))
#' Visualize the residuals of the linear regression models.
#' (WITHOUT PSQIS).
hist(resid(sleep_mcs_no_psqis))
hist(resid(sleep_pcs_no_psqis))

# Compare models with and without PSQIS.
anova(sleep_mcs_no_psqis,sleep_mcs)
AIC(sleep_mcs)
AIC(sleep_mcs_no_psqis)
#' Full model has lower AIC and anova is significant, meaning full model 
#' is better.
anova(sleep_pcs_no_psqis,sleep_pcs)
AIC(sleep_pcs)
AIC(sleep_pcs_no_psqis)
#' Full model has lower AIC BUT anova is significant, meaning smaller model 
#' is better. Based on this as well as comparing the summaries, we pick
#' smaller model.

# Plot empirical CDF (quantiles) against the theoretical from a normal distribution
# SF36.MCS
qqnorm(resid(sleep_mcs))
qqline(resid(sleep_mcs), col=2)

# SF36.PCS
qqnorm(resid(sleep_pcs_no_psqis))
qqline(resid(sleep_pcs_no_psqis), col=2)


# Anything below is notes only.
# Create backward step-wise model for ESS as sleep disturbance measure.
clean_data_ess_model_new <- subset(clean_data2, 
                                   select=c(-Subject, -PSQIS, -BSS, -AIS,
                                            -SF36.PCS, -SF36.MCS))
str(clean_data_ess_model_new)
ess_glm_mod_full_new <- glm(ESS~., data = clean_data_ess_model_new, 
                            family="binomial")
ess_glm_step_back_new <- stepAIC(ess_glm_mod_full_new,trace = F)
summary(ess_glm_step_back_new)

# Create backward step-wise model for PSQIS as sleep disturbance measure.
clean_data_psqis_model_new <- subset(clean_data2, 
                                     select=c(-Subject, -ESS, -BSS, -AIS, 
                                              -SF36.PCS, -SF36.MCS))
psqis_glm_mod_full_new <- glm(PSQIS~., data = clean_data_psqis_model_new, family="binomial")
psqis_glm_step_back_new <- stepAIC(psqis_glm_mod_full_new,trace = F)
summary(psqis_glm_step_back_new)

# Create backward step-wise model for BSS as sleep disturbance measure.
clean_data_bss_model_new <- subset(clean_data2, select=c(-Subject, -ESS, -PSQIS, 
                                                         -AIS, -SF36.PCS, -SF36.MCS))
bss_glm_mod_full_new <- glm(BSS~., data = clean_data_bss_model_new, 
                            family="binomial")
bss_glm_step_back_new <- stepAIC(bss_glm_mod_full_new,trace = F)
summary(bss_glm_step_back_new)

# Create backward step-wise model for AIS as sleep disturbance measure.
clean_data_ais_model_new <- subset(clean_data2, 
                                   select=c(-Subject, -ESS, -PSQIS, -BSS,
                                            -SF36.PCS, -SF36.MCS))
ais_glm_mod_full_new <- glm(AIS~., data = clean_data_ais_model_new, 
                            family="binomial")
ais_glm_step_back_new <- stepAIC(ais_glm_mod_full_new,trace = F)
summary(ais_glm_step_back_new)