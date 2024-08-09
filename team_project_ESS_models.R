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
# graft dysfunction/rejection (1), fibrosis (1), renal failure (1), gender (1), 
# and Time from transplantation (1).
# There is a total of 12 degree of freedom (df) from the predictors above.


# Logistic regression for ESS

# Select relevant columns for logistic regression model
clean_data_ess_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, 
                                        TFT, ESS))

# Create a logistic regression model with all selected predictors from literature
ess_glm_mod_full <- glm(ESS ~., data = clean_data_ess_model, family="binomial")
summary(ess_glm_mod_full)

# Find best fitted logistic regression model using stepAIC
ess_glm_step_back <- stepAIC(ess_glm_mod_full,trace = F)
summary(ess_glm_step_back)

# Manually removing variables based on p-values. 
# Remove Renal.fail 
ess_glm_mod_back_1 <- glm(ESS ~ Corticoid + Depression + BMI + Liver.diag + 
                            RGD + Any.fibro + Gender + TFT, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_1)

# Compare this model without Renal.fail with the full model
AIC(ess_glm_mod_full)
AIC(ess_glm_mod_back_1)
anova(ess_glm_mod_full, ess_glm_mod_back_1, test = "Chisq")

# Remove BMI as that had the highest p-value in ess_glm_mod_back_1
ess_glm_mod_back_2 <- glm(ESS ~ Corticoid + Depression + Liver.diag + 
                            RGD + Any.fibro + Gender +TFT, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_2)

# Compare this model without BMI with the previous model
AIC(ess_glm_mod_back_1)
AIC(ess_glm_mod_back_2)
anova(ess_glm_mod_back_1, ess_glm_mod_back_2, test = "Chisq")

# AIC and anova showed that the simpler model is better. Remove
# TFT as a variable from the model.
ess_glm_mod_back_3 <- glm(ESS ~ Corticoid + Depression + Liver.diag + Any.fibro +
                            RGD + Gender, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_3)

# Compare this model without Any.fibro with the previous model
AIC(ess_glm_mod_back_2)
AIC(ess_glm_mod_back_3)
anova(ess_glm_mod_back_2, ess_glm_mod_back_3, test = "Chisq")

# The simpler model is better. Remove Any.fibro next.
ess_glm_mod_back_4 <- glm(ESS ~ Corticoid + Depression + Liver.diag + 
                            RGD + Gender, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_4)

# Compare this model without Any.fibro with the previous model
AIC(ess_glm_mod_back_3)
AIC(ess_glm_mod_back_4)
anova(ess_glm_mod_back_3, ess_glm_mod_back_4, test = "Chisq")

# The simpler model is better according to both AIC and anova. Two of the levels
# of Liver.diag had the highest p-values. Let's remove Gender.
ess_glm_mod_back_5 <- glm(ESS ~ Corticoid + Depression + Liver.diag + RGD, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_5)

# Compare this model without Gender with the previous model
AIC(ess_glm_mod_back_4)
AIC(ess_glm_mod_back_5)
anova(ess_glm_mod_back_4, ess_glm_mod_back_5, test = "Chisq")

# The simpler model was better. Remove Depression.
ess_glm_mod_back_6 <- glm(ESS ~ Corticoid + Liver.diag + RGD, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_6)

# Compare this model without Depression with the previous model
AIC(ess_glm_mod_back_5)
AIC(ess_glm_mod_back_6)
anova(ess_glm_mod_back_5, ess_glm_mod_back_6, test = "Chisq")

# Simpler model is better. Try removing RGD next.
ess_glm_mod_back_7 <- glm(ESS ~ Corticoid + Liver.diag, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_7)

# Compare this model without Depression with the previous model
AIC(ess_glm_mod_back_6)
AIC(ess_glm_mod_back_7)
anova(ess_glm_mod_back_6, ess_glm_mod_back_7, test = "Chisq")

# Simpler model was better. Try removing Corticoid.
ess_glm_mod_back_8 <- glm(ESS ~ Liver.diag, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_8)

# Compare this model without Corticoid with the previous model
AIC(ess_glm_mod_back_7)
AIC(ess_glm_mod_back_8)
anova(ess_glm_mod_back_7, ess_glm_mod_back_8, test = "Chisq")

# More complex is better. Remove Liver.diag instead next.
ess_glm_mod_back_9 <- glm(ESS ~ Corticoid, 
                          data = clean_data_ess_model, family = "binomial")
summary(ess_glm_mod_back_9)

# Compare this model without Liver.diag with the previous model
AIC(ess_glm_mod_back_7)
AIC(ess_glm_mod_back_9)
anova(ess_glm_mod_back_7, ess_glm_mod_back_9, test = "Chisq")

# More complex is better. Compare this final model, ess_glm_mod_back_7, with the 
# previous stepwise model.
AIC(ess_glm_mod_back_7)
AIC(ess_glm_step_back)
anova(ess_glm_mod_back_7, ess_glm_step_back, test = "Chisq")
# AIC indicates the stepwise model is better. ANOVA indicates that the simpler 
# model, ess_glm_mod_back_7, is better. Check with the rule of thumb.
# Restricting the number of predictors by following the rule of thumb of m/10
# m/10 for ESS.
table(clean_data2$ESS)
# There are 74 participants who experience sleep disturbance according to ESS.
# Whereas, there are 194 participants who do not experience sleep disturbance.
74 / 10
# 7 predictors/degrees of freedom that can be used in the ESS model
# The new model is better, since it has less than 7 degrees of freedom. It will 
# be used to explain ESS.
ess_glm_mod_back_7
summary(ess_glm_mod_back_7)

#NOTE 
# Get the Odds Ratio of the ESS model
round(exp(ess_glm_mod_back_7$coefficients),2)

# Get the confidence interval of the Odds Ratio of the ESS model
round(exp(confint(ess_glm_mod_back_7)),2)

plot(fitted(ess_glm_mod_back_7),resid(ess_glm_mod_back_7))
qqnorm(resid(ess_glm_mod_back_7))

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


# Logistic regression for AIS

# Select relevant columns for logistic regression model
clean_data_ais_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, AIS))

# Based the relevant predictors based on literature, use stepwise variable selection to obtain a model with optimal fit
ais_glm_mod_full <- glm(AIS ~., data = clean_data_ais_model, family="binomial")
ais_glm_step_back <- stepAIC(ais_glm_mod_full,trace = F)
summary(ais_glm_step_back)

#NOTE
ais_glm_mod_2 <- glm(AIS ~ Depression + Any.fibro + Corticoid + RGD + Gender + Renal.fail,
                     data = clean_data_ais_model, family="binomial")
summary(ais_glm_mod_2)
anova(ais_glm_step_back, ais_glm_mod_2, test = "Chisq")

# Restricting the number of predictors by following the rule of thumb of m/15
# m/15 for AIS
table(clean_data2$AIS)
# There are 151 participants who experience sleep disturbance according to ESS.
# Whereas, there are 117 participants who do not experience sleep disturbance.
117/15
# 7 predictors/degrees of freedom that can be used in the AIS model

#NOTE 
# Get the Odds Ratio of the AIS model
round(exp(ais_glm_step_back$coefficients),2)

# Get the confidence interval of the Odds Ratio of the AIS model
round(exp(confint(ais_glm_step_back)),2)


# Logistic regression for BSS

# Select relevant columns for logistic regression model
clean_data_bss_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, BSS))

# Based the relevant predictors based on literature, use stepwise variable selection to obtain a model with optimal fit
bss_glm_mod_full <- glm(BSS ~., data = clean_data_bss_model, family="binomial")
bss_glm_step_back <- stepAIC(bss_glm_mod_full,trace = F)
summary(bss_glm_step_back)

# Restricting the number of predictors by following the rule of thumb of m/15
# m/15 for BSS
table(clean_data2$BSS)
# There are 106 participants who experience sleep disturbance according to ESS.
# Whereas, there are 162 participants who do not experience sleep disturbance.
106/15
# 7 predictors/degrees of freedom that can be used in the BSS model

#NOTE 
# Get the Odds Ratio of the BSS model
round(exp(bss_glm_step_back$coefficients),2)

# Get the confidence interval of the Odds Ratio of the BSS model
round(exp(confint(bss_glm_step_back)),2)


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

