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
which(working_data == "")
# No blanks in data set.

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


# Imputation with stochastic regression
imp_stoch <- mice(working_data1, method = "norm.nob", seed = 32,
                  m = 1, print = FALSE)
any(is.na(imp_stoch))
summary(imp_stoch)
xyplot(imp_stoch, Pittsburgh.Sleep.Quality.Index.Score ~ Age)

# Convert from mids to dataframe
imp_stoch_df <- complete(imp_stoch)

# Rename variables.
clean_data <- imp_stoch_df %>%
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

# Convert sleep disturbance measurements to binary.
clean_data1 <- clean_data %>%
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
prev_any <- (length(which(clean_data2$ESS == 1 | clean_data2$PSQIS == 1 |
                            clean_data2$AIS == 1 | clean_data2$BSS == 1))) / nrow(clean_data2)
print(paste0("Prevalence of sleep disturbance according to any questionnaire: ", prev_any))

# Specify scales needed to estimate prevalence of sleep disturbance
scales <- c("ESS", "PSQIS", "AIS", "BSS")
# Calculate prevalence of sleep disturbance
disturbed_sleep_preval(scales, clean_data2)

# Corticosteroid (1), depression (1), BMI (1), liver diagnosis (4), 
# graft dysfunction/rejection (1), fibrosis (1), renal failure (1), gender (1)
# 11 df

# Logistic regression

# Select relevant columns for logistic regression model
clean_data_ess_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, ESS))

cor.test(clean_data_ess_model$Corticoid, 
         clean_data_ess_model$ESS)

ess_glm_mod_full <- glm(ESS ~., data = clean_data_ess_model, family="binomial")
ess_glm_step_back <- stepAIC(ess_glm_mod_full,trace = F)
summary(ess_glm_step_back)

# m/10 for ESS
table(clean_data2$ESS)
# There are 74 participants who experience sleep disturbance according to ESS.
# Whereas, there are 194 participants who do not experience sleep disturbance.
74 / 10
# 4 predictors/degrees of freedom


# Logistic regression for PSQIS

# Select relevant columns for logistic regression model
clean_data_psqis_model <- subset(clean_data2, 
                                 select=c(Corticoid, Depression, BMI, Liver.diag,
                                          RGD, Any.fibro, Renal.fail, Gender, PSQIS))

psqis_glm_mod_full <- glm(PSQIS ~., data = clean_data_psqis_model, family="binomial")
psqis_glm_step_back <- stepAIC(psqis_glm_mod_full,trace = F)
summary(psqis_glm_step_back)

# m/15 for PSQIS
table(clean_data2$PSQIS)
# There are 183 participants who experience sleep disturbance according to ESS.
# Whereas, there are 85 participants who do not experience sleep disturbance.
85 / 15
# 5 predictors/degrees of freedom


# Logistic regression for AIS

# Select relevant columns for logistic regression model
clean_data_ais_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, AIS))

ais_glm_mod_full <- glm(AIS ~., data = clean_data_ais_model, family="binomial")
ais_glm_step_back <- stepAIC(ais_glm_mod_full,trace = F)
summary(ais_glm_step_back)

ais_glm_mod_2 <- glm(AIS ~ Depression + Any.fibro + Liver.diag, 
                     data = clean_data_ais_model, family="binomial")
summary(ais_glm_mod_2)
anova(ais_glm_step_back, ais_glm_mod_2)
# m/15 for AIS
table(clean_data2$AIS)
# There are 151 participants who experience sleep disturbance according to ESS.
# Whereas, there are 117 participants who do not experience sleep disturbance.
117/15
# 7 predictors/degrees of freedom

# Logistic regression for BSS

# Select relevant columns for logistic regression model
clean_data_bss_model <- subset(clean_data2, 
                               select=c(Corticoid, Depression, BMI, Liver.diag,
                                        RGD, Any.fibro, Renal.fail, Gender, BSS))

bss_glm_mod_full <- glm(BSS ~., data = clean_data_bss_model, family="binomial")
bss_glm_step_back <- stepAIC(bss_glm_mod_full,trace = F)
summary(bss_glm_step_back)

# m/15 for BSS
table(clean_data2$BSS)
# There are 106 participants who experience sleep disturbance according to ESS.
# Whereas, there are 162 participants who do not experience sleep disturbance.
106/15
# 7 predictors/degrees of freedom

# Create a linear regression model for sleep disturbance and QOL
sleep_mcs <- lm(SF36.MCS ~ ESS + PSQIS + AIS + BSS, data = clean_data2)
sleep_pcs <- lm(SF36.PCS ~ ESS + PSQIS + AIS + BSS, data = clean_data2)
summary(sleep_mcs)
summary(sleep_pcs)


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