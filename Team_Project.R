library(dplyr)

# Import data.
raw_data <- read.csv("project_data.csv")
View(raw_data)

# Explore data.
summary(raw_data)
str(raw_data)
dim(raw_data)
names(raw_data)

# Extract variables of interest.
working_data <- raw_data[,c(1,2,3,9,16,18,19,20,24,42,46,58,70,75,77,85,86,87)]
# No blanks in data set.
which(working_data == "")

# Count NAs in each variable.
for (i in names(working_data)) {
  print(i)
  print(sum(is.na(working_data[,i])))
}

# Remove rows (subjects) with any NAs.
clean_data <- na.omit(working_data)
summary(clean_data)
str(clean_data)

# Rename variables.
clean_data1 <- clean_data %>%
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
clean_data2 <- clean_data1 %>%
  mutate(ESS = ifelse(ESS > 10, 1, 0)) %>%
  mutate(PSQIS = ifelse(PSQIS > 4, 1, 0)) %>%
  mutate(AIS = ifelse(AIS > 5, 1, 0))

# Change categorical variables to factors.
clean_data3 <- clean_data2 %>% 
  mutate(across(c("Gender", "Liver.diag", "RoD", "RGD", "Any.fibro",
                  "Renal.fail", "Depression", "Corticoid", "ESS", 
                  "PSQIS", "AIS", "BSS"), as.factor))

# Create a function that extracts descriptive statistics for a variable and
# displays them in a graph
descriptive <- function(var, data){
  colours <- c("blue", "skyblue")
  for (i in var) {
    print(paste0("Summary Statistics for ", i, ":"))
    print(summary(data[[i]]))
    if (is.factor(data[[i]])) {
      plot(data[[i]], main = paste0("Frequency of ", i), 
           xlab = i, ylab = "Frequency", col = colours)
    } else {
      # plot_num(data)
      hist(data[[i]], main = paste0("Distribution of ", i),
           xlab = i, ylab = "Frequency", col = colours)
    }
  }
}

# Specify the variables needed to conduct summary statistics
vars <- c("Gender", "Age", "BMI", "TFT", "Liver.diag", "RoD", "RGD", 
          "Any.fibro", "Renal.fail", "Depression", "Corticoid", 
          "ESS", "PSQIS", "AIS", "BSS", "SF36.PCS", "SF36.MCS")
# Conduct descriptive analysis
descriptive(vars, clean_data3)

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
prev_any <- (length(which(clean_data3$ESS == 1 | clean_data3$PSQIS == 1 |
                           clean_data3$AIS == 1 | clean_data3$BSS == 1))) / nrow(clean_data3)
print(paste0("Prevalence of sleep disturbance according to any questionnaire: ", prev_any))

# Specify scales needed to estimate prevalence of sleep disturbance
scales <- c("ESS", "PSQIS", "AIS", "BSS")
# Calculate prevalence of sleep disturbance
disturbed_sleep_preval(scales, clean_data3)

# Create backward step-wise model for ESS as sleep disturbance measure.
clean_data_ess_model <- subset(clean_data3, select=c(-Subject, -PSQIS, -BSS, -AIS))
str(clean_data_ess_model)
ess_glm_mod_full <- glm(ESS~., data = clean_data_ess_model, family="binomial")
ess_glm_step_back <- stepAIC(ess_glm_mod_full,trace = F)
summary(ess_glm_step_back)

# Create backward step-wise model for PSQIS as sleep disturbance measure.
clean_data_psqis_model <- subset(clean_data3, select=c(-Subject, -ESS, -BSS, -AIS))
str(clean_data_psqis_model)
psqis_glm_mod_full <- glm(PSQIS~., data = clean_data_psqis_model, family="binomial")
psqis_glm_step_back <- stepAIC(psqis_glm_mod_full,trace = F)
summary(psqis_glm_step_back)

# Create backward step-wise model for BSS as sleep disturbance measure.
clean_data_bss_model <- subset(clean_data3, select=c(-Subject, -ESS, -PSQIS, -AIS))
str(clean_data_bss_model)
bss_glm_mod_full <- glm(BSS~., data = clean_data_bss_model, family="binomial")
bss_glm_step_back <- stepAIC(bss_glm_mod_full,trace = F)
summary(bss_glm_step_back)

# Create backward step-wise model for AIS as sleep disturbance measure.
clean_data_ais_model <- subset(clean_data3, select=c(-Subject, -ESS, -PSQIS, -BSS))
str(clean_data_ais_model)
ais_glm_mod_full <- glm(AIS~., data = clean_data_ais_model, family="binomial")
ais_glm_step_back <- stepAIC(ais_glm_mod_full,trace = F)
summary(ais_glm_step_back)

#' Evaluate relationship between sleep disturbance and quality of 
#' life (physical and mental).
attach(clean_data3)

# Create a linear regression model for sleep disturbance and QOL
sleep_mcs <- lm(SF36.MCS ~ ESS + PSQIS + AIS + BSS)
sleep_pcs <- lm(SF36.PCS ~ ESS + PSQIS + AIS + BSS)
summary(sleep_mcs)
summary(sleep_pcs)
