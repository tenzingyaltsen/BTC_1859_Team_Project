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

