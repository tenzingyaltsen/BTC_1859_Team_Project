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
summary(working_data)
str(working_data)

# Count NAs in each variable.
for (i in names(working_data)) {
  print(i)
  print(sum(is.na(working_data[,i])))
}

# Remove rows (subjects) with any NAs.
clean_data <- na.omit(working_data)
summary(clean_data)
str(clean_data)

# Make logicals, scales into factors.
