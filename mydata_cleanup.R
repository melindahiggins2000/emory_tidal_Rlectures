# mydata - cleanup data
# Import the CSV file
library(readr)
mydata <- read_csv("mydata.csv")

# Compute BMI for the PRE Weight
# base R workflow
mydata$bmiPRE <- 
  (mydata$WeightPRE * 703) / (mydata$Height * 12)^2

# Compute BMI for the POST Weight
# use the dplyr::mutate() function
# tidyverse workflow
mydata <- mydata %>%
  mutate(
    bmiPOST = (WeightPOST * 703) / (Height * 12)^2
  )

# create GenderCoded as a factor with labels
mydata$GenderCoded.f <-
  factor(mydata$GenderCoded,
         levels = c(1, 2),
         labels = c("Male", "Female"))

# create SES as a factor with labels
mydata$SES.f <- 
  factor(mydata$SES,
         levels = c(1, 2, 3),
         labels = c("low income",
                    "average income",
                    "high income"))

# make a copy of the dataset
mydata_corrected <- mydata

# compute a new corrected height
# fix heights for these 2 IDs
mydata_corrected <- 
  mydata_corrected %>%
  mutate(Height_corrected = case_when(
    (SubjectID == 28) ~ 6.2,
    (SubjectID == 8) ~ NA_real_,
    .default = Height
  ))

# for WeightPRE < 100, convert kg to lbs
mydata_corrected <- mydata_corrected %>%
  mutate(WeightPRE_corrected = case_when(
    (WeightPRE < 100) ~ WeightPRE * 2.20462,
    .default = WeightPRE
  ))

# For WeightPOST, for
# SubjectID 28, change WeightPOST=98 to 198
# since this person's WeightPRE was 230.
# also fix SubjectID= 20, for
# WeightPOST from 109 to 209 since
# their WeightPRE was 260

mydata_corrected <- mydata_corrected %>%
  mutate(WeightPOST_corrected = case_when(
    (SubjectID == 28) ~ 198,
    (SubjectID == 20) ~ 209,
    .default = WeightPOST
  ))


