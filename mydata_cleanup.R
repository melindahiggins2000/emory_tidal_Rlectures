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


