# Module 1.3.2 Code ============================================
# Section 1. To read in data ===================================

# Import the CSV file
library(readr)
mydata <- read_csv("mydata.csv")

# Import the EXCEL file
# Choose the "Data" TAB
library(readxl)
mydata <- read_excel("mydata.xlsx", sheet = "Data")

# Import a SPSS file
library(haven)
mydata <- read_sav("mydata.sav")

# Import a SAS file
library(haven)
mydata <- read_sas("mydata.sas7bdat", NULL)

# optionally install pkgsearch
# install.packages("pkgsearch")
library(pkgsearch)

# get history of haven package
havenhistory <- cran_package_history("haven")

# get history of foreign package
foreignhistory <- cran_package_history("foreign")

# display the earliest date on CRAN
# for these 2 packages
havenhistory$date[1]
foreignhistory$date[1]

# take a look at the datasets available in the
# "datasets" base R package
data()

# look at builtin dataset "pressure"
pressure

data(pressure)
View(pressure)

# look at datasets included with the
# palmerpenguins dataset
data(package = "palmerpenguins")

help(penguins, package = "palmerpenguins")
library(palmerpenguins)
data(penguins)

# Section 2. To view The Data ==================================

# import the mydata.csv dataset
mydata <- readr::read_csv("mydata.csv")

# print the dataset into the Console
mydata

str(mydata)

# Section 3. To subset the data - select and filter ============

# look at top 6 rows of data
head(mydata)

# look at the bottom 10 rows of data
tail(mydata, n=10)

help(NA, package = "base")

# Select the values in rows 1-4
# and in columns 1-3
mydata[1:4, 1:3]

# show all of rows 1-2
mydata[1:2, ]

# show all of columns 3-4
mydata[ ,3:4]

# list variable names in mydata
names(mydata)

# look at all of the ages
# of the 21 people in mydata
mydata$Age

# show all rows for
# the 2 weight variables in mydata
mydata[ , c("WeightPRE", "WeightPOST")]

# load dplyr package
library(dplyr)

# select Height and q1 from mydata
dplyr::select(mydata, c(Height, q1))

# start with mydata and then 
# select Height and q1 from mydata
mydata %>% dplyr::select(Height, q1)

# select Height and q1 from mydata
# and show only the top 6 rows
mydata %>% 
  dplyr::select(Height, q1) %>%
  head()

# select Height and q1 from mydata
# and show only the top 6 rows
mydata |> 
  dplyr::select(Height, q1) |>
  head()

mydata %>%
  dplyr::select(starts_with("q"))

# select columns from mydata
# and then only show rows for females
mydata %>%
  select(GenderCoded, Age, WeightPRE) %>%
  filter(GenderCoded == 2)

# this will generate an error
# select columns from mydata
# and then only show rows for females
mydata %>%
  select(GenderCoded, Age, WeightPRE) %>%
  filter(GenderCoded = 2)

mydata %>%
  filter(SubjectID %in% c(14, 21, 24))

# take mydata
# select SubjectID and Age
# sort descending by Age
# show the top 5 IDs and Ages
mydata %>%
  select(SubjectID, Age) %>%
  arrange(desc(Age)) %>%
  head(n=5)

# Section 4. To create and modify variables ====================

# Compute BMI for the PRE Weight
mydata$bmiPRE <- 
  (mydata$WeightPRE * 703) / (mydata$Height * 12)^2

# look at result
mydata$bmiPRE

# look at updated data structure
str(mydata)

# list the variable names in the
# updated dataset
names(mydata)

# Compute BMI for the POST Weight
# use the dplyr::mutate() function
mydata <- mydata %>%
  mutate(
    bmiPOST = (WeightPOST * 703) / (Height * 12)^2
  )

# check updates
str(mydata)
names(mydata)

mydata$GenderCoded

# create a new factor with labels
mydata$GenderCoded.f <-
  factor(mydata$GenderCoded,
         levels = c(1, 2),
         labels = c("Male", "Female"))

# look at new variable
mydata$GenderCoded.f

class(mydata$GenderCoded)
class(mydata$GenderCoded.f)

# table of frequencies of GenderCoded - numeric class
table(mydata$GenderCoded, useNA = "ifany")

# table of GenderCoded.f - factor class
table(mydata$GenderCoded.f, useNA = "ifany")

# Section 5. To get data summary and descriptive statistics ====

summary(mydata)

mydata %>%
  select(Age, GenderCoded.f, bmiPRE) %>%
  Hmisc::describe()

mydata %>%
  select(Age, bmiPRE) %>%
  psych::describe()

# get min, max for Age
min(mydata$Age)
max(mydata$Age)

min(mydata$Age, na.rm = TRUE)
max(mydata$Age, na.rm = TRUE)

# get median bmiPRE
# and 25th and 75th percentiles for bmiPRE
median(mydata$bmiPRE,
       na.rm = TRUE)

quantile(mydata$bmiPRE, 
         probs = 0.25,
         na.rm = TRUE)

quantile(mydata$bmiPRE, 
         probs = 0.75,
         na.rm = TRUE)

mean(mydata$Height, na.rm = TRUE)
sd(mydata$Height, na.rm = TRUE)

mydata %>%
  dplyr::summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

mydata %>%
  dplyr::group_by(GenderCoded.f) %>%
  dplyr::summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

# Step 1
class(mydata)

# save the output of step 2
step2 <- mydata %>%
  dplyr::group_by(GenderCoded.f)

class(step2)

step3 <- mydata %>%
  dplyr::group_by(GenderCoded.f) %>%
  dplyr::summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

class(step3)

# pull out the mean_age column using $
step3$mean_age

# pull out the sd_age column using select()
step3 %>%
  select(sd_age)

mydata$SES.f <- 
  factor(mydata$SES,
         levels = c(1, 2, 3),
         labels = c("low income",
                    "average income",
                    "high income"))

library(arsenal)
tab1 <- tableby(GenderCoded.f ~ Age + bmiPRE +SES.f, 
                data = mydata)
summary(tab1)

library(gtsummary)

mydata %>%
  select(Age, bmiPRE, SES.f, GenderCoded.f) %>%
  tbl_summary(by = GenderCoded.f)

library(tableone)

tableone::CreateTableOne(
  data = mydata,
  vars = c("Age", "bmiPRE", "SES.f"),
  strata = "GenderCoded.f"
)

library(gmodels)

CrossTable(mydata$SES.f,          # row variable
           mydata$GenderCoded.f,  # column variable
           prop.t = FALSE,        # turn off percent of total
           prop.r = FALSE,        # turn off percent of row
           prop.c = TRUE,         # turn on percent of column
           prop.chisq = FALSE,    # turn off percent for chisq test
           format = "SPSS")       # format like SPSS

# Section 6. Exporting/Saving Data =============================

# save the mydata dataset object
save(mydata,
     file = "mydata.RData")

# save all objects from module 1.3.2
save.image(file = "module132.RData")

# save mydata and tab1
save(mydata, tab1,
     file = "mydata_tab1.RData")

# remove all objects
rm(list = ls())

# check that global environment is empty
ls()

# read in mydata
load(file = "mydata.RData")

# check objects in global environment
ls()

rm(list = ls())

# read in mydata_tab1
load(file = "mydata_tab1.RData")

# check objects in global environment
ls()

rm(list = ls())

# read in module132.RData
load(file = "module132.RData")

# check objects in global environment
ls()

rm(list = ls())

# read in mydata
load(file = "mydata.RData")

# write as CSV
readr::write_csv(mydata,
                 file = "mydata_updated.csv")

haven::write_sav(mydata,
                 path = "mydata_updated.sav")

# rename GenderCoded.f and SES.f since the
# xxx.f wont work for SAS or Stata
names(mydata)[names(mydata) == "GenderCoded.f"] <-
  "GenderCoded_f"
names(mydata)[names(mydata) == "SES.f"] <-
  "SES_f"

haven::write_xpt(mydata,
                 path = "mydata_updated.xpt")

haven::write_dta(mydata,
                 path = "mydata_updated.dta")






