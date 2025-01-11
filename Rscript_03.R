# R Script 03 =================================
# read in abalone dataset
# go to File/Import Dataset/From text (readr)



# capture code - run here
# file import dataset using readr package
# this package was already installed for you
# but we have to load it into memory to use it
# the read_csv() function is in the readr package
library(readr)
abalone <- read_csv("abalone.csv")

# take a look at top 10 rows of abalone dataset
head(abalone, 10)


# get frequency of sex
table(abalone$sex)

# get mean and sd of length
mean(abalone$length)
sd(abalone$length)

# create colors for each sex
# use ifelse() function
# set M to blue
# set F to red
# set I to green
abalone$sexColor <-
  ifelse(
    abalone$sex == "M",
    "blue",
    ifelse(
      abalone$sex == "F",
      "red",
      "green"
  ))

# make plot of x=wholeWeight, y=rings
# color by sex
plot(x=abalone$wholeWeight, 
     y=abalone$rings, 
     pch=16,
     col = abalone$sexColor)


# let's open AbaloneReport.rmd
# Rmarkdown and explore that "reproducible" document.


