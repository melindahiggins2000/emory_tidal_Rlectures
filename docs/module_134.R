# Module 1.3.4 Code =======================================
# Section 0. Pre-work =====================================
# Make sure you have these packages installed
#   - VIM
- skimr (optional)
- modelsummary (optional)
- summarytools (optional)
- palmerpenguins
- ggplot2
- naniar 
- dplyr
- gtsummary 
- Hmisc
- mice 


# NOTE: Built-in datasets will be used for this module, so no new data needs to be loaded here.

# Section 1. Identify, summarize and visualize missing data ====
# Find Missing Data in Your Dataset ==========================

# load VIM package
# and sleep dataset within VIM Package
library(VIM)
data("sleep")

# open sleep in data viewer.

# Describe Missing Data ======================================

# do a simple summary - look at counts of NAs
summary(sleep)

# optional - example with skimr package ========================

# load skimr package, run skim() function
# to get summary stats
library(skimr)
skim(sleep)

# optional - example with modelsummary package =================

# load modelsummary package
# run datasummary_skim() based on skimr package
library(modelsummary)
datasummary_skim(sleep)

# optional - example with summarytools package =================

# load summarytools package
# run dfSummary(sleep) and 
# use view() to see formatted output
library(summarytools)
view(dfSummary(sleep))

# try a few of these on the penguins dataset ===================
# from palmerpenguins package

# load palmerpenguins package
# try summary()
library(palmerpenguins)
summary(penguins)

# try skim() from skimr package
skim(penguins)

# Visualize Missing Data =======================================

# Use aggr() function from VIM package =========================

# get the amount of missing data in the sleep dataset
a <- aggr(sleep, plot = FALSE)
a

# look at the complete missing summary
a$missings

# plots of missing data ========================================

# make plots of the amounts and patterns of missing data
plot(a, numbers = TRUE, prop = FALSE)

# Marginplots of how missingness varies with other measures ====

# pull out 2 variables from sleep
# make scatterplot highlighting missing values
x <- sleep[, c("Dream", "Sleep")]
marginplot(x)

# Visualize Missing Data with the `naniar` package =============

# load naniar and ggplot2
# make scatterplot highlighting the missing
library(naniar)
library(ggplot2)

ggplot(sleep, 
       aes(x = Dream, 
           y = Sleep)) + 
  geom_miss_point()

# make UpSet plot with naniar package
# to get plot of missing value patterns
gg_miss_upset(sleep)

# Section 2. Missing Data Mechanisms (bias mechanisms or models) ====
# Impact of missing data for descriptive stats - the mean ======

# compute mean for Dream variable
mean(sleep$Dream)

# compute mean for Dream variable - add parameter to remove missing values first
mean(sleep$Dream, na.rm = TRUE)

# Impact of missing data for summary statistics ================
# also load dplyr to use %>% pipes
# also notice the customized output
# for statistic = used inside the tbl_summary() function call
library(dplyr)
library(gtsummary)

sleep %>%
  select(Dream, Gest, BrainWgt) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{N_nonmiss}",
      "{mean} ({sd})"
    )
  )

# Impact of missing data for regression models =================
# wrap summary() around lm() output
# to get detailed results
summary(lm(Sleep ~ Dream, data = sleep))

# Impact of missing data - correlation - cor() function ========

# LISTWISE deletion, use = "complete.obs"
sleep %>%
  select(BrainWgt, Dream, Sleep) %>%
  cor(use = "complete.obs")

# PAIRWISE deletion, use = "pairwise.complete.obs"
sleep %>%
  select(BrainWgt, Dream, Sleep) %>%
  cor(use = "pairwise.complete.obs")

# Impact of missing data - correlation matrix - Hmisc package =======

# load Hmisc
# select 3 variables
# convert to a matrix first
# run rcorr()
# save output to c1
# This is PAIRWISE deletion by default
library(Hmisc)
c1 <- sleep %>%
  select(BrainWgt, Dream, Sleep) %>%
  as.matrix() %>%
  rcorr()

# view the correlations
c1$r

# view the sample sizes for each cell
c1$n

# view the p-values for each cell
c1$P

# select 3 variables
# filter out only the complete cases
# this is LISTWISE deletion
# convert to a matrix first
# run rcorr()
# save output to c1
c2 <- sleep %>%
  select(BrainWgt, Dream, Sleep) %>%
  filter(complete.cases(.)) %>% 
  as.matrix() %>%
  rcorr()

# view the correlations
c2$r

# view the sample sizes for each cell
c2$n

# view the p-values for each cell
c2$P

# Compare rows with and without missing data ===================

# make small dataset
s1 <- sleep %>%
  select(BodyWgt, BrainWgt, Dream)

# add missing indicator
s1$Dream_missing <- as.numeric(is.na(s1$Dream))

# both BodyWgt and BrainWgt are highly right skewed
# do a log transform of both
s1 <- s1 %>%
  mutate(log_BodyWgt = log(BodyWgt),
         log_BrainWgt = log(BrainWgt))

# make comparison table using gtsummary
s1 %>%
  tbl_summary(
    by = Dream_missing,
    include = c(BodyWgt, log_BodyWgt,
                BrainWgt, log_BrainWgt)
  ) %>%
  add_p()

# Use side-by-side boxplots to see these differences============

ggplot(s1, aes(group = Dream_missing,
               y = log_BodyWgt)) +
  geom_boxplot()

# Section 3. Missing Data Handling and Imputation Methods ======
# Imputation - Mean Substitution ===============================

# small dataset with 5 numbers
# get mean and sd
x <- c(2, 4, 3, 5, 10)
mean(x, na.rm = TRUE)
sd(x, na.rm = TRUE)

# set 2 to NA missing and run again
xna <- c(NA, 4, 3, 5, 10)
mean(xna, na.rm = TRUE)
sd(xna, na.rm = TRUE)

# put the mean of the 4 non-missing values
# in place of the NA and recompute
xsub <- c(5.5, 4, 3, 5, 10)
mean(xsub, na.rm = TRUE)
sd(xsub, na.rm = TRUE)

# k-nearest neighbor (kNN) missing imputation method ===========

# get small dataset for 2 variables from sleep
# run kNN() function from VIM package
x <- sleep[, c("Dream", "Sleep")]
x_imputed <- kNN(x)

# view scatterplot (marginplot) of the new kNN imputed values =======
# Notice the coloring of the points - the blue are the original values and the other colors represent the structure of missings.
# 
# * brown points represent values where Dream was missing initially
# * beige points represent values where Sleep was missing initially
# * black points represent values where both Dream and Sleep were missing initially

marginplot(x_imputed, delimiter = "_imp")

# kNN imputed values - compare correlations before and after ========

# correlation - original data with missing values
x %>%
  as.matrix() %>%
  Hmisc::rcorr()

# correlation - kNN imputed data 
x_imputed %>%
  select(Dream, Sleep) %>%
  as.matrix() %>%
  Hmisc::rcorr()

# kNN imputed values - compare regression before and after ========
# Simple Linear Regression Original Data:
summary(lm(Sleep ~ Dream, data = x))

# Simple Linear Regression kNN Imputed Data:
summary(lm(Sleep ~ Dream, data = x_imputed))

# Example of multiple missing imputation method (using `mice`) ======

# load mice package
# use mice() function 
# - set random seed for reproducibility
# - generate 20 imputed sets
# - do not print out everything
# - save output in imp object
# save 20 regression models output in fit
# pool these 20 regression models and view results
library(mice)
imp <- mice(x, seed = 1, 
            m = 20, 
            print = FALSE)
fit <- with(imp, lm(Sleep ~ Dream))
summary(pool(fit))

# look at one of the regression models from the impute data
# look at the 1st model from the 1st imputed dataset
# see simple output
fit[["analyses"]][[1]]

# get summary detailed output
summary(fit[["analyses"]][[1]])



