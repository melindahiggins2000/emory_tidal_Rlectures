# Module 1.3.5 Code =======================================
# Section 0. Pre-work =====================================
# Make sure you have these packages installed
# - VIM
# - gtsummary
# - easystats
# - broom
# - car
# - effects
# - olsrr
# - dplyr
# - ROCR
# - effectsize

## Session Objectives _(updated)_
# 
# 1. Develop linear regression models and explore results.
# 2. Develop logistic regression models and explore results.
# 3. Perform t-tests and ANOVA and explore results.
# 4. Modeling with Complex Survey Weights

# NOTE: Built-in datasets will be used for this module, so no new data needs to be loaded here.

# Section 1. Develop linear regression models, explore results ======
#Linear Regression Modeling =========================================

# load VIM Package to get sleep dataset
library(VIM)

# run model for predicting "Sleep" from "Dream"
# save the output in lm1
lm1 <- lm(Sleep ~ Dream, data = sleep)

# look at default output
lm1

# save the summary of lm1
slm1 <- summary(lm1)

# look at default output
slm1

# Nicer formatted regression table using `gtsummary::tbl_regression` ====

library(gtsummary)
tbl_regression(lm1)

# More output options for regression using `easystats` ==============

library(easystats)
model_parameters(lm1)

report(lm1)

# Other options within `tidyverse` packages =========================

library(broom)
tidy(lm1)
glance(lm1)

# The car and effects packages ======================================

library(car)

# get normal probability plot of the 
# regression model residuals
car::qqPlot(lm1$residuals)

# scatterplot of fitted model
# using the car package, add the smooth option
car::scatterplot(Sleep ~ Dream, data = sleep, 
                 smooth=TRUE)

library(effects)
plot(allEffects(lm1))

# The olsrr package =================================================

# load olsrr package
library(olsrr)

# get detailed regression output
# including standardized coefficients
ols_regress(lm1)

# diagnostic plots
# check for new output windows
ols_plot_diagnostics(lm1)

# normality tests for residuals
ols_test_normality(lm1)

# Section 2. Develop logistic regression models and explore results =====

# Simple glm() output ===============================================

# create outcome variable
sleep$dream_gt2 <- as.numeric(sleep$Dream > 2)

# fit the logistic regression model
glm1 <- glm(dream_gt2 ~ Sleep + Danger,
            data = sleep,
            family = "binomial")

# look at basic output
glm1

# get odds ratios
exp(coef(glm1))

# detailed output
sglm1 <- summary(glm1)
sglm1

# nicer table
tbl_regression(glm1, exponentiate = TRUE)

# AUC and ROC curve plot ============================================

# NOTE: We need only the COMPLETE data
# for the 3 variables we used in this model
library(dplyr)
s1 <- sleep %>%
  select(Sleep, Danger, dream_gt2) %>%
  filter(complete.cases(.))

# compute AUC and get ROC curve
library(ROCR)
p <- predict(glm1, newdata=s1, 
             type="response")
pr <- prediction(p, as.numeric(s1$dream_gt2))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# compute AUC, area under the curve
# also called the C-statistic
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

# also - add title to plot with AUC in title
plot(prf,
     main = paste("ROC Curve, AUC = ", round(auc, 3)))
abline(0, 1, col="red")

# Section 3. Perform t-tests explore results. =======================

# T-tests ===========================================================

# run t-test, save results
# default is an unequal variance "unpooled" t.test
tt1 <- t.test(Sleep ~ dream_gt2, 
              data = sleep)
tt1

# get the equal variance "pooled" t.test
tt2 <- t.test(Sleep ~ dream_gt2, 
              data = sleep,
              var.equal = TRUE)
tt2

# look at SDs - compare for equal variance
sd0 <- sleep %>%
  filter(dream_gt2 == 0) %>%
  select(Sleep) %>%
  unlist() %>%
  sd(na.rm = TRUE)
sd1 <- sleep %>%
  filter(dream_gt2 == 1) %>%
  select(Sleep) %>%
  unlist() %>%
  sd(na.rm = TRUE)
sd0
sd1

# perform Bartlett test for equal variance
bartlett.test(Sleep ~ dream_gt2, 
              data = sleep)

# get effect size Cohen's d for t-test
library(effectsize)
options(es.use_symbols = TRUE)

cohens_d(Sleep ~ dream_gt2, 
         data = sleep,
         na.action = na.omit)

# Get simple summary stats table with
# t-test comparison test for Sleep
# and Mann Whitney/Wilcoxon Rank Sum 
# test for Danger variable

# create factor variable with labels
sleep$dream_gt2.f <- factor(
  sleep$dream_gt2, 
  levels = c(0, 1),
  labels = c("Dream <= 2",
             "Dream > 2")
)

tbl_summary(
  sleep,
  by = dream_gt2.f,
  include = c(Sleep, Danger),
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})")
) %>%
  add_p(test = list(Sleep ~ "t.test", Danger ~ "wilcox.test"), 
        test.args = list(Sleep ~ list(var.equal = TRUE))
  )





