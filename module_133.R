# Module 1.3.3 Code =======================================
# Section 0. Pre-work =====================================
# Make sure you have these packages installed
#   - ggplot2
#   - ggthemes
#   - readr
#   - dplyr

# load mydata.RData from the end of Module 1.3.2
load(file = "mydata.RData")

# Section 1. Base R Graphical Functions ===================
# Base R - Scatterplot ====================================
plot(x = mydata$Height,
     y = mydata$WeightPRE)

# Add Labels
plot(x = mydata$Height,
     y = mydata$WeightPRE,
     xlab = "Height (in decimal inches)",
     ylab = "Weight (in pounds) - before intervention",
     main = "Weight by Height in the Mydata Project",
     sub = "Hypothetical Madeup mydata Dataset")

# Add colors and a legend
plot(x = mydata$Height,
     y = mydata$WeightPRE,
     col = c("blue", "green")[mydata$GenderCoded],
     pch = c(15, 19)[mydata$GenderCoded],
     xlab = "Height (in decimal inches)",
     ylab = "Weight (in pounds) - before intervention",
     main = "Weight by Height in the Mydata Project",
     sub = "Hypothetical Madeup mydata Dataset")
legend(3, 250, legend=c("Male", "Female"),
       col=c("blue", "green"), pch = c(15, 19), cex=0.8)

# Base R - Histogram ======================================

hist(mydata$Height,
     xlab = "Height (in decimal inches)",
     col = "lightblue",
     border = "black",
     main = "Histogram of Heights",
     sub = "Hypothetical Madeup mydata Dataset")

# see list of built-in colors in R
colors()

# Add density curve to histogram
# make histogram as we did above
# add freq = FALSE
hist(mydata$Height,
     freq = FALSE,
     xlab = "Height (in decimal inches)",
     col = "lightblue",
     border = "black",
     main = "Histogram of Heights",
     sub = "Hypothetical Madeup mydata Dataset")

# add density curve line
# add na.rm=TRUE to remove 
# the missing values in Height
lines(density(mydata$Height, na.rm=TRUE),
      col = "black")

# Data Wrangling - Fix the Heights
# 2 people are under 4 feet tall
# use dplyr::arrange()
library(dplyr)

mydata %>%
  select(SubjectID, Height) %>%
  arrange(Height) %>%
  head()

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

# Remake histogram
# make histogram as we did above
# add freq = FALSE
hist(mydata_corrected$Height_corrected,
     freq = FALSE,
     xlab = "Height (in decimal inches)",
     col = "lightblue",
     border = "black",
     main = "Histogram of Heights",
     sub = "Hypothetical Madeup mydata Dataset")

# add density curve line
# add na.rm=TRUE to remove 
# the missing values in Height
lines(density(mydata_corrected$Height_corrected, na.rm=TRUE),
      col = "black")

# Base R - Barchart =======================================
# Barchart of the 3 SES categories
# get table of frequencies for each category
tab1 <- table(mydata_corrected$SES.f)

opar <- par() # save current plotting parameters
par(lwd = 3) # change border linewidth

# make plot of the frequencies for 
# each category
barplot(tab1,
        xlab = "SES Categories",
        ylab = "Frequencies",
        col = "#f7f445",
        border = "darkgreen",
        main = "Socio Economic Status Categories",
        sub = "Hypothetical Madeup mydata Dataset")

par(opar) # reset plotting parameters to defaults

# Base R - Boxplot ========================================
# Make side-by-side boxplots of heights by gender
boxplot(Height_corrected ~ GenderCoded.f,
        data = mydata_corrected,
        xlab = "Gender",
        ylab = "Height (in decimal feet)",
        col = "#f58ef1",
        border = "darkmagenta",
        main = "Height by Gender",
        sub = "Hypothetical Madeup mydata Dataset")

# Section 2. The ggplot2 package ==========================
# ggplot2 - Scatterplot ===================================
#load ggplot2
library(ggplot2)

# create the plot space
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST))

# add geom_point() to "see" the points
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST)) +
  geom_point()

# add color by gender
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST,
           color = GenderCoded.f)) +
  geom_point()

# add labels, title, legend title
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST,
           color = GenderCoded.f)) +
  geom_point() +
  xlab("Weight (in pounds) before program") +
  ylab("Weight (in pounds) after program") +
  labs(
    title = "Weights (in pounds) before and after",
    subtitle = "Hypothetical Madeup mydata Dataset",
    color = "Gender"
  ) 

# notice the outliers due to weight PRE
# in kg instead of lbs - correct these
# for WeightPRE < 100, convert kg to lbs
mydata_corrected <- mydata_corrected %>%
  mutate(WeightPRE_corrected = case_when(
    (WeightPRE < 100) ~ WeightPRE * 2.20462,
    .default = WeightPRE
  ))

# For WeightPOST, for
# SubjectID 28, change WeightPOST=98 to 198
# since this person's WeightPRE was 230.
# also fix SubjectID = 32, for
# WeightPOST from 109 to 209 since
# their WeightPRE was 260

mydata_corrected <- mydata_corrected %>%
  mutate(WeightPOST_corrected = case_when(
    (SubjectID == 28) ~ 198,
    (SubjectID == 32) ~ 209,
    .default = WeightPOST
  ))

# redo plot with corrected values
# add reference line
# and custom colors and custom shapes
ggplot(data = mydata_corrected,
       aes(x = WeightPRE_corrected,
           y = WeightPOST_corrected,
           color = GenderCoded.f,
           shape = GenderCoded.f)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "red") +
  scale_shape_manual(values = c(16, 17),
                     na.value = 15) +
  scale_color_manual(values = c("blue", 
                                "magenta"),
                     na.value = "grey30") +
  xlab("Weight (in pounds) before program") +
  ylab("Weight (in pounds) after program") +
  labs(
    title = "Weights (in pounds) before and after",
    subtitle = "Hypothetical Madeup mydata Dataset",
    color = "Gender",
    shape = "Gender"
  ) 

# ggplot2 - Histogram =====================================
ggplot(data = mydata_corrected, 
       aes(x = Age)) +
  geom_histogram()

# add color
ggplot(mydata_corrected, 
       aes(x = Age)) +
  geom_histogram(fill = "lightblue",
                 color = "black")

# to add density curve
# update aes with after_stat() to get probabilities
# instead of frequencies (counts)
# add geom_density in a different color
# add axis labels and titles
ggplot(mydata_corrected, 
       aes(x = Age,
           y = after_stat(density))) +
  geom_histogram(fill = "lightblue",
                 color = "black") +
  geom_density(color = "red") +
  xlab("Age (in years)") +
  ylab("Proportion") +
  labs(
    title = "Ages for Participants",
    subtitle = "Hypothetical Madeup mydata Dataset"
  ) 

# ggplot2 - Boxplot =======================================
# boxplot of PRE weights by SES categories
ggplot(data = mydata_corrected,
       aes(x = SES.f,
           y = WeightPRE_corrected)) +
  geom_boxplot()

# filter out the person missing SES
# and then make the ggplot
library(dplyr)

mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected)) +
  geom_boxplot()

# add fill colors
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected,
             fill = SES.f)) +
  geom_boxplot()

# add better labels
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected,
             fill = SES.f)) +
  geom_boxplot() +
  xlab("Socio-Economic Status Categories") +
  ylab("Weight (in pounds) before program") +
  labs(
    title = "Weights by SES Categories",
    subtitle = "Hypothetical Madeup mydata Dataset"
  ) 

# add another layer on top with points
# use geom_jitter()
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected,
             fill = SES.f)) +
  geom_boxplot() +
  geom_jitter(height=0, 
              width=.10) +
  xlab("Socio-Economic Status Categories") +
  ylab("Weight (in pounds) before program") +
  labs(
    title = "Weights by SES Categories",
    subtitle = "Hypothetical Madeup mydata Dataset",
    fill = "SES Categories"
  ) 

# try a variation of geom_boxplot()
# change to geom_violin()
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected,
             fill = SES.f)) +
  geom_violin(bw=10) +
  xlab("Socio-Economic Status Categories") +
  ylab("Weight (in pounds) before program") +
  labs(
    title = "Weights by SES Categories",
    subtitle = "Hypothetical Madeup mydata Dataset"
  ) 

# ggplot2 - Barchart ======================================
# make barchart of frequencies of SES categories
# filter out subject that is missing SES 
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f)) +
  geom_bar()

# add clustering by gender with SES
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  filter(!is.na(GenderCoded.f)) %>%
  ggplot(aes(x = SES.f,
             fill = GenderCoded.f)) +
  geom_bar(position = "dodge")

# add custom colors and better labels
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  filter(!is.na(GenderCoded.f)) %>%
  ggplot(aes(x = SES.f,
             fill = GenderCoded.f)) +
  geom_bar(position = "dodge", 
           color = "black") +
  scale_fill_manual(values = c("blue", 
                               "magenta")) +
  xlab("Socio-Economic Status Categories") +
  ylab("Frequency") +
  labs(
    title = "Frequencies of SES Categories by Gender",
    subtitle = "Hypothetical Madeup mydata Dataset",
    fill = "Gender"
  )

# ggplot2 - Errorbar Plots ================================
# make plot of the means of the heights by gender
# and get the 95% confidence intervals
# by running a t-test and getting these numbers
# create a NEW data object dt
# capture the means of the correct heights 
# and get the 95% confidence intervals
# upper bound and lower bound by gender
# filter out the missing GenderCoded.f
dt <- mydata_corrected %>%
  filter(!is.na(GenderCoded.f)) %>%
  dplyr::group_by(GenderCoded.f)%>%
  dplyr::summarise(
    mean = mean(Height_corrected, na.rm = TRUE),
    lci = t.test(Height_corrected, 
                 conf.level = 0.95)$conf.int[1],
    uci = t.test(Height_corrected, 
                 conf.level = 0.95)$conf.int[2])
dt

# use dt to make the barplot with error bars on top
ggplot(data = dt) +
  geom_bar(aes(x = GenderCoded.f, 
               y = mean, 
               fill = GenderCoded.f), 
           color = "black",
           stat="identity") +
  scale_fill_manual(values = c("blue", 
                               "magenta")) + 
  geom_errorbar(aes(x = GenderCoded.f, 
                    ymin = lci, 
                    ymax = uci), 
                width = 0.4, 
                color ="black", 
                size = 1) +
  xlab("Gender") +
  ylab("Mean Height (in decimal feet)") +
  labs(
    title = "Average Heights by Gender",
    subtitle = "Hypothetical Madeup mydata Dataset",
    caption = "Error Bars Represent 95% Confidence Intervals",
    fill = "Gender"
  )

# make a different variation
# make an errorbar plot connected by a line
ggplot(data = dt) +
  geom_point(aes(x = GenderCoded.f, 
                 y = mean,
                 color = GenderCoded.f),
             size = 3) + 
  geom_errorbar(aes(x = GenderCoded.f, 
                    ymin = lci, 
                    ymax = uci,
                    color = GenderCoded.f), 
                width = 0.4, 
                size = 1.5) +
  geom_line(aes(x = GenderCoded.f, 
                y = mean), 
            group = 1,
            size = 1.5,
            color = "black") +
  scale_color_manual(values = c("blue", 
                                "magenta")) + 
  xlab("Gender") +
  ylab("Mean Height (in decimal feet)") +
  labs(
    title = "Average Heights by Gender",
    subtitle = "Hypothetical Madeup mydata Dataset",
    caption = "Error Bars Represent 95% Confidence Intervals",
    color = "Gender"
  )

# ggplot2 - Lollipop Plots ================================
# useful for visualizing PRE-to-POST changes in time
# code adapted from 
# https://r-graph-gallery.com/303-lollipop-plot-with-2-values.html

# sort data by WeightPRE_corrected ascending
data <- mydata_corrected %>%
  rowwise() %>%
  arrange(WeightPRE_corrected) %>%
  mutate(SubjectID = factor(SubjectID, SubjectID))

# Plot
ggplot(data) +
  geom_segment(aes(x = SubjectID,
                   xend = SubjectID,
                   y = WeightPRE_corrected,
                   yend = WeightPOST_corrected), 
               color = "grey30") +
  geom_point(aes(x = SubjectID, 
                 y = WeightPRE_corrected,
                 color = "WeightPRE_corrected"),
             size = 3) +
  geom_point(aes(x = SubjectID, 
                 y = WeightPOST_corrected,
                 color = "WeightPOST_corrected"),
             size = 3) +
  scale_color_manual(
    labels = c("PRE", "POST"),
    values = c("coral","darkblue"),
    guide  = guide_legend(), 
    name   = "Group") +
  coord_flip() +
  theme(legend.position = "bottom") +
  xlab("Subject IDs") +
  ylab("Weight Change (in pounds) PRE to POST")

# Section 3. Other Graphics Packages to Know ==============
# save plot objects and reuse or rearrange them
# make the scatterplot, save as p1

p1 <- ggplot(
  data = mydata_corrected,
  aes(
    x = WeightPRE_corrected,
    y = WeightPOST_corrected,
    color = GenderCoded.f,
    shape = GenderCoded.f
  )) +
  geom_point(size = 2) +
  geom_abline(slope = 1,
              intercept = 0,
              color = "red") +
  scale_shape_manual(values = c(16, 17), na.value = 15) +
  scale_color_manual(values = c("blue", "magenta"),
                     na.value = "grey30") +
  xlab("Weight (in pounds) before program") +
  ylab("Weight (in pounds) after program") +
  labs(
    title = "Weights (in pounds) before and after",
    subtitle = "Hypothetical Madeup mydata Dataset",
    color = "Gender",
    shape = "Gender"
  )

# make the histogram, save as p2

p2 <- ggplot(mydata_corrected, 
             aes(x = Age, 
                 y = after_stat(density))) +
  geom_histogram(fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  xlab("Age (in years)") +
  ylab("Proportion") +
  labs(title = "Ages for Participants", 
       subtitle = "Hypothetical Madeup mydata Dataset")

# make the barplot, save as p3

p3 <- mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  filter(!is.na(GenderCoded.f)) %>%
  ggplot(aes(x = SES.f,
             fill = GenderCoded.f)) +
  geom_bar(position = "dodge", 
           color = "black") +
  scale_fill_manual(values = c("blue", 
                               "magenta")) +
  xlab("Socio-Economic Status Categories") +
  ylab("Frequency") +
  labs(
    title = "Frequencies of SES Categories by Gender",
    subtitle = "Hypothetical Madeup mydata Dataset",
    fill = "Gender"
  )

# patchwork package =======================================
# use patchwork to make a composite plot
# using the 3 plot objects saved above
# load patchwork package
library(patchwork)

# put p1 and p2 side-by-side
# and put both of these on top of p3
(p1 + p2) / p3

# ggpubr package and ggarrange() function  ================
# load ggpubr package
library(ggpubr)

# use ggarrange twice
# put p1 and p2 side by side
# then put on top of p3
ggarrange(
  ggarrange(p1, p2, widths = c(1, 1)),
  p3, nrow = 2, ncol = 1)

# GGally package and ggpairs() function  ==================
# make matric scatterplots
# get correlations
# add best fit "smooth" line to each plot
# to see trends in correlations between pairs of variables
library(GGally)

ggpairs(mydata_corrected,
        columns = c("Height_corrected", 
                    "WeightPRE_corrected", 
                    "WeightPOST_corrected"),
        lower = list(continuous = "smooth"))

# add color and groupings by gender
# useful to see if gender moderates
# or changes these slopes or correlations
ggpairs(mydata_corrected,
        mapping = aes(color = GenderCoded.f),
        columns = c("Height_corrected", 
                    "WeightPRE_corrected", 
                    "WeightPOST_corrected"),
        lower = list(continuous = "smooth"))

# add gender to the variable list
# to get more details
ggpairs(mydata_corrected,
        mapping = aes(color = GenderCoded.f),
        columns = c("GenderCoded.f",
                    "Height_corrected", 
                    "WeightPRE_corrected", 
                    "WeightPOST_corrected"),
        lower = list(continuous = "smooth"))





