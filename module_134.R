# Module 1.3.4 Code =======================================
# Section 0. Pre-work =====================================
# Make sure you have these packages installed
#   - ggplot2
#   - dplyr
#   - patchwork
#   - ggpubr
#   - GGally
#   - vcd
#   - gapminder (optional)
#   - gganimate (optional)
#   - plotly (optional)
#   - gt (optional)
#   - gtExtras (optional)

# load mydata.RData from the end of Module 1.3.2
load(file = "mydata.RData")


# notes
# mice pkg https://amices.org/mice/
# book, https://stefvanbuuren.name/fimd/foreword.html
# nanair, https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# https://www.datawim.com/post/missing-data-visualization-in-r/
# 
# https://libguides.princeton.edu/R-Missingdata
# 
# https://cran.r-project.org/web/packages/mice/index.html
# 
# https://cran.r-project.org/web/views/MissingData.html
# 
# https://cran.r-project.org/web/views/MissingData.html
# 
# https://rmisstastic.netlify.app/
#   
#   https://rmisstastic.netlify.app/tutorials/josse_tierney_bookdown_user2018tutorial_2018
# 
# https://modelsummary.com/vignettes/datasummary.html
# 
# https://dabblingwithdata.amedcalf.com/2018/01/02/my-favourite-r-package-for-summarising-data/
#   
#   https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html
# 
# https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html













# Section 1. Base R Graphical Functions ===================
# Base R - Scatterplot ====================================
plot(x = mydata$Height,
     y = mydata$WeightPRE)

# Add axis labels, main title and subtitle
plot(x = mydata$Height,
     y = mydata$WeightPRE,
     xlab = "Height (in decimal inches)",
     ylab = "Weight (in pounds) - before intervention",
     main = "Weight by Height in the Mydata Project",
     sub = "Hypothetical Madeup mydata Dataset")

# Add colors with col 
# and change plot characters with pch
# and add a legend
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
# this new dataset will have the 
# corrected values and updates
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

# build a plot step by step
# and add layers using geoms and more
# create the plot space
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST))

# next add + geom_point() layer to "see" the points
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST)) +
  geom_point()

# add color by gender
# notice this automatically adds a legend
ggplot(data = mydata_corrected,
       aes(x = WeightPRE,
           y = WeightPOST,
           color = GenderCoded.f)) +
  geom_point()

# add labels, title, legend title using labs()
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

# Also for WeightPOST, for
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
# make histogram of Age
ggplot(data = mydata_corrected, 
       aes(x = Age)) +
  geom_histogram()

# add color
# use fill for the bar color
# use color for the bar outline color
ggplot(mydata_corrected, 
       aes(x = Age)) +
  geom_histogram(fill = "lightblue",
                 color = "black")

# to add density curve
# update aes with after_stat(density) to get 
# probabilities instead of frequencies (counts)
# add geom_density line in a different color, red
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

# filter out the one person missing SES
# using dplyr filter() and pipe %>%
# the results into ggplot()
# and then make the ggplot
library(dplyr)

mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected)) +
  geom_boxplot()

# add fill colors by SES category
# notice the legend that gets created
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  ggplot(aes(x = SES.f, 
             y = WeightPRE_corrected,
             fill = SES.f)) +
  geom_boxplot()

# add better axis labels, title and subtitle
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
# use geom_jitter() and change the jitter
# height and width a little
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
# use position = "dodge" to put
# the bars side-by-side otherwise
# they will show up as stacked by default
mydata_corrected %>%
  filter(!is.na(SES.f)) %>%
  filter(!is.na(GenderCoded.f)) %>%
  ggplot(aes(x = SES.f,
             fill = GenderCoded.f)) +
  geom_bar(position = "dodge")

# add custom colors using scale_fill_manual()
# fill is the inside color
# and color is the border color
# and better labels
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
# looking a t-test output
tt1 <- t.test(mydata_corrected$Height_corrected, 
              conf.level = 0.95)

# look at the output in the console
tt1

# look at the complete structure
# of the tt1 t-test output object
str(tt1)

# select the conf.int part of tt1
tt1$conf.int

# look at the lower and upper limits
# get each value separately
tt1$conf.int[1] # for the lower ci limit
tt1$conf.int[2] # for the upper ci limit

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
# lineplot with errorbars
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
# save plot objects and reuse or rearrange them ===========
# make the scatterplot, save as plot object p1

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
# to get more details - notice you get
# bar charts for the counts of gender
# boxplots by gender
# and stacked histograms by gender
ggpairs(mydata_corrected,
        mapping = aes(color = GenderCoded.f),
        columns = c("GenderCoded.f",
                    "Height_corrected", 
                    "WeightPRE_corrected", 
                    "WeightPOST_corrected"),
        lower = list(continuous = "smooth"))

# Visualize Categorical Data with vcd package  ============
# Let’s visualize the relative proportions of gender 
# and SES using the vcd::mosaic() function.
library(vcd)

vcd::mosaic(GenderCoded.f ~ SES.f, 
            data = mydata_corrected,
            gp = gpar(fill = c("gray","dark magenta")),
            main = "Gender and SES",
)

# Example of an animated graph with gganimate =============
# if you would like to try this
library(gapminder)
library(gganimate)

ggplot(gapminder, 
       aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per capita', 
       y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Interactive Graphics with plotly ========================
library(plotly)

fig <- plot_ly(mydata_corrected, 
               x = ~WeightPRE_corrected, 
               color = ~SES.f, 
               type = "box",
               orientation = "h")
fig

# Section 4. Summary Tables with Graphics =================
# example of a summary table with graphics ================
library(gtExtras)

mydata_corrected %>%
  select(Height_corrected,
         WeightPRE_corrected,
         WeightPOST_corrected,
         GenderCoded.f,
         SES.f) %>%
  gt_plt_summary()






