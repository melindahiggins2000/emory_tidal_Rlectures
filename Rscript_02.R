# R Script 02 =================================
# Create some data
age <- c(34, 23, 54, 38, 40)
ht_in <- c(66, 71, 74, 70, 68)
wt_lbs <- c(130, 160, 210, 180, 200)
bmi <- (703 * wt_lbs) / ((ht_in) * (ht_in))
favColor <- c("blue","yellow","red","red","blue")
bmi_gt30 <- bmi > 30

# What type of data is bmi?
class(bmi)

# What type of data is bmi_gt30?
class(bmi_gt30)

# What type of data is favColor?
class(favColor)

# let's put these together
# in a data.frame
df <- data.frame(age,
                 favColor,
                 ht_in,
                 wt_lbs,
                 bmi,
                 bmi_gt30)

# look at it
df

# look at it in the viewer
View(df)

# or click on it in GLOBAL ENVIRONMENT viewer

# get details
str(df)
dim(df)

# look at top 6 rows which is whole dataset
head(df)

# loo at bottom 2 rows
tail(df, 2)

# select the age column using $, get the mean
mean(df$age)

# mean() function is in the base package
help(mean)

# how would you find the median of age?
# hint help(median)
median(df$age)


# what about the standard deviation of age?
# click on Packages, find stats package
# in stats package, type in "standard" to find sd()
sd(df$age)



# plot x=ht_in and y=wt_lbs
# plot both
# plot favorite colors
plot(x=df$ht_in, 
     y=df$wt_lbs, 
     pch=16,
     col = df$favColor)

# pch is plotting character
# learn more, help(points)


# take a quick look at your session
sessionInfo()

