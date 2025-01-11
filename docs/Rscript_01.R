# R Script 01 =================================
# Console Commands Line Examples ==============
4 + 4
sqrt(25)
pi
seq(from=1, to=10, by=0.5)

# What does the seq() function do?
# Go to help tab
# or in console, type help(seq)



# Click on the ENVIRONMENT TAB
# Notice it is empty


# EXERCISE 01 =================================
# How to save the output?
# save sequence of numbers in object x
x <- seq(from=1, to=10, by=0.5)

# view the contents of x
x

# Also take a look at the ENVIRONMENT TAB

# use x to create new object y
y <- x*x

# plot x and y
plot(x,y)

# Where did the plot go?
# Go to the Plots window - explore options



# EXERCISE 02 =================================
# create a new object cosx
cosx <- cos(x)

# plot x, cosx
plot(x, cosx)

# Check ENVIRONMENT TAB
# How many objects are there now?



# use functions as needed on the fly!
# plot x, sin(x)
plot(x, sin(x))
plot(y=sin(x), x=x)     # order doesn't matter with explicit assignment

# plot both points and lines
# make the color red
plot(x, sin(x), type = "both", col = "red")
plot(x, sin(x), type = "b", col = "red")

# Check ENVIRONMENT TAB
# How many objects are there now?



# [YOUR TURN] plot x and the tangent of x
# change color to blue
# hint: help(sin) - to see list of other trig functions
# hint: cut and paste code above to help you
plot(x, tan(x), type = "b", col = "blue")

# [YOUR TURN] plot log of x with log y
# change color to green
# hint: help(log) - to see list of log functions
plot(log(x), log(y), type = "b", col = "green")

