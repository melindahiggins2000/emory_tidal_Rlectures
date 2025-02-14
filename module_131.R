# Module 1.3.1 Code =============================
# Section 2. Write simple R code in Console =====

# type code into Console
5 + 5
.Last.value

# save output in an object
ten <- 5 + 5
ten

# built in constants
pi

# notice the index counter [#]
letters
LETTERS
month.name

# get details on type and size
class(letters)
length(letters)

# get help
# look in help window
# or type in Console
help(pi, package = "base")

# if don'tknow package
# run wide search
??plot

# try built-in function
seq(10)

# get help
# see arguments, details and 
# see value (the output)
help(seq, package = "base")

seq(from = 1,
    to = 10,
    by = 1)

# we can use lazy coding
seq(1, 10, 0.1)

# or we can change the order
# using explicit arguments
seq(by = 0.1,
    from = 0,
    to = 5)

# Section 4. Install and load R packages ========
# get R session details
sessionInfo()

# [1] install ggplot2
install.packages("ggplot2")

# try using ggplot - but
# I have not yet loaded ggplot2 into the session
# try the ggplot() function with the
# built-in pressure dataset to see error
ggplot(pressure, aes(temperature, pressure)) +
  geom_point()

# [2] Load ggplot2 package into session
library(ggplot2)

# now check session
sessionInfo()

# try ggplot code again
ggplot(pressure, aes(temperature, pressure)) +
  geom_point()

