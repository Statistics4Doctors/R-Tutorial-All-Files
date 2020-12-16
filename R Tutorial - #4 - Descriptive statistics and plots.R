# R Tutorial - #4 - Descriptive statistics and plots

# By: Aaron Prosser, MD MSc | Last edited: 7-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Descriptive statistics
# Part 2 - Basics of plotting with ggplot

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("psych")
install.packages("corrplot")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(ggplot2)
library(psych)
library(corrplot)

# Disable scientific notation:
options(scipen=999, digits=4)

# Set directory:
dir <- "C:/Users/Aaron/iCloudDrive/Projects/Statistics for Doctors/R Tutorial/"

# Import data and create a data frame:
file <- "ChildAggression_Excel.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
dat <- as.data.frame(import)

# Add the observation IDs since we need them for some plots
Child <- seq(from=1, to=length(dat$Aggression), by=1)
dat$Child <- Child



# -------- #
#  Part 1  # Descriptive statistics
# -------- #

# Descriptive statistics of all the variables in the data frame, except the 7th ID column
describe(dat[,1:6], IQR=TRUE) # Mean (+ trimmed mean), sd, median, range, skew, kurtosis

# Pearson Correlation matrix of all the variables
# I use 'm' as a generic variable for any "model" or statistical test
m <- corr.test(x=dat[,1:6], method="pearson") 
round(m$r, digits=3) # Round the correlations to the 3rd decimal place
round(m$p, digits=3) # Round the p-values to the 3rd decimal place
# Plot the correlation matrix
corrplot(m$r, type="lower", method=c("color"))



# -------- #
#  Part 2  # Basics of plotting with ggplot
# -------- #                                               

# Links to tutorials: https://ggplot2.tidyverse.org/
# Reference: https://ggplot2.tidyverse.org/reference/
# Graphical parameters: https://www.statmethods.net/advgraphs/parameters.html

# ggplot() creates a new ggplot plot. 

# Using the ggplot() function, you can define which data (x, y) you're going 
# to plot. It will then apply this to all the 'layers' of the graph. These layers
# are defined by the 'geoms' ("geometric objects") listed after ggplot(). 

# Alternatively, you can define the data within each layer. The advantage of this
# is that you can plot different data frames within the same plot, allowing
# you to make complicated graphs.

# "aes" = aesthetic mappings. The x and y arguments of aes() define which
# variables to map to the x and y axes. ggplot looks for these variables in
# the data argument (in the case "data=dat"). In aes() you may also map graphical
# properties (e.g., color) to the variables you're plotting.

# Histogram of the outcome variable
ggplot(data=dat, aes(x=Aggression)) + 
  # Set the X and Y axis labels
  ylab('Frequency') +
  xlab('Child Aggression') +
  # Plot the histogram. You can adjust the bin width parameter
  geom_histogram(color="black", fill="white", binwidth=0.05)

# Scatter plot to visualize each child's score on the outcome variable
ggplot(data=dat, aes(x=Child, y=Aggression)) + 
  # Set the axis limits
  coord_cartesian(ylim=c(0,5), xlim=c(0,666)) + 
  # Set the location of the zero point using expand() and the breaks for each axis using breaks()
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=5, by=0.5)) + 
  scale_x_continuous(expand=c(0, 0), breaks=seq(from=0, to=666, by=50)) +
  xlab("Child") +
  ylab("Child Aggression") +
  # Plot the points. "shape=1" = hollow circles
  geom_point(shape=1) 

# Box and whisker plot
ggplot(data=dat, aes(x=Child, y=Aggression)) +
  coord_cartesian(ylim=c(0,5),xlim=c(0,666)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=5, by=0.5)) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(from=0, to=666, by=50)) +
  xlab("Child") +
  ylab("Child Aggression") +
  geom_point(shape=1) +
  # Plot the Box and whisker plot. Alpha=transparency parameter.
  # Alpha range: 0 (transparent) to 1 (not transparent)
  geom_boxplot(colour="Black", outlier.alpha=0)