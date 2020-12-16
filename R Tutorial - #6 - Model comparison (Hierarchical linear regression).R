# R Tutorial - #6 - Model comparison (Hierarchical linear regression)

# By: Aaron Prosser, MD MSc | Last edited: 7-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Model comparison (Hierarchical linear regression)

# Install these packages, if you haven't already:
install.packages("openxlsx")

# Load the packages we're using in this tutorial:
library(openxlsx)

# Disable scientific notation:
options(scipen=999, digits=4)

# Set directory:
dir <- "C:/Users/Aaron/iCloudDrive/Projects/Statistics for Doctors/R Tutorial/"

# Import data and create a data frame:
file <- "ChildAggression_Excel.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
dat <- as.data.frame(import)



# -------- #
#  Part 1  # Model comparison (Hierarchical linear regression)
# -------- #
m0 <- lm(Aggression ~ 1, data=dat)
m1 <- lm(Aggression ~ Parenting_Style, data=dat)
m2 <- lm(Aggression ~ Parenting_Style + Video_Games, data=dat)
m3 <- lm(Aggression ~ Parenting_Style + Video_Games + Sibling_Aggression, data=dat)

# Get the R-squared of the reduced vs. full model
reduced <- m0
full <- m1
summary(reduced)
summary(full)

# Compare the full model to the reduced model:
anova(reduced, full)