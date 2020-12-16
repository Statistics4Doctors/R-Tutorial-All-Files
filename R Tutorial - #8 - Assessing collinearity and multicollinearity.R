# R Tutorial - #8 - Assessing collinearity and multicollinearity

# By: Aaron Prosser, MD MSc | Last edited: 7-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Assessing collinearity and multicollinearity

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("car")
install.packages("psych")
install.packages("corrplot")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(car)
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



# -------- #
#  Part 1  # Assessing collinearity and multicollinearity
# -------- #
# Test collinearity via zero-order correlations:
m <- corr.test(x=dat, method="pearson") 
round(m$r, digits=3) # Round the correlations to the 3rd decimal place
# Plot the correlation matrix
corrplot(m$r, type="lower", method=c("color"))

# Test multicollinearity via VIF and Tolerance: 

# As you may know, 

# 1) Variance inflation factor (VIF): inflation of the variances (and thus SE) 
#    of the parameter estimates due to correlations among the predictors 
#    in the model.

#    VIF = 1/Tolerance

#    If a predictor's VIF ≥ 5 or 10, then multicollinearity present.

# 2) Tolerance: % of variance in the predictor which cannot be accounted for 
#    by other predictors.

#    Tolerance = 1/VIF

#    If a predictor’s Tolerance ≤ 0.20 or 0.10, then multicollinearity is present. 

m.all <- lm(Aggression ~ Parenting_Style + Video_Games + 
              Sibling_Aggression + Television + Diet, data=dat)

VIF <- vif(m.all)
Tolerance <- 1/VIF
cbind(VIF, Tolerance)