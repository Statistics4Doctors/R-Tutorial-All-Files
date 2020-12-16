# R Tutorial - #7 - Selecting predictors for multiple linear regression

# By: Aaron Prosser, MD MSc | Last edited: 10-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Selecting predictors for multiple linear regression

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("olsrr")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(olsrr)

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
#  Part 1  # Selecting predictors for multiple linear regression
# -------- #

# The Exponential Problem of Model Selection: 

# There are 2^k-1 distinct models for any set of predictors (k). - 1 because
# we typically ignore the intercept-only model.

# So, there are 2^5-1 = 31 distinct models we can choose from to analyze
# the Child Aggression Study.

# There are three main methods to select predictors:

# 1) 1-in-10 rule: 
#   The # of predictors (k) in your model should be ≤ n/10, 
#   where n = the total number of observations.
#   Otherwise, you risk over-fitting your data.

# 2) Criterion-based methods:
#   - t-tests of parameter estimates
#   - Model comparison using the F-test
#   - R-squared
#   - Mallow's Cp
#   - Information criteria (e.g., AIC, BIC)

# 3) Stepwise procedures:
#   - Forward selection
#   - Backward selection
#   - Sequential replacement

# Package: https://cran.r-project.org/web/packages/olsrr/olsrr.pdf

m.all <- lm(Aggression ~ Parenting_Style + Video_Games + 
                       Sibling_Aggression + Television + Diet, data=dat)

# Criterion-based methods:
all.mod <- ols_step_all_possible(model=m.all) # Criteria for all possible model combinations
# ols_step_all_possible() generates other criteria not shown:
str(all.mod) 
# Visualize the models as they rank in terms of each criteria:
plot(all.mod) 

# Criteria of the best subset of models:
best.mod <- ols_step_best_subset(model=m.all)
str(best.mod) 
plot(best.mod)

# Stepwise procedures:
# Forward selection
ols_step_forward_p(model=m.all, details=FALSE) # Forward selection based on p-values
# Backward selection
ols_step_backward_p(model=m.all, details=FALSE) # Backward selection based on p-values
# Sequential replacement
ols_step_both_p(model=m.all, details=FALSE) # Sequential replacement based on p-values