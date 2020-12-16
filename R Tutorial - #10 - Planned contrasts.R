# R Tutorial - #10 - Planned contrasts

# By: Aaron Prosser, MD MSc | Last edited: 13-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Seven Rules for building contrasts
# Part 2 - Planned contrasts using the aov_4() function: 4 steps
# Part 3 - Planned contrasts using the lm() function: 4 steps

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("emmeans")
install.packages("dplyr")
install.packages("afex")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(emmeans)
library(dplyr)
library(afex)

# Disable scientific notation:
options(scipen=999, digits=4)

# Set directory:
dir <- "C:/Users/Aaron/iCloudDrive/Projects/Statistics for Doctors/R Tutorial/"

# Import data and create a data frame:
file <- "Alcohol.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
dat <- as.data.frame(import)

# Factor the predictors:
dat$Sex <- factor(dat$Sex, levels=c("Male", "Female"), 
                  labels=c("Male", "Female"))
dat$Sex <- relevel(dat$Sex, ref="Female")
dat$Alcohol <- factor(dat$Alcohol, levels=c("None", "2 Pints", "4 Pints"), 
                      labels=c("None", "2 Pints", "4 Pints"))
dat$Alcohol <- relevel(dat$Alcohol, ref="None")



# -------- #
#  Part 1  # Seven Rules for building contrasts
# -------- # 

# Contrasts are 'weights' similar to dummy codes, which become the predictors 
# in our linear model, and, so, we get parameter estimates for these contrasts:
contrasts(dat$Alcohol)

# Planned contrasts are a way to reduce the false positive rate by only comparing
# groups which you hypothesized would be different before you ran your experiment
# (e.g., clinical trial), rather than doing a ton of post-hoc pairwise comparisons
# which requires adjusting the p-values for multiple comparisons.

# "Building contrasts" means defining the weights which will be fed into
# the linear model as predictors of the outcome. The parameter estimates are then
# based on these weights, rather than dummy codes. So, each parameter estimate
# becomes a way to test a specific a priori hypothesis by contrasting the group
# means you hypothesized would be different.

# Great resource on contrasts in R: https://rstudio-pubs-static.s3.amazonaws.com/65059_586f394d8eb84f84b1baaf56ffb6b47f.html

# Seven Rules for constructing contrasts:

#   Rule 1: Compare only 2 "chunks" of variation (i.e., 1 or more levels of a factor)
#           per contrast. If you have a control group, that should probably be 
#           your 1st contrast. After that, choose comparisons which make sense
#           based on your a priori hypotheses.

# Contrast #1: Experimental vs. control conditions
chunk 1 vs. chunk 2 = 
no alcohol vs. any alcohol = 
None vs. (2 pints + 4 pints)

# Contrast #2: Dose-response relationship
chunk 1 vs. chunk 2 =
2 pints vs. 4 pints

# Cool, but how do we define these contrasts mathematically? 

# First, look at the order of the levels of the factor:
attributes(dat$Alcohol) 

# You must define each contrast according to the order of the levels:
  
# Contrast #1: 
no alcohol vs. any alcohol = 
c(-1, 0.5, 0.5)

# Contrast #2:
2 pints vs. 4 pints = 
c(0, -1, 1)

#   Rule 2: If a chunk has been singled out in a contrast, it can't be used in
#           subsequent contrasts (i.e., its weight must be 0), UNLESS it can be
#           subdivided into smaller chunks of variation.

#   Rule 3: Chunks coded with positive weights are compared to chunks coded with
#           negative weights. So, one chunk must have a positive weight, the other
#           chunk must have a negative weight. It doesn't matter which chunk
#           you choose for the positive vs. negative weights.

#   Rule 4: Chunks not in a contrast must be coded as 0.

#   Rule 5: The sum of ALL the weights in a contrast MUST be 0 in order to
#           make the contrast orthogonal.

# Contrast #1: c(-1, 0.5, 0.5)
-1 + 0.5 + 0.5 = 0
# Contrast #2: c(0, -1, 1)
0 + -1 + 1 = 0

#   Rule 6: In order that the parameter estimates equal the difference of the means
#           of the chunks being compared, the weight for a chunk must sum to 1 or -1 
#           depending on the weights you assigned to the chunks. If you don't care about 
#           this, then ignore Rule #6. The t-statistics and p-values will be the same 
#           as long as you obey the other rules.

# Contrast #1: c(-1, 0.5, 0.5)
Sum of chunk 1 = -1
Sum of chunk 2 = 0.5 + 0.5 = 1
# Contrast #2: c(0, -1, 1)
Sum of chunk 1 = -1
Sum of chunk 2 = 1

#   Rule 7: If you've built your contrasts properly, then the max. # of orthogonal 
#           contrasts you should have by the end is k - 1, where k = the # of group means 
#           available.

# e.g., the factor 'Alcohol' has x3 levels, so we can have a max. of 2 orthogonal contrasts.



# -------- #
#  Part 2  # Planned contrasts using the aov_4() function: 4 steps
# -------- # 

# Step 1 - Run the model
m <- aov_4(Self_control ~ Alcohol + (1|ID), type=2, data=dat)
summary(m)

# Step 2 - Get the estimated marginal means (EMM)
EMM <- emmeans(m, specs= ~ Alcohol, model="univariate")

# Step 3 - Define the contrasts using the Seven Rules
contr <- list(
  any_vs_none = c(-1, 0.5, 0.5), # Compares no alcohol with any amount of alcohol
  pints4_vs_pints2 = c(0, -1, 1) # Compares 2 pints with 4 pints
)

# Step 4 - Apply the contrasts to the EMM
contrast(EMM, method=contr) %>% summary(infer = TRUE) 

# What if you ignore Rule #6?
contr <- list(
  any_vs_none = c(-2, 1, 1), # Compares no alcohol with any amount of alcohol
  pints4_vs_pints2 = c(0, -2, 2) # Compares 2 pints with 4 pints
)
# The t-statistics and p-values are the same! Only the estimates and SE are 
# different because they are weighted different. But notice that the RATIO
# between the estimates and SE remain the same. So, the t-statistics and p-values 
# are the same. This is why, ultimately, Rule #6 isn't that important, unless
# you care about having the estimates be the difference of the means of the 
# chunks being compared.
contrast(EMM, method=contr) %>% summary(infer = TRUE)



# -------- #
#  Part 3  # Planned contrasts using the lm() function: 4 Steps
# -------- # 

# Step 1 - Define the contrasts using the Seven Rules
any_vs_none <- c(-1, 0.5, 0.5) # Compares no alcohol with any amount of alcohol
pints4_vs_pints2 <- c(0, -1, 1) # Compares 2 pints with 4 pints

# Step 2 - Calculate the inverse of the contrast matrix
contr.tmp <- rbind(
  tmp=1, # 'tmp' is just to make the matrix square.
  any_vs_none,
  pints4_vs_pints2
)
contr <- solve(contr.tmp) # Inverse of the temporary contrast matrix using solve()
contr <- contr[,-1] # Remove the 'tmp' (1st column) since it serves no purpose

# Step 3 - Link those contrasts to the predictors:
contrasts(dat$Alcohol) <- contr
attributes(dat$Alcohol)

# Step 4 - Run the linear model with the planned contrasts:
m <- lm(Self_control ~ Alcohol, data=dat)
summary(m)

# What if you don't take the inverse of the contrast matrix?
contr <- cbind(
  any_vs_none, # Compares no alcohol with any amount of alcohol
  pints4_vs_pints2 # Compares 2 pints with 4 pints
)
contrasts(dat$Alcohol) <- contr
attributes(dat$Alcohol)
m <- lm(Self_control ~ Alcohol, data=dat)
summary(m)