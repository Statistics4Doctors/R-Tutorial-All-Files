# R Tutorial - #3 - How to import and explore data

# By: Aaron Prosser, MD MSc | Last edited: 2-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Example: The Child Aggression Study
# Part 2 - How to import data sets into R
# Part 3 - How to create a data frame for data analysis
# Part 4 - How to explore a data frame

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("foreign")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(foreign)

# Disable scientific notation:
options(scipen=999, digits=4)



# -------- #
#  Part 1  # Example: The Child Aggression Study
# -------- # 

# This is NOT real data. It is simulated data for teaching purposes.

# N = 666 children with older sibling(s)

# Cross-sectional study to find the predictors of aggression
# in children with older sibling(s).

# Outcome measure: 
# - Aggression Scale: 0 to 5 point scale (higher score = more aggression)

# Predictor variables:
# - Television watching: 0 to 5 point scale (Higher score = more time spent watching TV)
# - Video game playing: 0 to 5 point scale (Higher score = more time spent playing video games)
# - Sibling aggression: 0 to 5 point scale (Higher score = more aggression in older siblings)
# - Poor Diet: 0 to 5 point scale (Higher score = more additives in food)
# - Coercive parenting style: 0 to 10 point scale (Higher score = more coercive parenting practices)



# -------- #
#  Part 2  # How to import data sets into R
# -------- # 

# Unlike SPSS or Excel, R does NOT store data sets. Your data set needs to be saved 
# by another program (e.g., SPSS, Excel) external to R and then IMPORTED into R. 

# This sounds like a bad thing, but it is actually VERY GOOD. This is because
# you NEVER manipulate your original data while analyzing your data in R.

# Rather, in R, you create a virtual copy of the original data set, which you can then
# manipulate, analyze, and even export. A virtual data set protects you from 
# catastrophic unintentional changes to the original data.

# Set your directory for importing files. Change the path in 'dir' to the file 
# folder where you have saved the data sets you downloaded from GitHub:
dir <- "C:/Users/Aaron/iCloudDrive/Projects/Statistics for Doctors/R Tutorial/"

# Import a "comma separated value" (CSV) .txt file
file <- "ChildAggression_CSV.txt"
path <- paste(dir,file,sep="")
import <- read.table(path, header=TRUE, sep = ",")
head(import)

# Import a tab delimited .txt file
file <- "ChildAggression_TabDelimited.txt"
path <- paste(dir,file,sep="")
import <- read.table(path, header=TRUE, sep = "\t")
head(import)

# Import an Excel file
file <- "ChildAggression_Excel.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
head(import)

# Import an SPSS (.SAV) file
file <- "ChildAggression_SPSS.SAV"
path <- paste(dir,file,sep="")
import <- read.spss(path, to.data.frame=TRUE)
head(import)



# -------- #
#  Part 3  # How to create a data frame for data analysis
# -------- #
dat <- as.data.frame(import) # If 'import' isn't a data frame, this will make sure it is.

# Problem: We don't have a variable which has each observation's ID
head(dat)
# So, let's create a vector of IDs by generating a sequence of numbers.
Child <- seq(from=1, to=length(dat$Aggression), by=1)
# Attach that vector called "Child" to the data frame. You can attach new columns
# to a data frame by using the "$" operator, followed by the name of the new column
dat$Child <- Child
head(dat)

# Export this new data frame to our computer as an Excel file:
file <- "NEW_ChildAggression_Excel.xlsx"
path <- paste(dir,file,sep="")
write.xlsx(dat, path)



# -------- #
#  Part 4  # How to explore a data frame
# -------- #
dim(dat) # Get the dimensions of your data frame (1st = # rows, 2nd = # columns)
nrow(dat) # Get the number of rows of your data frame
ncol(dat) # Get the number of columns of your data frame
str(dat) # Structure of the data frame
attributes(dat) # Attributes within the data frame

# Square brackets "[ ]" allow you to explore a data frame: [rows, columns].
dat[1,] # get row #1
dat[,1] # get column #1
dat[5:10,] # get rows from 5 to 10
head(dat[,1:3]) # get columns 1 to 3
dat[c(1:4,10,12),] # get rows 1 to 4, and row 10 and 12
head(dat[,c(1:3,6)]) # get columns 1 to 3, and column 6

isTRUE(is.na(dat)) # Any missing values in the whole data set?

# You can also explore using the "$" symbol to call up a specific variable
# in the data frame.
dat$Aggression
dat$Parenting_Style

# How long is a row or column?
length(dat$Aggression) # Length of column "Aggression" = N = 666
length(dat[1,]) # Length of the 1st row = 7 = x1 outcome + x5 predictors + ID variable

# Get a subset of the ENTIRE DATA FRAME which meets a criterion
new_dat <- subset(dat, dat$Parenting_Style >= mean(dat$Parenting_Style))
head(new_dat)
dim(new_dat) # How many children may this criterion?

# Get a subset of ONLY THE OUTCOME VARIABLE which meets a criterion
new_dat <- subset(dat$Aggression, dat$Parenting_Style < mean(dat$Parenting_Style))
head(new_dat)
length(new_dat) # Use "length" instead of "dim" because new_data is a vector, not matrix

# Get a subset of the outcome variable only which meets a set of criteria
new_dat <- subset(dat$Aggression, 
                  dat$Parenting_Style > mean(dat$Parenting_Style)+sd(dat$Parenting_Style) |
                    dat$Parenting_Style < mean(dat$Parenting_Style)-sd(dat$Parenting_Style))
head(new_dat)
length(new_dat)