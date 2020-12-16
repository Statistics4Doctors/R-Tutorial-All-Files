# R Tutorial - #2 - Basics of coding in R

# By: Aaron Prosser, MD MSc | Last edited: 30-Nov-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Basics of the R programming language
# Part 2 - How to install and load packages
# Part 3 - How to troubleshoot problems
# Part 4 - How to disable scientific notation



# -------- #
#  Part 1  # Basics of the R programming language
# -------- # 

# Let's begin. You're going to see a lot of hashtags (#) in this code.
# so, let's start here.

# A hashtag (#) is a "commenting symbol". It's a way to make notes in your code.
# I use a lot of notes because I find it's easier to understand what I was doing
# when I look back at old code. I use a lot of # in this code for teaching purposes,
# so you can follow along and understand how the code works after watching the 
# YouTube tutorials.

# The # symbol tells R to not read anything to the right of it, so that if you 
# run your code, R won't try to process it.

# x <- 1
x <- 1

# Second, you're going to see the "<-" symbol a lot. "<-" is how you define 
# variables in R. It kind of means "equals" (=), but not really. 

# Think of "<-" as "X is Y", not "X = Y". In R, if you want to ask whether X = Y, 
# what you're likely asking is a logical statement, which is either true or false. 
# R uses the symbol "==" for these kinds of logical statements. I go into logical
# operators below.

# For example:

y <- 1
x <- y
x == y # It is TRUE that X = Y

y <- 2
x == y # It is FALSE that X = Y


# TYPES OF DATA:

# There are 6 major types of data in R: 
# - Numeric
# - Character
# - Logical 
# - Factor
# - Ordinal
# - Missing values

# 1) Numeric: e.g., 1, 2, 3,...
x <- c(1, 2, 3)
# You can ask R to check if a variable is numeric:
is.numeric(x) 

# 2) Character (i.e., string): e.g., "hello", "1", "2", "3",...
x <- c("hello", "world")
# You can ask R to check if a variable is a character:
is.character(x)
is.numeric(x)

x <- c("1", "2", "3")
is.character(x)
is.numeric(x)


# 3) Logical: e.g., TRUE, FALSE
x <- c(TRUE, FALSE, TRUE, TRUE)
# You can ask R to check if a variable is logical:
is.logical(x)

# 4) Factor: you can turn any variable (character or numeric) into a nominal
# variable (e.g., levels of a factor for an ANOVA) by making it a factor: 
x <- c(0, 2, 1, 1, 0, 2, 0, 1, 2, 2)
# Convert the numerical values in x into different levels of a factor:
x <- factor(x) 
# Give labels to these numbered levels of the factor:
x <- factor(x, 
            levels=c(0, 1, 2), 
            labels=c("Placebo", "Treatment A", "Treatment B")) 
# The reference category is the first level listed when you look at 
# the levels after you apply the attributes() function to the variable:
attributes(x)
# By default, R sets the reference category to the first value you put in 
# "levels=c(X, Y, Z, etc.)". You can manually re-set the reference category for 
# a factor using the relevel() function:
x <- relevel(x, ref="Treatment A") 
attributes(x)

# 5) Ordinal: similarly you can turn any variable (character or numeric) into a 
# rank ordered variable (i.e., ordinal variable) by setting 'ordered' to TRUE.
# The reference category in an ordinal variable is the first value you put in 
# "levels=c(X, Y, Z, etc.)". For some reason, you cannot use the relevel() function 
# for an ordinal variable. R will give you an error message.

# For example, let's say we want "Low risk" to be the reference category, 
# and we want "Low risk" dummy coded with the value 0:
x <- c(0, 2, 1, 1, 0, 2, 0, 1, 2, 2) 
x <- factor(x, levels=c(0, 1, 2), 
            labels=c("Low risk", "Moderate risk", "High risk"), 
            ordered=TRUE)
# Try changing the reference category to "High risk" using the relevel() function:
x <- relevel(x, ref="High risk")
# Clearly that didn't work. 
# To change the reference category to "High risk", you need to switch the levels 
# and labels so that they list the "High risk" category first:
x <- c(0, 2, 1, 1, 0, 2, 0, 1, 2, 2) 
x <- factor(x, levels=c(2, 1, 0), 
            labels=c("High risk", "Moderate risk", "Low risk"), 
            ordered=TRUE)

# 6) Missing values = NA
x <- c(0, 2, NA, 1, NA, 2, 0, 1, NA, 2) 
# You can ask R to check if there are missing values in a variable:
is.na(x) 


# CONVERTING BETWEEN DATA TYPES:

# In R, you can force some variables to be another data type. Sometimes you want
# to do this (e.g., if you want 0s and 1s to code for TRUE and FALSE):
x <- c(1, 2, 3)
x <- as.character(x)
# Anything NOT zero is called TRUE in R.
x <- c(0, 1)
x <- as.logical(x) 
x <- c(0, 1, 2, 3)
x <- as.logical(x)
# But, you can't force character variables to be numeric or logical. For example:
x <- "Hello"
x <- as.numeric(x)
x <- "Hello"
x <- as.logical(x)


# DATA STRUCTURES:

# A "data structure" is a generic term for anything that can have data.
# There are 5 major structures in R: 
# - Vectors
# - Matrices
# - Arrays
# - Data frames 
# - Lists

# 1) Vector: is a sequence of objects of the SAME TYPE of data. Vectors
# can be made in different ways, but most commonly with the c() function.
# In many ways, vectors are the building blocks on R, since we use vectors 
# to make data frames and matrices.
x <- c(1, 2, 3, 4, 5)
y <- c("my", "name", "is", "Aaron")
z <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

# If you have different types of data in a vector, R will automatically convert
# them into one data type. For example:

# R automatically converts the numeric element into a character:
x <- c(1, "Hello", 3, 4, 5)
# R automatically converts the logical element into numeric:
x <- c(1, TRUE, 3, FALSE, 5)
# R automatically converts the logical element into character:
x <- c("Hello", TRUE, FALSE)

# 2) Matrix: a 2-dimensional data structure containing objects of the SAME 
# TYPE of data. I personally don't use matrices a lot in R for data analysis.
# They are very useful for some functions, but I find data frames accomplish
# most of what I need R to do:
x <- 1:10
rnames <- c("A", "B", "C", "D", "E")
colnames <- c("Outcome", "Predictor")
matrix(data=x, nrow=5, ncol=2, dimnames=list(rnames, colnames))

# 3) Array: similar to a matrix but has > 2-dimensions. We won't go into these.
# Honestly, I haven't yet needed to use them. But, if you want to know more, 
# type this in the console:
help(array)

# 4) Data frame: a more general version of a matrix, since different columns
# can have different types of data. I use data frames primarily in data 
# analysis with R. For example:

# Let's make a vector of participant identification (ID) numbers:
ID <- 1:10
# Now, lets make some fake outcome data. We will make three variables, one for
# the continuous, dichotomous, and ordinal outcome variables if this fake study.

# Random sequence of 10 numbers from 1 to 100:
continuous_outcome <- runif(10, min=1, max=100)
continuous_outcome <- round(continuous_outcome, digits=2)

# Random sequence of 0s or 1s:
dichotomous_outcome <- sample(c(0, 1), replace=TRUE, size=10) 
dichotomous_outcome <- factor(dichotomous_outcome,
                              levels=c(0, 1),
                              labels=c("No violence", "Violence"))
dichotomous_outcome <- relevel(dichotomous_outcome, ref="No violence")

# Random sequence of 0s, 1s, or 2s:
ordinal_outcome <- sample(c(0, 1, 2), replace=TRUE, size=10) 
ordinal_outcome <- factor(ordinal_outcome,
                          levels=c(0, 1, 2),
                          labels=c("Low risk", "Moderate risk", "High risk"),
                          ordered=TRUE)

# Let's make two dichotomous predictor variables. We build the first predictor
# using dummy codes (0, 1) to define two groups. We build the second predictor
# using character elements ("X", "Y") to define two groups:
predictor1 <- rep(c(0, 1), times=5) 
predictor1 <- factor(predictor1,  
                     levels=c(0, 1),
                     labels=c("Control", "Case"))
predictor1 <- relevel(predictor1, ref="Control")
predictor2 <- c(rep("Male", times=5),
                rep("Female", times=5))
predictor2 <- factor(predictor2, 
                     levels=c("Male", "Female"),
                     labels=c("Male", "Female"))
predictor2 <- relevel(predictor2, ref="Female")

my_data <- data.frame(ID, 
                      continuous_outcome, 
                      dichotomous_outcome, 
                      ordinal_outcome, 
                      predictor1, 
                      predictor2)
head(my_data)
# Learn about how this data frame is structured
str(my_data)
attributes(my_data)

# 5) List: an ordered collection of objects (e.g., numeric, character, logical, 
# factor, vectors, matrix, data frames, etc.). This allows you to put a lot of 
# different types of information under one variable name. We will explore lists
# in Tutorial #5 with an example to see how they work.

# BUILT-IN FUNCTIONS IN R:
# Logical operators:
# <       Less than
# <=      Less than or equal to
# >       Greater than
# >=      Greater than or equal to
# ==      Exactly equal to
# !=      Not equal to
# !x      Not x
# x | y   x OR y
# x & y   x AND y

# Arithmetic operators:
# +       Addition
# -       Subtraction
# *       Multiplication
# /       Division
# ^ or ** Exponentiation

# Useful mathematical functions:
# mean(x)       Mean of object x
# sd(x)         Standard deviation of x
# var(x)        Variance of x
# median(x)     Median of x
# quantile(x)   Quantiles of x (Default: 0%, 25%, 50%, 75%, 100%)
# sum(x)        Sum of x
# range(x)      Range of x
# min(x)        Minimum value of x
# max(x)        Maximum value of x
# na.rm         Some functions have this option to remove any missing values.
#               If "na.rm=TRUE", then they will be removed. For example:
x <- c(0, 2, NA, 1, NA, 2, 0, 1, NA, 2) 
mean(x)
mean(x, na.rm=TRUE)
# abs(x)        Absolute value of x
# sqrt(x)       Square root of x
# round(x, digits=n)    Round x to the nth digit
# log(x)        Natural logarithm of x
# log10(x)      Common logarithm of x
# exp(x)        Exponential of x (e^x)

# Other useful functions:
# length(x)     Length of vector x
# dim(x)        Dimensions of matrix or data frame x (i.e., # rows, # columns)
# str(x)        Structure of object x
# attributes(x) Attributes of object x
# class(x)      Tells you the class or type of object x
# names(x)      Tells you the names of object x (if applicable)
# c(x, y,...)   Combine objects x, y, etc. into a vector
# cbind(x, y, etc.)   Combine objects x, y, etc. as columns
# rbind(x, y, etc.)   Combine objects x, y, etc. as rows
# rep(x, times=Y)   Repeat x object a total of y times
# seq(from=x, to=y, by=z)   Generate a sequence of numbers from x to y by intervals of z
# cat("\f")     Clear the console in RStudio, but not the stored objects
# rm(list=ls()) Clear all objects in the environment.



# -------- #
#  Part 2  # How to install and load packages 
# -------- # 

# How to install a package in R:
install.packages("PACKAGE")

# Each time you start R, you must load the packages you'll be using. If you use
# R regularly, you'll find that there are packages you'll always load, since they 
# are used a lot in data analysis (e.g., psych, ggplot).

# How to load packages each time you start R:
library(PACKAGE) # No quotes required



# -------- #
#  Part 3  # How to troubleshoot problems
# -------- # 
# When I run into a problem in R, these are my strategies to troubleshoot 
# (ordered by how useful I personally find each strategy):

# 1) Google search: "R [INSERT PROBLEM]" - I guarantee you someone probably had 
# the same problem and someone smart showed them how to solve it.

# 2) Look up the package's PDF: 
#   e.g., https://cran.r-project.org/web/packages/psych/psych.pdf

# 3) Look in R documentation: 
#   e.g., https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm

# 4) Go to the package's homepage, if they have one: 
#    e.g., https://ggplot2.tidyverse.org/index.html  

# 5) Search Quick-R: https://www.statmethods.net/index.html



# -------- #
#  Part 4  # How to disable scientific notation
# -------- # 
options(scipen=999, digits=4)