# R Tutorial - #5 - Linear regression analyses

# By: Aaron Prosser, MD MSc | Last edited: 9-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Simple linear regression
# Part 2 - How explore a list: the linear regression output as an example
# Part 3 - Plotting the regression model
# Part 4 - Saving graphs to the computer
# Part 5 - Testing parametric assumptions
# Part 6 - Multiple linear regression
# Part 7 - Assessing interactions in multiple regression

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("psych")
install.packages("lm.beta")
install.packages("nortest")
install.packages("interactions")
install.packages("ggfortify")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(psych)
library(lm.beta)
library(nortest)
library(interactions)
library(ggfortify)

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
#  Part 1  # Simple linear regression
# -------- #

# Introduction to coding your linear models:

# lm(formula, data=my_data)
# lm(Y ~ model, data=my_data)

# the ~ operator specifies that the variable Y is being modeled by the 
# predictors of your model (A, B, ...). 

# In the formula, each predictor is separated by either a:
# 1) Nothing (if there is only 1 predictor in the model):

# e.g., lm(Y ~ A, data=my_data)

# 2) + operator (additive effects or "lower-order main effects")

# e.g., lm(Y ~ A + B, data=my_data)

# 3) : operator (interaction effects or "higher-order effects")

# e.g., lm(Y ~ A + B + A:B, data=my_data)

# 4) * operator, a shorthand to define the lower- AND higher-order effects.

# e.g., This...

# lm(Y ~ A*B, data=my_data)

# is the same as this...

# lm(Y ~ A + B + A:B, data=my_data)

# For more details about formulae, see:
# help(formula)
# https://conjugateprior.org/2013/01/formulae-in-r-anova/

# The intercept is automatically added by R:
m <- lm(Aggression ~ Parenting_Style, data=dat) 
# You can explicitly include the intercept using "1" at the start of the formula.
m <- lm(Aggression ~ 1 + Parenting_Style, data=dat) # 1=intercept
# summary() is a generic function which summarizes a model fitting procedure:
summary(m) 
lm.beta(m) # Standardized coefficients



# -------- #
#  Part 2  # How explore a list: the linear regression output as an example
# -------- #

m$rank # Get the number of coefficients in the model, incl. the intercept

m$coefficients # Within the list, select the object "coefficients"

m$coefficients[1] # Within the object "coefficients", select the 1st object
m$coefficients[2] # Within the object "coefficients", select the 2nd object

head(m$fitted.values) # Get the predicted values of the linear model (Y=b0 + b1X1)
head(predict(m)) # An alternative way to do the same thing

head(m$residuals) # Get the residuals of the model = (Y - Predicted values)
head(residuals(m)) # An alternative way to do the same thing
head(dat$Aggression-m$fitted.values) # Manually calculate the residuals of the model

# Put the residuals and predicted values of the outcome in the data frame
dat$predicted <- m$fitted.values
dat$residuals <- m$residuals

head(dat, 20)



# -------- #
#  Part 3  # Plotting the regression model
# -------- #
# Method 1
plot1 <- ggplot(data=dat, aes(x=Parenting_Style, y=Aggression)) +
  coord_cartesian(ylim=c(0,5), xlim=c(0,10)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=5, by=0.5)) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(from=0, to=10, by=1)) +
  # Main plot title
  ggtitle("Plot 1") +
  # Center the title (for whatever reason the default in ggplot is the top left corner)
  theme(plot.title=element_text(hjust=0.5)) + 
  xlab("Coercive Parenting Style") +
  ylab("Child Aggression") +
  geom_point(shape=1) + 
  # Add regression line using geom_abline
  geom_abline(intercept=m$coefficients[1], 
              slope=m$coefficients[2], 
              colour="green4", size=1)

# Method 2
plot2 <- ggplot(data=dat, aes(x=Parenting_Style, y=Aggression)) +
  coord_cartesian(ylim=c(0,5), xlim=c(0,10)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=5, by=0.5)) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(from=0, to=10, by=1)) +
  ggtitle("Plot 2") +
  theme(plot.title=element_text(hjust=0.5)) + 
  xlab("Coercive Parenting Style") +
  ylab("Child Aggression") +
  geom_point(shape=1) + 
  # Add regression line with 95% CI using geom_smooth
  geom_smooth(method=lm, se=TRUE, level=0.95, 
              colour="green4", size=1)

# Combine graphs into one:
ggarrange(plot1, plot2, ncol=2, nrow=1)



# -------- #
#  Part 4  # Saving graphs to the computer
# -------- #

# The plot must be AFTER the png/jpeg/bmp/pdf() function. 

# The dev.off() function saves it to the computer.

# Save this combined plot to our computer. 
# PNG
png(file=paste(dir,"RegressionPlots.png",sep=""), width=900, height=450)
ggarrange(plot1, plot2, ncol=2, nrow=1)
dev.off()
# JPEG
jpeg(file=paste(dir,"RegressionPlots.jpeg",sep=""), width=900, height=450)
ggarrange(plot1, plot2, ncol=2, nrow=1)
dev.off()
# bmp
bmp(file=paste(dir,"RegressionPlots.bmp",sep=""), width=900, height=450)
ggarrange(plot1, plot2, ncol=2, nrow=1)
dev.off()
# PDF
pdf(file=paste(dir,"RegressionPlots.pdf",sep=""))
ggarrange(plot1, plot2, ncol=2, nrow=1)
dev.off()



# -------- #
#  Part 5  # Testing parametric assumptions
# -------- #
# Histogram of the outcome variable
ggplot(data=dat, aes(x=Aggression)) +
  ylab('Frequency') +
  xlab('Child Aggression') +
  geom_histogram(color="black", fill="white", binwidth=0.05)

# Box and whisker plot of the outcome variable
ggplot(data=dat, aes(x=Child, y=Aggression)) +
  coord_cartesian(ylim=c(0,5),xlim=c(0,666)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=5, by=0.5)) +
  scale_x_continuous(expand=c(0, 0), breaks=seq(from=0, to=666, by=50)) +
  ylab('Child Aggression') +
  xlab("Child") +
  geom_point(shape=1) +
  geom_boxplot(colour = "Black", outlier.alpha=0)

# Histogram of the residuals
ggplot(data=dat, aes(x=residuals)) +
  ylab('Frequency') +
  xlab(bquote("Residuals")) +
  geom_histogram(color="black", fill="white", binwidth=0.05)

# Diagnostic plots from the autoplot() function in the ggfortify package:
autoplot(m, which=c(1,2,3,4))

# Calculate skew and kurtosis of the residuals
describe(dat$residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(dat$residuals) # Shapiro-Francia test
ad.test(dat$residuals) # Anderson-Darling test
lillie.test(dat$residuals) # Lilliefors (Kolmogorov-Smirnov) test



# -------- #
#  Part 6  # Multiple linear regression
# -------- #
m <- lm(Aggression ~ Parenting_Style + Video_Games,  data=dat)
summary(m)
lm.beta(m) # Standardized coefficients



# -------- #
#  Part 7  # Assessing interactions in multiple regression
# -------- #
m <- lm(Aggression ~ Parenting_Style*Television, data=dat) 
summary(m)
lm.beta(m) # Standardized coefficients
# Plot the data:
interact_plot(m, 
              pred=Parenting_Style, 
              modx=Television,
              x.label="Coercive Parenting Style",
              y.label="Aggression")
# Simple slopes analysis with Johnson-Neyman interval
sim_slopes(m, 
           pred=Parenting_Style, 
           modx=Television, 
           johnson_neyman=TRUE)