# R Tutorial - #11 - Factorial ANOVA

# By: Aaron Prosser, MD MSc | Last edited: 14-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Type of Sum of Squares (SS) in R: A Caution!
# Part 2 - Two-way factorial ANOVA using the afex package
# Part 3 - Two-way factorial ANOVA using the lm() function

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("psych")
install.packages("car")
install.packages("emmeans")
install.packages("dplyr")
install.packages("nortest")
install.packages("afex")
install.packages("ggfortify")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(ggplot2)
library(psych)
library(car)
library(emmeans)
library(dplyr)
library(nortest)
library(afex)
library(ggfortify)

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
#  Part 1  # Type of Sum of Squares (SS) in R: A Caution!
# -------- # 

# In R, the default for the built-in anova() function is Type I SS. 

# Type III SS is the default for SPSS and SAS. This can lead to different F-tests
# when analyzing the same data using different statistical software, if you're
# not paying attention to the Type of SS and whether your data is balanced.

# In balanced designs, Type I, II and III SS will produce the same F-tests because
# the factors are orthogonal (i.e., uncorrelated) to each other. So, for balanced
# experimental designs, the type of sum of squares isn't an issue. 

# So, make sure your design is balanced if you can!

# But, in unbalanced designs, the F-tests will differ depending on the type of SS
# because the factors will be correlated with each other (i.e., non-orthogonal).
# The non-orthogonal nature of unbalanced designs screws up the partitioning of 
# the variance, so you get different F-tests depending on the type of SS you use.

# Arguably, Type III SS are the preferable type of SS for (1) unbalanced data and 
# (2) when interactions are present or relevant to your hypotheses. This probably
# explains why it is the default in SPSS and SAS. 

# But, in order to get valid results, Type III SS require that the predictors (i.e., factors)
# are encoded with orthogonal contrasts (i.e., sum to zero) because Type III SS 
# violates the 'principle of marginality'.

# PROBLEM: 

# R's default when defining linear models is to use contr.treatment(),
# which creates non-orthogonal "contrasts" (i.e., dummy codes) which don't 
# sum to zero.
options('contrasts')

# SOLUTION:

# You need to manually define the orthogonal contrasts for each predictor before 
# running the lm() function for your factorial ANOVA with Type III SS.

# There are four functions in R which automatically create contrast matrices
# for predictors for use in linear models (and thus ANOVAs):

#  1) contr.sum() = orthogonal contrasts for non-ordered factors
#  2) contr.poly() = orthogonal polynomial contrasts for ordered factors
#  3) contr.helmert() = Helmert contrasts
#  4) contr.treatment() = non-orthogonal "contrasts" (i.e., dummy codes)

# So, when doing a factorial ANOVA with Type III SS using the lm() function, 
# you need to do three things:

#  1) Define the orthogonal contrasts for each factor (i.e., predictor) using
#     one of the above functions.
#  2) Define your linear model using the lm() function.
#  3) Calculate the Type III SS for your linear model using the Anova() function 
#     from the car package.

 

# -------- #
#  Part 2  # Two-way factorial ANOVA using the afex package
# -------- # 

# 1) aov_4() function: automatically sets orthogonal contrasts
m <- aov_4(Self_control ~ Alcohol*Sex + (1|ID), type=3, data=dat)
summary(m)

# Plot the data:
emmip(m, Sex ~ Alcohol, CIs=TRUE, 
      xlab="Alcohol", 
      ylab="Self-control") 

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ Alcohol*Sex, model="univariate",
                  adjust="tukey") # Tukey's HSD
                  #adjust="bonferroni") # Bonferroni
                  #adjust="fdr") # FDR (Benjamini–Hochberg) adjustment
                  #adjust="none") # No adjustment
posthocs$emmeans # Estimated marginal means (EMM)
posthocs$contrasts %>% summary(infer = TRUE) # Post-hoc comparisons

# Plot the interaction between of alcohol and sex on self-control in ggplot:
means <- as.data.frame(posthocs$emmeans) 
ggplot() +
  coord_cartesian(ylim=c(0,100)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=100, by=10)) + 
  xlab("Alcohol") +
  ylab("Self-control") +
  theme(legend.position="bottom") +
  geom_jitter(data=dat, shape=16, size=2, width=0.05, alpha=0.4, 
              aes(x=Alcohol, y=Self_control, col=Sex)) +
  geom_point(data=means, size=4, 
             position=position_dodge(0.25),
             aes(x=Alcohol, y=emmean, col=Sex)) +
  geom_errorbar(data=means, width=0, size=1, 
                position=position_dodge(0.25),
                aes(x=Alcohol, y=emmean, 
                    ymin=lower.CL, ymax=upper.CL, col=Sex))

# Extract residuals/predicted values:
new_dat <- fitted(m, append=TRUE)
residuals_tmp <- residuals(m, append=TRUE)
new_dat$.residuals <- residuals_tmp$.residuals
head(new_dat, 20)

# Parametric test assumptions:
# Histogram of the residuals
ggplot(data=new_dat, aes(x=.residuals)) +
  ylab('Frequency') +
  xlab(bquote("Residuals")) +
  geom_histogram(color="black", fill="white", binwidth=7)

# Diagnostic plots
autoplot(m$lm, which=c(1,2,3,4))

# Calculate skew and kurtosis of the residuals
describe(new_dat$.residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(new_dat$.residuals) # Shapiro-Francia test
ad.test(new_dat$.residuals) # Anderson-Darling test
lillie.test(new_dat$.residuals) # Lilliefors (Kolmogorov-Smirnov) test

# Given the residual plot, let's test homogeneity of variance
leveneTest(Self_control ~ Alcohol*Sex, data=dat)

# Planned contrasts:
# See Tutorial #10



# -------- #
#  Part 3  # Two-way factorial ANOVA using the lm() function
# -------- # 

# What happens if you don't have orthogonal contrasts with Type III SS?
contrasts(dat$Alcohol)
contrasts(dat$Sex)
m <- lm(Self_control ~ Alcohol*Sex, data=dat) # Define the model using lm()
Anova(m, type="III") # Incorrect F-statistics for the main effects

# for the lm() function you must manually set the orthogonal contrasts:
contrasts(dat$Alcohol) <- contr.poly(3) # Alcohol is an ordered factor
contrasts(dat$Sex) <- contr.sum(2) # Sex is an unordered factor
attributes(dat$Alcohol)
attributes(dat$Sex)

m <- lm(Self_control ~ Alcohol*Sex, data=dat) # Define the model using lm()
Anova(m, type="III") # Correct F-statistics for the main effects
Anova(m, type="II") # Same results with Type II SS because the data is balanced
anova(m) # Same results with Type I SS because the data is balanced
summary(m)

# Plot the data:
emmip(m, Sex ~ Alcohol, CIs=TRUE, 
      xlab="Alcohol", 
      ylab="Self-control") 

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ Alcohol*Sex, model="univariate",
                  adjust="tukey") # Tukey's HSD
                  #adjust="bonferroni") # Bonferroni
                  #adjust="fdr") # FDR (Benjamini–Hochberg) adjustment
                  #adjust="none") # No adjustment
posthocs$emmeans # Estimated marginal means (EMM)
posthocs$contrasts %>% summary(infer = TRUE) # Post-hoc comparisons

# Extract residuals/predicted values:
dat$predicted <- m$fitted.values
dat$residuals <- m$residuals
head(dat, 20)

# Parametric test assumptions:
# Histogram of the residuals
ggplot(data=dat, aes(x=residuals)) +
  ylab('Frequency') +
  xlab(bquote("Residuals")) +
  geom_histogram(color="black", fill="white", binwidth=7)

# Diagnostic plots
autoplot(m, which=c(1,2,3,4))

# Calculate skew and kurtosis of the residuals
describe(dat$residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(dat$residuals) # Shapiro-Francia test
ad.test(dat$residuals) # Anderson-Darling test
lillie.test(dat$residuals) # Lilliefors (Kolmogorov-Smirnov) test

# Given the residual plot, let's test homogeneity of variance
leveneTest(m)

# Planned contrasts:
# See Tutorial #10