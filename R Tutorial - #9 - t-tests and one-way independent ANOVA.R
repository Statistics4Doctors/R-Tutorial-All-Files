# R Tutorial - #9 - t-tests and one-way independent ANOVA

# By: Aaron Prosser, MD MSc | Last edited: 12-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Tutorial Outline:
# Part 1 - Example: The Alcohol and Self-control Study
# Part 2 - Descriptive statistics and preliminary plot
# Part 3 - t-tests
# Part 4 - One-way independent ANOVA using the afex package
# Part 5 - One-way independent ANOVA using the lm() function

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



# -------- #
#  Part 1  # Example: The Alcohol and Self-control Study
# -------- # 

# This is NOT real data. It is simulated data for teaching purposes.

# N = 48 participants

# Study to see if there are sex differences in how alcohol affects self-control.

# Outcome measure: 
# - Self-control: 0 to 100 point scale (higher score = more self-control)

# Predictor variables:
# - Alcohol intake: 0 pints, 2 pints, 4 pints (categorical variable)
# - Sex: Male vs. Female (categorical variable)

# Import data and create a data frame:
file <- "Alcohol.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
dat <- as.data.frame(import)

# Confirm that Sex and Alcohol are character vectors
head(dat$Sex)
head(dat$Alcohol)

# Convert them into factored variables in preparation for data analysis:
dat$Sex <- factor(dat$Sex, levels=c("Male", "Female"), 
                  labels=c("Male", "Female"))
dat$Sex <- relevel(dat$Sex, ref="Female")
dat$Alcohol <- factor(dat$Alcohol, levels=c("None", "2 Pints", "4 Pints"), 
                      labels=c("None", "2 Pints", "4 Pints"))
dat$Alcohol <- relevel(dat$Alcohol, ref="None")

# Confirm that Sex and Alcohol are now factors
attributes(dat$Sex)
attributes(dat$Alcohol)



# -------- #
#  Part 2  # Descriptive statistics and preliminary plot
# -------- # 
describe(dat$Self_control)

# Is the data balanced or unbalanced (i.e., same #s per level of factor or not)?
table(dat$Sex, dat$Alcohol)
# Any missing data?
isTRUE(is.na(dat))

# Plot the individual participant data by the factors:
ggplot(data=dat, aes(x=Alcohol, y=Self_control)) +
  coord_cartesian(ylim=c(0,100)) + 
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=100, by=10)) + 
  xlab("Alcohol") +
  ylab("Self-control") +
  geom_jitter(shape=16, size=2, width=0.05, aes(color=Sex))

# Questions: 
# 1) Does this scatter plot suggest a main effect of sex or alcohol?
# 2) What about an interaction between alcohol and sex?



# -------- #
#  Part 3  # t-tests
# -------- #
# Student's t-test:
m <- t.test(Self_control ~ Sex, data=dat, var.equal=TRUE, paired=FALSE)
data <- c(m$statistic, m$stderr, m$estimate[1], 
          m$estimate[2], m$parameter, m$p.value)
rnames <- c("Student's t-test:")
colnames <- c("t", "SE", "Mean Group 0", "Mean Group 1", "df", "p-value")
matrix(data, nrow=1, ncol=6, dimnames=list(rnames, colnames))

# Welch's t-test:
m <- t.test(Self_control ~ Sex, data=dat, var.equal=FALSE, paired=FALSE)
data <- c(m$statistic, m$stderr, m$estimate[1], 
          m$estimate[2], m$parameter, m$p.value)
rnames <- c("Welch's t-test:")
colnames <- c("t", "SE", "Mean Group 0", "Mean Group 1", "df", "p-value")
matrix(data, nrow=1, ncol=6, dimnames=list(rnames, colnames))

# Paired-sample t-test: 

# t.test() accepts the data in wide and long format: 

# 1) Wide format: there is x1 row per subject, with the levels of the 
#    repeated measures in separate columns.

# t.test(pretreatment, posttreatment, data=my_data, paired=TRUE)

# 2) Long format: there are multiple rows per subject, with separate rows for 
#    each observation from a different level of the repeated measure. 

# t.test(outcome ~ time, data=my_data, paired=TRUE)



# -------- #
#  Part 4  # One-way independent ANOVA using the afex package
# -------- #

# There are two ways I do one-way independent ANOVAs in R. 
#  1) aov_4() function from the afex package.
#  2) lm() with the Anova() function from the car package.

# https://cran.r-project.org/web/packages/afex/afex.pdf

# Important things to know about the aov_4() function:
# 1) It must contain a single Error term (...|ID) specifying the ID column 
#    and potential within-subject factors (if present).

# 2) Factors outside the Error term are treated as between-subject factors. 
#    Within-subject factors specified in the Error term.

# 3) Type III sum of squares is the default, but can be changed to Type II.

# 4) aov_4() returns multiple objects: aov(), Anova() and lm(). You will shortly
#    see why this is helpful.

# 5) It returns an object which can be directly passed into emmeans() for 
#    post-hoc testing and planned contrasts.
#         emmeans package tutorials: 
#         - https://cran.r-project.org/web/packages/emmeans/, see the 'Vignettes' Section
#         - https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

m <- aov_4(Self_control ~ Alcohol + (1|ID), type=2, data=dat)
summary(m)

# Plot the data:
emmip(m, ~ Alcohol, CIs=TRUE,
      xlab="Alcohol", 
      ylab="Self-control") 

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ Alcohol, model="univariate",
                  adjust="tukey") # Tukey's HSD
                  #adjust="bonferroni") # Bonferroni
                  #adjust="fdr") # FDR (Benjamini–Hochberg) adjustment
                  #adjust="none") # No adjustment
posthocs$emmeans # Estimated marginal means (EMM) - think: means based on the model.
posthocs$contrasts %>% summary(infer = TRUE) # Post-hoc (all pairwise) comparisons

# convert the emmGrid object into a data frame so we can easily extract
# the means, SE, and CIs to plot the results in ggplot:
means <- as.data.frame(posthocs$emmeans) 

# Plot the main effect of alcohol on self-control in ggplot:
ggplot() +
  coord_cartesian(ylim=c(0,100)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=100, by=10)) + 
  xlab("Alcohol") +
  ylab("Self-control") +
  theme(legend.position="bottom") +
  geom_jitter(data=dat, shape=16, size=2, width=0.05, alpha=0.4, 
              aes(x=Alcohol, y=Self_control, col=Alcohol)) +
  geom_point(data=means, size=4, 
             aes(x=Alcohol, y=emmean, col=Alcohol)) +
  geom_errorbar(data=means, width=0, size=1,
                aes(x=Alcohol, y=emmean, 
                    ymin=lower.CL, ymax=upper.CL, col=Alcohol))

# Extract residuals/predicted values:
new_dat <- fitted(m, append=TRUE)
residuals_tmp <- residuals(m, append=TRUE)
new_dat$.residuals <- residuals_tmp$.residuals
head(new_dat, 20)

# It is generally better to use the above method to extract the residuals and
# predicted values from aov_4() objects. This is because sometimes the order of 
# the rows will change when aov_4() processes a within-subject factor. This method 
# makes sure that the observed values align with their corresponding predicted 
# values/residuals. But it requires making a new data frame, which I've called
# new_dat.

# Parametric test assumptions:
# Histogram of the residuals
ggplot(data=new_dat, aes(x=.residuals)) +
  ylab('Frequency') +
  xlab(bquote("Residuals")) +
  geom_histogram(color="black", fill="white", binwidth=7)

# Diagnostic plots
autoplot(m$lm, which=c(1,2,3,4)) # look how I'm selecting the lm() object!

# Calculate skew and kurtosis of the residuals
describe(new_dat$.residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(new_dat$.residuals) # Shapiro-Francia test
ad.test(new_dat$.residuals) # Anderson-Darling test
lillie.test(new_dat$.residuals) # Lilliefors (Kolmogorov-Smirnov) test



# -------- #
#  Part 5  # One-way independent ANOVA using the lm() function
# -------- #
m <- lm(Self_control ~ Alcohol, data=dat)
Anova(m, Type="II") # Capital 'A' Anova() function from the car package
contrasts(dat$Alcohol) # R automatically generates dummy codes
summary(m) # Get the parameter estimates of these dummy coded predictors

# Plot the data:
emmip(m, ~ Alcohol, CIs=TRUE, 
      xlab="Alcohol", 
      ylab="Self-control") 

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ Alcohol, model="univariate",
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