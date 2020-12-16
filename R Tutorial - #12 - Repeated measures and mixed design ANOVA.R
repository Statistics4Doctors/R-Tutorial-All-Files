# R Tutorial - #12 - Repeated measures and mixed design ANOVA

# By: Aaron Prosser, MD MSc | Last edited: 14-Dec-2020

# The following code is open-source. Feel free to edit/share at your pleasure.
# All the data sets and R code for this tutorial series can be downloaded here:
https://github.com/Statistics4Doctors

# Part 1 - Example: Psychotherapy Trial
# Part 2 - Descriptive statistics and preliminary plot
# Part 3 - One-way repeated measures ANOVA
# Part 4 - Mixed design ANOVA

# Install these packages, if you haven't already:
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("psych")
install.packages("emmeans")
install.packages("dplyr")
install.packages("nortest")
install.packages("afex")
install.packages("ggfortify")
install.packages("ggResidpanel")

# Load the packages we're using in this tutorial:
library(openxlsx)
library(ggplot2)
library(psych)
library(emmeans)
library(dplyr)
library(nortest)
library(afex)
library(ggfortify)
library(ggResidpanel)

# Disable scientific notation:
options(scipen=999, digits=4)

# Set directory:
dir <- "C:/Users/Aaron/iCloudDrive/Projects/Statistics for Doctors/R Tutorial/"



# -------- #
#  Part 1  # Example: Psychotherapy Trial
# -------- # 

# This is NOT real data. It is simulated data for teaching purposes.

# N = 134 in total, 67 in each intervention group

# RCT to assess the efficacy of outpatient psychotherapy vs. wait list control 
# to improve psychosocial functioning (i.e., capacity for work, love, and play)
# in adult patients with chronic depression.

# Outcome measure: 
# - Psychosocial functioning: 0 to 100 point scale (higher score = better functioning)

# Predictor variables:
# - Intervention: Wait list control group vs. Psychotherapy group
# - Time: baseline (t0), 1 mo (t1), 6 mo (t6), 12 mo (t12)

# Import data and create a data frame:
file <- "psychotherapy_trial.xlsx"
path <- paste(dir,file,sep="")
import <- read.xlsx(path, sheet = 1, startRow = 1)
dat <- as.data.frame(import)
head(dat, 20)

# Factor the predictors:
dat$intervention <- factor(dat$intervention, levels=c("Wait list", "Psychotherapy"), 
                           c("Wait list", "Psychotherapy"))
dat$intervention <- relevel(dat$intervention, ref="Wait list")
dat$time <- factor(dat$time, levels=c("t0", "t1", "t6", "t12"), 
                   labels=c("t0", "t1", "t6", "t12"))
dat$time <- relevel(dat$time, ref="t0")

attributes(dat$intervention)
attributes(dat$time)



# -------- #
#  Part 2  # Descriptive statistics and preliminary plot
# -------- # 

# Is the data balanced or unbalanced (i.e., same #s per level of factor or not)?
table(dat$intervention, dat$time)
# Any missing data?
isTRUE(is.na(dat))

# Descriptive statistics of all the time points and intervention groups
wl_t0 <- subset(dat$psychosocial_fx, dat$intervention=="Wait list" & dat$time=="t0")
wl_t1 <- subset(dat$psychosocial_fx, dat$intervention=="Wait list" & dat$time=="t1")
wl_t6 <- subset(dat$psychosocial_fx, dat$intervention=="Wait list" & dat$time=="t6")
wl_t12 <- subset(dat$psychosocial_fx, dat$intervention=="Wait list" & dat$time=="t12")
psy_t0 <- subset(dat$psychosocial_fx, dat$intervention=="Psychotherapy" & dat$time=="t0")
psy_t1 <- subset(dat$psychosocial_fx, dat$intervention=="Psychotherapy" & dat$time=="t1")
psy_t6 <- subset(dat$psychosocial_fx, dat$intervention=="Psychotherapy" & dat$time=="t6")
psy_t12 <- subset(dat$psychosocial_fx, dat$intervention=="Psychotherapy" & dat$time=="t12")
dat_descriptives <- data.frame(wl_t0, wl_t1, wl_t6, wl_t12,
                               psy_t0, psy_t1, psy_t6, psy_t12)

# Get the descriptive stats for each time point and intervention group
dat_descriptives <- as.data.frame(describe(dat_descriptives))

# Add some helpful columns to the data frame in preparation for the plot
dat_descriptives$time <- rep(c("t0", "t1", "t6", "t12"), 2)
dat_descriptives$time_num <- rep(c(0, 1, 6, 12), 2)
dat_descriptives$intervention <- c(rep(c("Wait list"), 4),
                                   rep(c("Psychotherapy"), 4))

# Plot the group-level effects
ggplot() +
  coord_cartesian(ylim=c(0,100), xlim=c(0,12)) +
  scale_y_continuous(expand=c(0, 0), breaks=seq(from=0, to=100, by=10)) + 
  scale_x_continuous(breaks=c(0, 1, 6, 12), minor_breaks=NULL) + 
  ylab('Psychosocial functioning') +
  xlab('Time (Months)') +
  theme(legend.position="bottom") +
  geom_jitter(data=dat, shape=16, size=1, width=0.2, alpha=0.4, 
              aes(x=time_num, y=psychosocial_fx, col=intervention)) +
  geom_point(data=dat_descriptives, size=4, 
             position=position_dodge(0.25),
             aes(x=time_num, y=mean, col=intervention)) + 
  geom_line(data=dat_descriptives, size=1,
            aes(x=time_num, y=mean, col=intervention))



# -------- #
#  Part 3  # One-way repeated measures ANOVA
# -------- # 

# Select only the patients in the psychotherapy arm of the RCT
dat_psy <- subset(dat, dat$intervention=="Psychotherapy")
table(dat_psy$time)

# How to specify a repeated measures using aov_4(): 

# Data must be in LONG format.

# Outcome = dependent variable
# Factor A = within-subject factor
# Factor B = between-subject factor
# ID = participant ID column

# aov_4(outcome ~ A*B + (rm predictors | ID), type=3, data=my_data)

m <- aov_4(psychosocial_fx ~ time + (time|ID), type=3, data=dat_psy)
summary(m)
# Get the Sphericity-corrected degrees of freedom (df)
nice(m, 
     intercept=TRUE, 
     correction=
       "GG") # Greenhouse-Geisser correction (default)
       #"HF") # Huynh-Feldt correction
       #"none") # No df correction
                     
# Plot the data:
emmip(m, ~ time, CIs=TRUE,
      xlab="Time (Months)",
      ylab="Psychosocial functioning") 

# IMPORTANT: Read the afex package PDF, p. 30, regarding why model should be set to 
# "multivariate" for repeated measures data: https://cran.r-project.org/web/packages/afex/afex.pdf

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ time,  model="multivariate",
                  adjust="tukey") # Tukey's HSD
                  #adjust="bonferroni") # Bonferroni
                  #adjust="fdr") # FDR (Benjamini–Hochberg) adjustment
                  #adjust="none") # No adjustment
posthocs$emmeans # Estimated marginal means (EMM)
posthocs$contrasts %>% summary(infer = TRUE) # Post-hoc comparisons

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

# Unfortunately, at present, someone needs to develop code so we can run the 
# same autoplot() function we did to get the diagnostic plots for the one-way
# and factorial ANOVAs and linear regressions:
autoplot(m$lm, which=c(1,2,3,4))

# Similarly:
plot(m$lm)

# But! We can use the resid_auxpanel() function from the ggResidpanel package 
# to get a nice plot similar to the autoplot() function:
resid_auxpanel(residuals=new_dat$.residuals, 
               predicted=new_dat$.fitted,
               smoother=TRUE, qqbands=TRUE, bins = 50,
               plots=c("resid", "qq", "index", "hist"))
# If you wanted to, you could manually plot the scale-location and Cook's distance
# graphs yourself since you have all the data to calculate & plot these.

# Calculate skew and kurtosis of the residuals
describe(new_dat$.residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(new_dat$.residuals) # Shapiro-Francia test
ad.test(new_dat$.residuals) # Anderson-Darling test
lillie.test(new_dat$.residuals) # Lilliefors (Kolmogorov-Smirnov) test

# Planned contrasts
# Step 1 - Run the model
m <- aov_4(psychosocial_fx ~ time + (time|ID), type=3, data=dat_psy)

# Step 2 - Get the estimated marginal means (EMM)
EMM <- emmeans(m, specs= ~ time, model="multivariate")

# Step 3 - Define the contrasts using the Seven Rules

# Max. number of orthogonal contrasts:
k - 1 = 4 - 1 = max. of 3 contrasts

# What's the order of the group means?
attributes(dat_psy$time)
EMM

# OK, so we need to build our max. 3 orthogonal contrasts based on this order:
contr <- list(
  baseline_vs_FU = c(-1, 1/3, 1/3, 1/3), # Baseline vs. any follow-up time
  early_vs_late = c(0, -1, 0.5, 0.5), # Early (t1) vs. late (t6 or t12) follow-up times
  t6_vs_t12 = c(0, 0, -1, 1) # t6 vs. t12 month follow-up
)

# Step 4 - Apply the contrasts to the EMM
contrast(EMM, method=contr) %>% summary(infer = TRUE) 



# -------- #
#  Part 4  # Mixed design ANOVA
# -------- # 

m <- aov_4(psychosocial_fx ~ time*intervention + (time|ID), type=3, data=dat)
summary(m)
# Get the Sphericity-corrected degrees of freedom (df)
nice(m, 
     intercept=TRUE, 
     correction=
       "GG") # Greenhouse-Geisser correction (default)
       #"HF") # Huynh-Feldt correction
       #"none") # No df correction

# Plot the data:
emmip(m, intervention ~ time, CIs=TRUE,
      xlab="Time (Months)", 
      ylab="Psychosocial functioning") 

# Post-hoc comparisons and estimated marginal means:
posthocs <- emmeans(m, specs=pairwise ~ time*intervention,  model="multivariate",
                  adjust="tukey") # Tukey's HSD
                  #adjust="bonferroni") # Bonferroni
                  #adjust="fdr") # FDR (Benjamini–Hochberg) adjustment
                  #adjust="none") # No adjustment
posthocs$emmeans # Estimated marginal means (EMM)
posthocs$contrasts %>% summary(infer = TRUE) # Post-hoc comparisons

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
resid_auxpanel(residuals=new_dat$.residuals, 
               predicted=new_dat$.fitted,
               smoother=TRUE, qqbands=TRUE, bins = 50,
               plots=c("resid", "qq", "index", "hist"))

# Calculate skew and kurtosis of the residuals
describe(new_dat$.residuals, type=1)

# Statistical tests of the normality of the residuals
sf.test(new_dat$.residuals) # Shapiro-Francia test
ad.test(new_dat$.residuals) # Anderson-Darling test
lillie.test(new_dat$.residuals) # Lilliefors (Kolmogorov-Smirnov) test

# Planned contrasts
# Step 1 - Run the model
m <- aov_4(psychosocial_fx ~ time*intervention + (time|ID), type=3, data=dat)

# Step 2 - Get the estimated marginal means (EMM)
EMM <- emmeans(m, specs= ~ time*intervention, model="multivariate")

# Step 3 - Define the contrasts using the Seven Rules

# Max. number of orthogonal contrasts:
k - 1 = (4 x 2 group means) - 1 = max. of 7 contrasts

# What's the order of the group means?
EMM

# OK, so we need to build our max. 7 orthogonal contrasts based on this order:
contr <- list(
  psy_vs_wl = c(-0.25, -0.25, -0.25, -0.25, # Psychotherapy vs. wait list control conditions
                0.25, 0.25, 0.25, 0.25),
  psy_baseline_vs_FU = c(0, 0, 0, 0, # Psychotherapy: Baseline vs. any follow-up time 
                         -1, 1/3, 1/3, 1/3), 
  wl_baseline_vs_FU = c(-1, 1/3, 1/3, 1/3, # Wait list: Baseline vs. any follow-up time 
                        0, 0, 0, 0), 
  psy_vs_wl_earlyTx = c(0,-1, 0, 0, # Difference in functioning early in treatment?
                        0, 1, 0, 0),
  psy_vs_wl_lateTx = c(0, 0, -0.5, -0.5, # Difference in functioning late in treatment?
                       0, 0, 0.5, 0.5),
  psy_vs_wl_6moTx = c(0, 0, -1, 0, # Difference in functioning after 6 months of treatment?
                      0, 0, 1, 0),
  psy_vs_wl_12moTx = c(0, 0, 0, -1, # Difference in functioning after 12 months of treatment?
                         0, 0, 0, 1)
)

# Step 4 - Apply the contrasts to the EMM
contrast(EMM, method=contr) %>% summary(infer = TRUE) 