setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

library(readxl)
ESS <- read_excel("ESS2020.xlsx")
# a random sample from the Hungarian population, size is n=1849

# Analyize: NetUsePerDay
NetUseData <- ESS$NetUsePerDay_Minutes[!is.na(ESS$NetUsePerDay_Minutes)]

n <- length(NetUseData)

# Conf Interval for the standard deviation of net usage time with 97% confidence
# !!! EXCEPTION !!!

s <- sd(NetUseData)
alpha <- 1-0.97

low <- sqrt((n-1)*s^2/qchisq(1-alpha/2, df=(n-1)))
upp <- sqrt((n-1)*s^2/qchisq(alpha/2, df=(n-1)))
c(low,upp)
# st deviation of daily internet usage usage time in the Hungarian Population
# is between 139 and 152 minutes with a prob of 97%

# ASSUMPITON: variable we are examining is normally distributed
hist(NetUseData) # long right tail :(
# confidence is NOT 97% but something lower


# MAIN PROBLEM: do not have a formula for Standard Error (SE)

# SOLUTION: Bootstrap Simulations

# Original Def of SE:
# average difference between an estimator and the true population parameter
# --> (if estimator is unbiased)
# Calculate the SE as the SD(Estimators) from lots of samples


# Bootstrap Trick: take IID subsamples from our observed samples with the same size

sample(NetUseData, size = length(NetUseData), replace = TRUE) # repeat this lots of times

# "repeat this lots of times" = Replication Number = 10000

set.seed(17)
boot_subsamples <- sample(NetUseData, size = length(NetUseData), replace = TRUE)

# add the remaining subsamples from a for loop
for (i in 1:(10000-1)) {
  set.seed(17+i)
  boot_subsamples <- rbind(boot_subsamples,
                           sample(NetUseData, size = length(NetUseData), replace = TRUE))
  # table 10000 rows x 1099 columns
}

boot_subsamples <- as.data.frame(boot_subsamples)

rownames(boot_subsamples) <- paste0("Subsample",1:nrow(boot_subsamples))
colnames(boot_subsamples) <- paste0("Elements", 1:length(NetUseData))
head(boot_subsamples[,c(1,2,1098,1099)])

# calculate any kind of estimators for every subsample

boot_subsamples$means <- apply(boot_subsamples, 1, mean)
boot_subsamples$corr_sd <- apply(boot_subsamples[,1:1099], 1, sd)
head(boot_subsamples[,1096:1101])

classical_sd <- function(x) sqrt(mean((x-mean(x))^2))

SE_Mean_Boot <- classical_sd(boot_subsamples$means)
SE_Mean_Formula <- s/sqrt(n)

SE_StDev <- classical_sd(boot_subsamples$corr_sd)
# 6.49 minutes --> average difference of a sample st dev from the true pop st dev is
#                  6.49 minutes

# Bootstrap Conf Interval for Mean with 97% conf
hist(boot_subsamples$means) # Nice & Normal (CLT)
# Conf Int.: looked for two values in this Normal Dist between which
# we have conf level % of the sample means
# alfa/2 and 1-alfa/2
boot_ci_mean <- quantile(boot_subsamples$means, probs = c(alpha/2,1-alpha/2))

k <- qnorm(1-alpha/2) # sample size is large (n>50)
formula_ci_mean <- c(mean(NetUseData)-SE_Mean_Formula*k,
                     mean(NetUseData)+SE_Mean_Formula*k)


# Bootstrap Conf Interval for St Dev with 97% conf
hist(boot_subsamples$corr_sd) # something similar to normal, but not quite
# maybe different kurtosis maybe a bit of left tail? --> not sure
# i don't need to care

boot_ci_sd <- quantile(boot_subsamples$corr_sd, probs = c(alpha/2,1-alpha/2))
# st dev of Hun Pop internet usage time: 131-159 minutes with 97% probability
# from the chi-squared formula: 139 and 152 minutes (WRONG: data is NOT normal)


# Bootstrap Conf Interval of the Median --> built-in function: boot package
library(boot)

# write a custom function for the statistical measure You want a conf interval for
bootMedian <- function(data, indices) median(data[indices])

set.seed(17)
boot_result <- boot(NetUseData, bootMedian, R=10000)

boot_result
# what this functiun says as bias --> mean(simulated medians) - sample_median
hist(boot_result$t[,1]) # distribution of simulated sample medians

# conf interval with 97% conf level for the median
boot.ci(boot_result, conf = 0.97, type = "perc")
# in the Hungarian population median internet usage time is between 120 and 150 minutes
# 97% probability

# 2 assumptions: sampling method is "IID"-like (IID or SR with small selection ratio)
#                estimator we are using is unbiased

#-----------------------------------------------------------------------------------------
# HYPOTHESIS TESTING

swimmer_population <- read_excel("LIDLBalaton2022.xlsx")
samples_100 <- read_excel("swim_samples.xlsx")

# 1. Null + Alternative Hypothesis (H0, H1)
# 2. Calculating test statistic (from observed sample data)
# 3. Calculating p-value
# 4. Based on the results --> H0 or H1 TRUE?

# Case of the mean --> parameter = population mean

# Statement #1: mean swimming time is smaller than 167.5 minutes
# Statement #2: mean swimming time is greater than 150 minutes

# Step #1

# S1 --> H0 TRUE
# H0: pop_mean = 167.5
# H1: pop_mean < 167.5

# S2 --> H1 TRUE
# H0: pop_mean = 150
# H1: pop_mean > 150

# easy job --> pop_mean
pop_mean <- mean(swimmer_population$TIME)

# Goal: find out whether H0 or H1 is true from just 1 sample (n=100) alone

sample_17 <- as.numeric(samples_100[17,])
sample_17_mean <- mean(sample_17)
# what we need to show:
# sample_17_mean is SIGNIFICANTLY different from 150 --> H1
# sample_17_mean is NOT significantly different from 167.5 --> H0

# Step 2 --> Calculating the test statistic
# (sample_mean - theoretical_mean)/SE_mean
# SE_mean = corr_sd/sqrt(n)

samples_100$means <- apply(samples_100[,1:100],1,mean)
samples_100$corr_sd <- apply(samples_100[,1:100],1,sd)

samples_100$test_stat_H0true <- (samples_100$means - 167.5)/
  (samples_100$corr_sd/sqrt(100))
samples_100$test_stat_H0false <- (samples_100$means - 150)/
  (samples_100$corr_sd/sqrt(100))

head(samples_100[,101:104])

# if H0 is TRUE:
# (sample_mean - theoretical_mean)/SE_mean ~ N(0,1) = t(n-1) [sample size is large]

# lets' show this on a histogram
library(ggplot2)
ggplot(samples_100) +
  geom_histogram(aes(x=test_stat_H0true, y=after_stat(density), fill="H0 TRUE")) +
  geom_histogram(aes(x=test_stat_H0false, y=after_stat(density), fill="H0 FALSE")) +
  stat_function(fun = dt,
                args = list(df = (100-1)),
                col="blue", linewidth=1)

# Decide based on the assumption that when H0 is true, then the test statistic is
# t(n-1) distributed
# How likely it is that the test stat is coming from a t(n-1) distribution?

# Based Sample #6 --> there will be errors --> 
# Type I Error: Rejecting a true H0
# Type II Error: Accepting a false H0

# Control Type I Errors --> their probability is defined as SIGNIFICANCE LEVEL = alpha

# Critical Values --> quantile fictions
alpha <- 0.05

# Statement#1 --> left-tailed
crit_low <- qt(alpha, df=100-1)
# test_stat < crit_low --> H1


# Statement#2 --> right-tailed
crit_upp <- qt(1-alpha, df=100-1)
# test_stat > crit_upp --> H1

# Let's use these decision rules --> See Type I Errors
# --> only interpretable for Statement #1 --> we know that H0 is TRUE
samples_100$decisions_H0true <- ifelse(samples_100$test_stat_H0true < crit_low,
                                       "H1", "H0")
head(samples_100[,103:105])

mean(samples_100$decisions_H0true=="H1") # 5.9% ~ alpha = 5% or
#                                        # 1.6% ~ alpha = 1%

# See Type II Errors
# --> only interpretable for Statement #2 --> we know that H0 is FALSE
samples_100$decisions_H0false <- ifelse(samples_100$test_stat_H0false > crit_upp,
                                        "H1", "H0")

mean(samples_100$decisions_H0false=="H0") # alpha = 5% --> 0.5%
#                                           alpha = 1% --> 3.6%

# Beta = prob of Type II Errors --> when we have only 1 sample: DON'T KNOW THIS

# don't say "accepting the H0" but we say that "WE FAIL TO REJECT THE H0"

# There are some samples where the decision can change between H0 and H1 when we
# change alpha --> we can only see this if we recalculate the critical values

# Solution: p-value --> treat the test statistic itself as a critical value
# recalculate the significance level from it --> alpha where the DECISION CHANGES
# between H0 and H1

# Calc the p-value
samples_100$pvalue_H0true <- pt(samples_100$test_stat_H0true, df=100-1) # left-tailed
samples_100$pvalue_H0false <- 1-pt(samples_100$test_stat_H0false, df=100-1) # right-tailed
head(samples_100[,107:108])

# p-value = empirical prob of Type I Errors
# --> large when H0 true
# --> small when H0 false

# p-value = signif level where the decision changes between H0 and H1
# p-value < alpha --> H1 --> NOT SCARED
# p-value >= alpha --> H0 (fail to reject) --> SCARED

# if alpha ~ p-value --> decision is NOT stable
# common significance levels (Type I Error probabilities with usually
#                              small Type II error prob)
# 1% - 10%
# if 1% <= p-value <= 10% --> NOT TO DECIDE --> decision not stable
# if p-value < 1% --> H1 (instead of 1% sometimes 0.1%)
# if p-value > 10% --> H0

boxplot(samples_100[,107:108])

# this decision rule based on p-values is always the same!

# Examine 1 specific sample--> sample # 17

test_stat_17_H0true <- samples_100$test_stat_H0true[17]
test_stat_17_H0false <- samples_100$test_stat_H0false[17]

p_value_17_H0true <- samples_100$pvalue_H0true[17]
p_value_17_H0false <- samples_100$pvalue_H0false[17]

# Statement #2 --> p-value < 1% --> reject H0 --> good decision (H0 false)
# Statement #1 --> 1% < p-value < 10% --> not deciding (leaning towards H0)
#              --> alpha = 10% --> reject H0 --> Type I Error (H0 true)
#              --> alpha = 5% --> not reject H0 --> good decision (H0 true)

t.test(sample_17, mu = 167.5, alternative = "less") # Statement #1
t.test(sample_17, mu = 150, alternative = "greater") # Statement #2

# t-test for the mean --> p-value is from a t(n-1) distribution

# Assumptions for hypothesis tests of the mean

# t-test --> p-value is from t(n-1) distribution
# 1) large sample size (n>=50)
# 2) small sample size (n<50) & your variable is normally distributed

# z-test --> p-value is from N(0,1) distribution (test stat is the same)
# 1) large sample size (n>=50)
# 2) small sample size (n<50) & population st dev is known for the SE

# Hypothesis test for the ESS data

ESS <- read_excel("ESS2020.xlsx")

# Variable: education years

# Statement: average Hungarian in the population is spending more years in education than
#            12 years (primary+high school)

# Step 1 --> H0+H1

# H0: pop_mean = 12 (mean of educ years is not significantly diff from 12)
# H1: pop_mean > 12 (mean of educ years is significantly greater than 12)

# Step 2+3 --> Calculations
t.test(ESS$Education_Years, mu = 12, alternative = "greater")

# p-value = 0.7% --> < 0.01=1% --> rejecting the H0 on all common signif levels
# --> H1: mean of educ years is significantly greater than 12

# n = 1830 ---> large sample size --> no need to check normal dist
# sample_mean = 12.19

# sample size increases --> smaller differences between sample and hypothetical means
# can be considered as significant
