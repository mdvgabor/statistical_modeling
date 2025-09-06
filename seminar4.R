setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
# Read the Cancer Survival Times dataset
library(readxl)
surv <- read_excel("CancerSurvival.xlsx") # Survival times after cancer in months

# Frequency distribution
hist(surv$SurvMonth) # cont with long right tail --> Exponential(lambda)

# Maximum Likelihood
# we should find those distribution parameters that maximize the chance of our
# observed sample occurring in an IID sampling process

# P(Data | Parameter) --> max(Parameter)

# define th negative log-likelihood of the sample as a custom function in R
neg_ll_exp <- function(lambda){
  return(-sum(log(dexp(x = surv$SurvMonth, rate = lambda))))
}

# do the optimization
results <- optim(0.02, neg_ll_exp)
results$value # minimized negative log-likelihood
results$par # estimated lambda
1/mean(surv$SurvMonth)

# Let's try fitting a normal distribution on these data
neg_ll_norm <- function(parameters){
  return(-sum(log(dnorm(x = surv$SurvMonth,
                        mean = parameters[1],
                        sd = parameters[2]))))
}

res_norm <- optim(c(60,60), neg_ll_norm)
res_norm$par
# mean = 226.4
# st dev = 271.58
mean(surv$SurvMonth) # 226.17
sd(surv$SurvMonth) # 273.9 --> corrected st dev (in the end: /(n-1))
sqrt(mean((surv$SurvMonth-mean(surv$SurvMonth))^2)) # 271.57 --> uncorrected st dev

# Means Squared Error MSE = Bs^2 + SE^2

# Maximum Likelihood Estimations always produces the estimate with the smallest MSE
# "most efficient"

# let's see both fitted density function on the histogram
library(ggplot2)

ggplot(surv, mapping = aes(x=SurvMonth)) +
  geom_histogram(aes(y=after_stat(density))) +
  stat_function(fun = dexp,
                args = list(rate = results$par),
                col = "blue") +
  stat_function(fun = dnorm,
                args = list(mean = res_norm$par[1], sd = res_norm$par[2]),
                col = "red")

# Comparing the maximized log-likelihoods or minimized negative ll-s
res_norm$value
results$value

# Generally: the normal distribution has an "advantage" when we fit it to obserevd data
#            compared to the exponential distribution

# Normal distribution is more likely to overfit on the data, since it has more parameters
# Overfitting: my distribution only fits on the data because its more flexible
#              however it's not the "true" distribution of the data, so
#              when predicting for the population (out-of-sample) it makes large mistakes

# Solution: apply Information Criterion Measures  -->  ICs
# IC = f(neg log-likelihood) + g(p) --> minimize
# p = number of fitted (estimated) parameters

# Akaike IC = AIC = -2*ll + 2*p
# Bayes-Schwarz IC = BIC = SBC = -2*ll + p*ln(n)

# AIC < ... < BIC

# External package in R for fitting distribution with Maximum Likelihood
library(fitdistrplus)

fit_exp <- fitdist(surv$SurvMonth, "exp", method = "mle")
summary(fit_exp)

fit_norm <- fitdist(surv$SurvMonth, "norm", method = "mle")
summary(fit_norm)

denscomp(list(fit_exp, fit_norm))

#--------------------------------------------------------------------------
# Read Balaton swim data and the 10000 IID samples generated from it

swim_population <- read_excel("LIDLBalaton2022.xlsx")
swim_samples <- read_excel("swim_samples.xlsx")
swim_samples <- as.data.frame(swim_samples)
rownames(swim_samples) <- paste0("Sample", 1:nrow(swim_samples))

sample_5 <- as.numeric(swim_samples[5,])

# Function for the "original" uncorrected variance formula
class_var <- function(x){
  pop_variance <- mean((x - mean(x))^2)
  return(pop_variance)
} 

# Parameters: statistical measures computed from population data
PopMean <- mean(swim_population$TIME)

# Estimators: statistical measures computed from sample data
mean(sample_5)

# Sampling Error = Estimator - Parameter
# Vision & Mission --> approximate Sampling Error from data of just one sample

# Calculate the sample means in all 10000 samples
swim_samples$sample_means <- apply(swim_samples, 1, mean)
head(swim_samples[,99:101])

# standard deviation of the estimators in lots of samples
sqrt(class_var(swim_samples$sample_means)) # 4.4 minutes

# If estimator is Unbiased --> these st deviations are the expected differences
# between one sample's estimator and the true parameter value = standard errors

# standard errors with the simplified formulas

# mean
PopVariance <- class_var(swim_population$TIME)
sqrt(PopVariance)/sqrt(100) # 4.4 minutes

# apply simplified standard error formula on 1 sample!!!

# mean
sqrt(var(sample_5)/100) # SE approximated as 4.19 minutes

mean(sample_5)
# Population Mean: 167.17 +- 4.19 minutes
# in this case this estimation interval is correct
# it contains the true population mean

# Let's calculate the "hit rate" for this estimation interval for all the
# 10000 samples --> "cheating: SE formula I use the pop st deviation"

SE_Mean <- sqrt(PopVariance)/sqrt(100)

swim_samples$mean_low <- swim_samples$sample_means - SE_Mean
swim_samples$mean_upp <- swim_samples$sample_means + SE_Mean

head(swim_samples[,101:103])

mean(swim_samples$mean_low < PopMean & PopMean < swim_samples$mean_upp)
# ~ 68% --> 2/3 --> maybe its ok?
# in 1/3 of the samples I'm bound to miss :(

# NEW Vision&Mission: find a way to influence this hit rate of 68%
# "hit rate" = confidence level of the estimate

# look at the freq distribution of the 10000 sample means
hist(swim_samples$sample_means)
# this is the nicest normal distribution we can ask for Christmas

# This is because of the CLT!! --> because we have IID samples!!
# CLT: X_1, ..., X_n random variables that are IID
# Then sum(X_1 + ... + X_n) ~ Normal if n is large enough

# sample elements: y_1, ..., y_n --> can be considered as IID random variables
# sample mean = sum(y_1, ..., y_n)/n --> 1/n * sum(y_1, ..., y_n) ~
# ~ Normal(PopMean, SE)
# SE = Pop St Deviation / sqrt(n)

ggplot(swim_samples, aes(x=sample_means)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm,
                args = list(mean = PopMean, sd = SE_Mean),
                col="red", linewidth=1)


swim_samples$z <- (swim_samples$sample_means - PopMean)/SE_Mean
# Distribution of z? N(0,1)
mean(swim_samples$z) # ~ 0.0
sd(swim_samples$z) # ~ 1.0

# P(-1 < z < +1) = 68%

# P(-1 < (sample_mean-pop_mean)/SE < +1) = 68% --> solve for pop_mean

# P(sample_mean -1*SE < pop_mean < sample_mean +1*SE) = 68%

# P(-2 < z < +2) = 95%

# P(sample_mean -2*SE < pop_mean < sample_mean +2*SE) = 95%

swim_samples$mean_low95 <- swim_samples$sample_means - SE_Mean*2
swim_samples$mean_upp95 <- swim_samples$sample_means + SE_Mean*2

# hit rate this way
mean(swim_samples$mean_low95 < PopMean & PopMean < swim_samples$mean_upp95)
# 95% as expected!

# Formula for a Confidence Interval of the Mean
# sample_mean +- SE*k
# k = confidence multiplier --> comes from the st normal distr
# length of confidence interval = margin of error = estimate precision = 
# = SE*k = delta

# how to calculate 'k' multiplier from st normal distr in a simpler way
# alpha = probability of mistake = 1 - conf level
alpha <- 1-0.95 # 95% confidence
qnorm(1-alpha/2)
# ~ 1.96

# margin of estim error = SE * k
qnorm(1-0.1/2) # 90% -> 1.64
qnorm(1-0.01/2) # 99% -> 2.57
qnorm(1-0/2) # 100% -> infinity

#--------------------------------------------------------------------------
# Efficiency of Sleeping Pills Data

SampleData = c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4)
# 10 patients suffering from insomnia --> assumption: IID sample
# --> new sleeping pill --> variable: change in sleeptime after taking the pills (hours)

SampleMean = mean(SampleData)
SampleMean # 2.3 hours

# Marketing Purposes: find a mean that is true for unobserved patients (population)

# CI --> sample_mean +- SE*k
# conf level = 95%

alpha <- 1-0.95
k_z <- qnorm(1-alpha/2)

# SE = st. deviation / sqrt(n)
n <- length(SampleData)

# the previously derived CI formula contained the population standard deviation
assumed_pop_st_deviation <- 2 # assumed st dev of the sleeptime changes
# "from previous examinations we can assume that the st deviation is 2"

SE <- assumed_pop_st_deviation/sqrt(n)
# 0.6 hours

# interval estimation
c(SampleMean-SE*k_z, SampleMean+SE*k_z)
# the mean sleeptime change for patients who are NOT observed is between
# 1.09 and 3.57 hours with 95% probability

# Communication: 1.09 --> prudent estimation

# Now: calculate the same thing but using sample st deviation
# sample st dev --> corrected version --> unbiased

corrected_st_dev_sample <- sd(SampleData)
# 2 hours

SE <- corrected_st_dev_sample/sqrt(n)
# 0.6 hours

# extra uncertainty: changed something "I knew" (pop st dev) to something
# I calculated from sample data alone (corrected st dev)

# I need to show this extra uncertainty in the conf interval
# calculate the k multiplier from t(n-1) distribution

k_t <- qt(1-alpha/2, df=(n-1))

c(k_z, k_t) # both multipliers are for the same 95% confidence

# conf interval
c(SampleMean-SE*k_t, SampleMean+SE*k_t)
# 0.89 - 3.76 hours

qt(1-alpha/2, df=(500-1))
# sample size increases the diff between k_z and k_t disappears

# our cut-off --> n=50 --> n > 50 --> "large sample"

# in small samples (n <= 50) we also need for assume for the CI that the original data
# (sleeptime changes) is normally distributed as well
hist(SampleData)

# otherwise, in large samples no assumptions are needed for the CIs to work

# New Data: ESS2020.xlsx --> European Social Survey Hungarian Respondents
# Random Sample (IID-like sample) from the adult Hungarian population

ESS <- read_excel("ESS2020.xlsx")

# NetUsePerDay --> Estimate average for the whole Hun Pop 97%

# Sample Size
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
# n = 1099 > 50 --> large sample
# data's distribution does not matter
# does not matter how I calc the k (st norm or t distribution)

SE <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE)/sqrt(n)

alpha <- 1-0.97
k_z <- qnorm(1-alpha/2)

# conf interval
c(mean(ESS$NetUsePerDay_Minutes, na.rm = TRUE) -SE*k_z,
  mean(ESS$NetUsePerDay_Minutes, na.rm = TRUE) +SE*k_z)
# 172 - 191 minutes a day

margin_of_error <- SE*k_z
# 9.5 minutes

# let's make it 4.75 minutes (9.5/2)

# 1) change the conf level: lower confidence = lower 'k' = lower margin of error
#    ON THE COST THAT YOU LOSE CONFIDENCE

# 2) if you don't want to change the level of confidence
#    INCREASE THE SAMPLE SIZE
# SE = st_dev/sqrt(n) --> higher 'n' = lower SE = lower margin of error

# SE * k = 4.75
# SE * 2.17 = 4.75
# sample_st_dev/sqrt(n) * 2.17 = 4.75 --> solve for 'n'

# n = (sample_st_dev * 2.17/4.75)^2
(sd(ESS$NetUsePerDay_Minutes, na.rm=TRUE)*k_z/4.75)^2
# 4403.32 --> n_needed = 4404 people

# General CI Formula of the Mean = sample_mean +- SE*k

# 1) when we assumed something on the pop st dev --> k: qnorm
# 2) when we assumed that the data is normally dist --> k: qt(df=(n-1))
# 3) when we have large sample (n > 50) --> nothing else matters

# Student t distribution: chief quality engineer of Guinness (real name: William Gosset)