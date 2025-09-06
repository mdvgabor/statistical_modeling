setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)
library(ggplot2)

Tesla <- readxl::read_excel("TSLA.xlsx")

ggplot(data = Tesla, mapping = aes(x = TESLA)) +
  geom_histogram(aes(y = after_stat(density)))

mu_T <- mean(Tesla$TESLA)
sigma_T <- sd(Tesla$TESLA)

# dnorm() probability at point x
# pnorm() cumulative probability until x

# i-th price change of Tesla = T_i
# T_i ~ N(1.78, 27)

# P(T_i = 30) = f(30) price chage is exactly 30

dnorm(x = 30, mean = 1.78, sd = 27)

# P(T_i < -30) --> cumulative distribution function
#                 the price change is more or equal to -30

pnorm(-30, mean = mu_T, sd = sigma_T)

# P(T_i > 20) --> cumulative distribution function
#                 the price change is more or equal to 20
1-pnorm(20, mean = mu_T, sd = sigma_T)

# P(-40 < T_i < -20)

pnorm(-20, mean = mu_T, sd = sigma_T) - pnorm(-40, mean = mu_T, sd = sigma_T)

# What is the value that a bigger loss occurring has only 5%
# probability
# P(T_i < x) = 0.05 --> x=?
# quantile function

qnorm(0.05, mean = mu_T, sd = sigma_T)

# ------------------------------------------------------------------------------

# Simulate/Generate data that behave like a specific prob distribution

# Basic tool: random numbers between 0 and 1

# random numbers from Uniform(0,1)

unlucky_numbers <- runif(n = 50, min = 0, max = 1)
unlucky_df <- as.data.frame(unlucky_numbers)


#-------------------------------------------------------------------------------

# Central Limit Theorem = CLT --> Simulate how this works

# Let's assume: X_1, X_2, ..., X_n random variables that are
#                                  independent and identically distributed
# E(X_i) = mu and SD(X_i) = sigma

# SUM(X_1, X_2, ..., X_n) ~ N(n*mu, sqrt(n)*sigma)

# 1st Simulation: X_i ~ Exp(0.1) and n=10

ClT_n10 <- sapply(1:10000, function(x) sum(rexp(n = 10, rate = 0.1)))
hist(ClT_n10)

#-------------------------------------------------------------------------------

swim_population <- readxl::read_excel("LIDLBalaton2022.xlsx")

set.seed(17)

row_numbers_sample <- sample(rownames(swim_population), size = 100, replace = TRUE)
swimmer_sample <- swim_population[row_numbers_sample,]

#Population Parameters

class_var <- function(x){ # assume: x is a numeric vector
  pop_variance <- mean((x - mean(x))^2)
  return(pop_variance)
} 

PopMean <- mean(swim_population$TIME)
SampleMean <- mean(swimmer_sample$TIME)
Sampling_error_mean <- PopMean - SampleMean

samples <- swimmer_sample$TIME
for (i in 1:9999) {
  set.seed(17+i)
  samples <- rbind(samples, sample(swim_population$TIME, size = 100, replace = TRUE))
}
samples <- as.data.frame(samples)

rownames(samples) <- paste0("Sample", 1:10000)
colnames(samples) <- paste0("Observation", 1:100)

#-------------------------------------------------------------------------------

PopVariance <- class_var(swim_population$TIME)
PopProportion <- sum(swim_population$TIME>180)/
  nrow(swim_population)

# 1st Calculate every estimator in every sample

samples$sample_means <- apply(samples, 1, mean)
samples$sample_vars <- apply(samples[, 1:100], 1, class_var)
samples$sample_props <- apply(samples[, 1:100], 1, function(x) sum(x>180) / length(x))

# calculate corrected variance
# Var(IID sample) --» biased = (n-1)/n * pop_var
# E(sample variances) * n/(n-1) = pop variance

samples$corrected_vars <- samples$sample_vars*100/99
mean(samples$corrected_vars)
samples$corrected_vars2 <- apply(samples[,1:100],1,var )
mean(samples$corrected_vars2)

# 3rd step --> standard deviation of the estimators in lots of samples
sqrt(class_var(samples$sample_means)) #4.44
sqrt(class_var(samples$sample_props)) #0.04

# mean estimation : 

# Sampling Error for Unbiased Estimators = Standard Error = SQRT(Var(Estimator))
# E[(Estimators - Parameter)^2] = MSE

MSE_Uncorrected_Variance <- mean((samples$sample_vars - PopVariance)^2)

# MSE = SE^2 + Bs^2

SE_Uncorrected_Variance <- sqrt(class_var(samples$sample_vars))
BS_Uncorrected_Variance <- mean(samples$sample_vars) - PopVariance
MSE <- SE_Uncorrected_Variance^2 + BS_Uncorrected_Variance^2


# MSE(Corrected Variance) = SE^2(Corrected Variance)

MSE_Corrected_Variance <- class_var(samples$corrected_vars)

#-------------------------------------------------------------------------------

# Methods:
# 1) Method of Moments
# 2) Maximum Likelihood Method

surv <- readxl::read_excel("CancerSurvival.xlsx")
num_claims <- readxl::read_excel("insurance_examples.xlsx",
                         sheet = "CarInsurance_NumberOfClaims")
hist(surv$SurvMonth)
hist(num_claims$NumClaims)

# Both cases: what is lambda?

# 1) Method of Moments

# 1st non-centered moment
# in the sample = mean
# in the distribution = expected value

# 2nd centered moment
# in the sample = corrected variance
# in the distribution = variance

# Assumption: sample moment = distribution moment

# Exp(lambda)

# expected value = sample mean
# 1/lambda = sample mean
# lambda = 1/sample mean

# Poi(lambda)
# expected value = sample mean
# lambda = sample mean

# N(mu, sigma)
# mu = sample mean
# sigma = sample (corrected) st deviation

# let's try it


lambda_exp <- 1/mean(surv$SurvMonth)
lambda_poi <- mean(num_claims$NumClaims)

# Poisson: let's see how well the distribution fits our data
observed_frequency <- table(num_claims$NumClaims)
observed_frequency

expected_freq <- round(dpois(x=0:4, lambda = lambda_poi)*400,0)
names(expected_freq) <- 0:4
expected_freq

car_fleet <- readxl::read_excel("insurance_examples.xlsx", sheet = "CarInsurance_Fleet")
barplot(table(car_fleet$NumClaims_Fleet))
# distribution to fit: Binom(m, p)

# Method of Moments

# expected value = sample mean
# m * p = sample mean

# distribution variance = corrected variance of the sample
# m * p * (1-p) = corrected sample variance

# substitute 1. to 2.
# sample mean * (1-p) = variance
# p = 1- var/mean
p_binom <- 1-var(car_fleet$NumClaims_Fleet)/mean(car_fleet$NumClaims_Fleet)

# go back to 1.
# m = mean/estimated_p
m_binom <- mean(car_fleet$NumClaims_Fleet)/p_binom
m_binom <- ceiling(m_binom)

# Method of Moments (MM) Advantage: simple
# Disadvantage: it only considers "special values" (moments) of the distribution and
#               observed sample and not the whole data

# Maximum Likelihood
# we should find those distribution parameters that maximize the chance of our
# observed sample occurring in an IID sampling process

# P(Data | Parameter) --> max(Parameter)

# 1st P(Observation | Parameter)
# Exponential with the survival data
# 3rd element in the surv data is 10.42 --> P(10.42 | lambda = 0.02)

# calculate the prob of all 58 values occurring in Exp(0.02) separately
dexp(x = surv$SurvMonth, rate = 0.02)

# P(Data | Parameter) = Product(P(Observation | Parameter)) BECAUSE first I in IID
# likelihood function of the sample
prod(dexp(x = surv$SurvMonth, rate = 0.02)) # it's always ~ 0

# "logarithmize that is"
# log(P(Data | Parameter)) = log(Product(P(Observation | Parameter))) =
# = SUM(log(P(Observation | Parameter)))
# log-likelihood function
sum(log(dexp(x = surv$SurvMonth, rate = 0.02)))


# define this log-likelihood of the sample as a custom function in R
ll_exp <- function(lambda) {
  return(sum(log(dexp(x = surv$SurvMonth, rate = lambda))))
}

ll_exp(0.02)
ll_exp(0.002)

# R can only minimize a function and NOT maximize
neg_ll_exp <- function(lambda){
  return(-ll_exp(lambda))
}
neg_ll_exp(0.02)
neg_ll_exp(0.002)

# do the optimization
results <- optim(0.02, neg_ll_exp)
results$value # minimized negative log-likelihood
results$par # estimated lambda

# ML fit with Binomial distribution for the car fleet data

neg_ll_binom <- function(parameters) {
  return(-sum(log(dbinom(x = car_fleet$NumClaims_Fleet,
                         size = parameters[1], prob = parameters[2]))))
}

neg_ll_binom(c(6, 0.04))

res_binom <- optim(c(9, 0.07), neg_ll_binom)
res_binom$par

#-------------------------------------------------------------------------------
surv <- readxl::read_excel("CancerSurvival.xlsx") # Survival times after cancer in months

neg_ll_norm <- function(parameters){
  return(-sum(log(dnorm(x = surv$SurvMonth, mean = parameters[1], sd = parameters[2]))))
}

result <- optim(c(60,60), neg_ll_norm)
result$par
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

ggplot(data = surv, mapping = aes(x = SurvMonth)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, args = list(result$par[1], result$par[2]), col="red")

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
fit_exp <- fitdist(data = surv$SurvMonth, distr = "exp", method = "mle")
summary(fit_exp)
fit_norm <- fitdist(data = surv$SurvMonth, distr = "norm", method = "mle")
summary(fit_exp)

denscomp(list(fit_exp, fit_norm))

#-------------------------------------------------------------------------------
library(readxl)
swim_population <- read_excel("LIDLBalaton2022.xlsx")
swim_samples <- read_excel("swim_samples.xlsx")
swim_samples <- as.data.frame(swim_samples)
rownames(swim_samples) <- paste0("Sample", 1:10000)

sample_5 <- as.numeric(swim_samples[5,])

#uncorrected variance function
class_var <- function(x){
  pop_variance <- mean((x-mean(x))^2)
  return(pop_variance)
}
# Sampling Error = Estimator - Parameter
# Vision & Mission --> approximate Sampling Error from data of just one sample

# Calculate the sample means in all 10000 samples
swim_samples$sample_means <- apply(swim_samples, 1,mean)

# standard deviation of the estimators in lots of samples
sqrt(class_var(swim_samples$sample_means)) # 4.44

# If estimator is Unbiased --> these st deviations are the expected differences
# between one sample's estimator and the true parameter value = standard errors

# standard errors with the simplified formulas

# mean
PopVariance <- class_var(swim_population$TIME)
sqrt(PopVariance) / sqrt(100) #4.4

# apply simplified standard error formula on 1 sample!!!

# mean
sqrt(var(sample_5)/100) # SE approximated as 4.19 minutes

mean(sample_5)
# Population Mean: 167.17 +- 4.19 minutes
# in this case this estimation interval is correct
# it contains the true population mean

# Let's calculate the "hit rate" for this estimation interval for all the
# 10000 samples --> "cheating: SE formula I use the pop st deviation"

SE_mean <-sqrt(PopVariance) / sqrt(100)

swim_samples$mean_low <- swim_samples$sample_means - SE_mean
swim_samples$mean_up <- swim_samples$sample_means + SE_mean

head(swim_samples[,101:103])

# Formula for a Confidence Interval of the Mean
# sample_mean +- SE*k
# k = confidence multiplier --> comes from the st normal distr
# length of confidence interval = margin of error = estimate precision = 
# = SE*k = delta

# how to calculate 'k' multiplier from st normal distr in a simpler way
# alpha = probability of mistake = 1 - conf level

alpha <- 1-0.95

# margin of estim error = SE * k
qnorm(1-alpha/2)

#-------------------------------------------------------------------------------
# Efficiency of Sleeping Pills Data

SampleData = c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4)
# 10 patients suffering from insomnia --> assumption: IID sample
# --> new sleeping pill --> variable: change in sleeptime after taking the pills (hours)

hist(SampleData)

sample_mean <- mean(SampleData) #2.33

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

SE <- assumed_pop_st_deviation / sqrt(n) #0.6

#interval estimation
c(sample_mean -k_z*SE, sample_mean + k_z*SE)
# the mean sleeptime change for patients who are NOT observed is between
# 1.09 and 3.57 hours with 95% probability

# Communication: 1.09 --> prudent estimation

# Now: calculate the same thing but using sample st deviation
# sample st dev --> corrected version --> unbiased

corr_std_sample <- sd(SampleData)
# 2 hours

SE <- corr_std_sample / sqrt(n)

# extra uncertainty: changed something "I knew" (pop st dev) to something
# I calculated from sample data alone (corrected st dev)

# I need to show this extra uncertainty in the conf interval
# calculate the k multiplier from t(n-1) distribution

k_t <- qt(1-alpha/2, df=n-1)
# conf interval
c(sample_mean-SE*k_t, sample_mean+SE*k_t)
# 0.89 - 3.76 hours
# sample size increases the diff between k_z and k_t disappears

# our cut-off --> n=50 --> n > 50 --> "large sample"

# in small samples (n <= 50) we also need for assume for the CI that the original data
# (sleeptime changes) is normally distributed as well
hist(SampleData)

# otherwise, in large samples no assumptions are needed for the CIs to work

# New Data: ESS2020.xlsx --> European Social Survey Hungarian Respondents
# Random Sample (IID-like sample) from the adult Hungarian population

#-------------------------------------------------------------------------------
# New Data

ESS <- readxl::read_excel("ESS2020.xlsx")
# NetUsePerDay --> Estimate average for the whole Hun Pop 97%

# n = 1099 > 50 --> large sample
# data's distribution does not matter
# does not matter how I calc the k (st norm or t distribution)

n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
SE <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE) / sqrt(n)

alpha <- 1-0.97
k_z <- qnorm(1-alpha/2)

PopMean <- mean(ESS$NetUsePerDay_Minutes, na.rm = TRUE)

#confidence interval
c(PopMean - k_z*SE, PopMean + k_z*SE)
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


# ------------------------------------------------------------------------------
# Data "collect"

ESS <- readxl::read_excel("ESS2020.xlsx")
sample_mean <- mean(ESS$NetUsePerDay_Minutes, na.rm = TRUE)
s <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE) #corrected sd
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
alpha <- 1-0.97

# Calc "ingredients" for the CI
SE <- s/sqrt(n)
k <- qt(1-alpha/2, df = n-1)

# confidence interval
c(sample_mean -k*SE, sample_mean + k*SE)
# In the unobserevd Hungarian population the average internet usage time per day
# is between 172 and 191 minutes with prob of 97%

# automated CI calculation
library(rcompanion)

groupwiseMean(NetUsePerDay_Minutes ~ 1, data = ESS, na.rm = TRUE, conf = 0.97, digits = 5)

# let's calculate the same confidence interval but for each political party preference
internetuse_party <- groupwiseMean(NetUsePerDay_Minutes ~ PoliticalPartyPref,
                                   data = ESS,
                                   na.rm = TRUE, conf = 0.97, digits = 5)
internetuse_party

#----------------------------------------------------------------------------

# let's estimate the proportion of Fidesz supporters in the whole population
# with 99% confidence

# conf interval for the mean of a variable where Fidesz=1 and EElse=0
# --> Bernoulli(p) --> p = proportion of Fidesz supporters
# Bernoulli expected value = p

ESS$Fidesz <- ifelse(ESS$PoliticalPartyPref =="Fidesz-KDNP",1,0)

groupwiseMean(Fidesz ~ 1, data = ESS, na.rm = TRUE, conf = 0.99, digits = 5)
# prop of Fidesz supporters in the sample = 19.7%
# pop of supporters in the whole Hungarian pop 17%-22% with 99% prob

# Number of Fidesz supporters in the whole Hung pop?
# answer: need N (population size)

N <- 7500000
N*c(0.17,0.22)
# number of supporters is between 127 and 165 thousand people with 99% prob

# Assumption when estimating proportion: large sample size!
# Because: proportion is the mean of a Bernoulli distributed variable
# --> Bernoulli != Normal Dist --> t-distr 'k' factor does not work in small samples

# proportion conf intervals: large sample if:
# 1) n*p >= 10 --> number of favourable cases
# 2) n*(1-p) >= 10 --> num of unfavourable cases
# p = sample proportion

p <- mean(ESS$Fidesz)
n <- nrow(ESS)

n*p >=10 & n*(1-p) >= 10 # TRUE --> assumption is met!

# let's calculate the margin of error (CI length)
# sample_prop +- SE*k
# SE*k = margin of error
0.22128 - 0.1974
# margin of error = SE * k
margin_of_error <- 0.22128 - 0.1974
# 2.4 %-points with 99% confidence

# what is the required sample size to estimate prop of Fidesz supporters
# in the whole population with 1%-point margin of error and 99% confidence

# margin of error = SE * k
k <- qnorm(1-0.01/2) # 99% conf --> alpha = 1%
# SE = corr_st_dev/sqrt(n)
corr_std_dev <- sd(ESS$Fidesz)

# 0.01 = 0.398/sqrt(n) * 2.57
# n = (0.398*2.57/0.01)^2
(0.398*2.57/0.01)^2 # 10 463 people

# calculate this same 'n' but without using ANY sample data
# n = (???*2.57/0.01)^2

# 0/1 data has a Bernoulli distribution --> SD(Bernoulli(p)) = sqrt(p(1-p))
sqrt(p*(1-p)) # 0.398

(sqrt(p*(1-p))*2.57/0.01)^2


#-------------------------------------------------------------------------------

# Confidence Interval for St Deviation

# !!! EXCEPTION !!!

# we don't have a formula for st error (SE)

# Y = population data
# y = sample data

# Assume: Y ~ N(mu, sigma)

# lots of samples --> (n-1)*s^2/sigma^2 ~ Chi^2(n-1)

# Formula for Conf Int of St Dev
# L = sqrt[(n-1)*s^2/qchi2(1-alpha/2, df = n-1)]
# U = sqrt[(n-1)*s^2/qchi2(alpha/2, df = n-1)]

# Estimate the st deviation for internet usage times for the whole pop
# with 97% confidence

# Data
ESS <- readxl::read_excel("ESS2020.xlsx")
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
s <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE)
alpha <- 1-0.97
k_t <- qt(1-alpha/2, df = n-1)
SE <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE)/sqrt(n)

# Conf Interval
c(sqrt((n-1)*s^2/qchisq(1-alpha/2,df=n-1)),
  sqrt((n-1)*s^2/qchisq(alpha/2,df=n-1)))

margin_of_error <- SE*k_t
margin_of_error
# st dev of internet usage time in the Hungarian population is between
# 139 and 152 minutes with 97% probability
# --> !! 97% probability is uncertain, because the data is NOT Normal

#-------------------------------------------------------------------------------

# Simple Random Samples WITHOUT REPLACEMENT = SR Sample

# General Method
# sample_mean/proportion +- SE * k

# Change in How SE is calculated!! --> no replacement --> with every individual selected
# in the sample the probability of other elements to be selected in the future
# is modified --> Cov(y_i, y_j) != 0

# SE_SR = SE_IID * sqrt(1-n/N)

# n/N = selection ratio

# 97% confidence level estimation for the average net usage time
# but now let's treat the data as SR sample and NOT IID
# assuming N = 9.7 million

n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
sample_mean <- mean(ESS$NetUsePerDay_Minutes, na.rm = TRUE)
s <- sd(ESS$NetUsePerDay_Minutes,na.rm = TRUE)
N <- 9700000

SE_SR <- s/sqrt(n) * (1-n/N)

alpha <- 1-0.97
k_t <- qt(1-alpha/2, df = n-1)

# Conf Interval itself
c(sample_mean-SE_SR*k_t, sample_mean+SE_SR*k_t)
# 172 and 191 minutes

#-------------------------------------------------------------------------------
# Stratified Sample --> Proportionally Startified = PS

# Strata: j = 1,2,...,M

# Assume: n_j/n = N_j/N

HH <- readxl::read_excel("Households_Income.xlsx")

# HCSO: prop. stratified sample from Hungarian Households according to Settlement Type
# yearly Income in thousand HUF

# Conf. Interval for the mean Household Income for the Hungarian Households
# conf level 95%

# Sampling inside strata is SR --> N = 4.1 million households

# 1) let's do conf interval like it were a SR sample

# Data "collect"
sample_mean <- mean(HH$Income, na.rm=TRUE)
s <- sd(HH$Income, na.rm = TRUE) # correct st dev
n <- sum(!is.na(HH$Income))
alpha <- 1-0.95
N <- 4100000

SE <- s/sqrt(n) * sqrt(1-n/N)
k_t <- qt(1-alpha/2, df = n-1)

# Conf Interval itself
c(sample_mean-SE*k_t, sample_mean+SE*k_t)
# average Hungarian Household yearly income is between 4.58 and 4.71 million HUF
# with a 95% prob

# PS Sampling --> SE = s_w/sqrt(n)*sqrt(1-n/N)

# s_w --> within-stratum (corrected) st deviation

# s_w <= s

# table: rows = strata; columns: {sample size, mean, sd}

helper_table <- aggregate(Income ~ Settlement, data = HH, FUN = mean)
helper_table$sd <- aggregate(Income ~ Settlement, data = HH, FUN = sd)[,2]
helper_table$sample_size <- table(HH$Settlement)

# PS Sampling --> SE = s_w/sqrt(n)*sqrt(1-n/N)
s_w <- sqrt(sum(helper_table$sd^2*(helper_table$sample_size-1))/(n-1))

SE_PS <- s_w/sqrt(n)*sqrt(1-n/N)

k_t <- qt(1-alpha/2, df = n-1)

#conf int,
c(sample_mean-SE_PS*k_t, sample_mean+SE_PS*k_t)
# average household income in whole of Hungary between 4.58 and 4.71 million HUF
# with 95 prob

# eta^2 = variance-ratio = s_b^2/s^2 = 1 - s_w^2/s^2 --> variance: proportion --> %
1-s_w^2/s^2
# 1.9% --> settlement type influences only 1.9% of the variation of incomes

1-SE_PS^2/SE^2 # 1.9% --> sampling variance is decreased BY 1.9%

SE_PS^2/SE^2 # 98.1%
# sampling variance (SE^2) is decreased TO 98.1% by the stratification

# conf interval for the mean in large samples with different sampling methods

# Ranking in Efficiency
# 1) Prop Stratified (PS)
# 2) SR
# 3) IID

#-------------------------------------------------------------------------------
ESS <- readxl::read_excel("ESS2020.xlsx")
# a simple random sample from the Hungarian population, size is n=1849

# Analyize: NetUsePerDay

NetUseData <- ESS$NetUsePerDay_Minutes[!is.na(ESS$NetUsePerDay_Minutes)]
n <- length(NetUseData)

# Conf Interval for the standard deviation of net usage time with 97% confidence
# !!! EXCEPTION !!!

s <- sd(NetUseData)
alpha <- 1-0.97

low <- sqrt((n-1)*s^2 / qchisq(1-alpha/2, df = n-1))
up <- sqrt((n-1)*s^2 / qchisq(alpha/2, df = n-1))
c(low,up)
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
sample(x = NetUseData, size = length(NetUseData), replace = TRUE)

# "repeat this lots of times" = Replication Number = 10000
set.seed(17)
boot_subsamples <- sample(NetUseData, size = length(NetUseData), replace = TRUE)
# add the remaining subsamples from a for loop
for (i in 1:10000-1) {
  set.seed(17+i)
  boot_subsamples <- rbind(boot_subsamples, sample(NetUseData, size = length(NetUseData), replace = TRUE))
}
boot_subsamples <- as.data.frame(boot_subsamples)

rownames(boot_subsamples) <- paste0("Subsample", 1:nrow(boot_subsamples))
colnames(boot_subsamples) <- paste0("Observation", 1:length(NetUseData))

# calculate any kind of estimators for every subsample
boot_subsamples$sample_means <- apply(boot_subsamples,1,mean)
boot_subsamples$sample_sd <- apply(boot_subsamples[,1:1099],1,sd)

classical_sd <- function(x) {sqrt(mean((x-mean(x))^2))}

SE_Mean_Boot <- classical_sd(boot_subsamples$sample_means)
SE_Mean_Formula <- s/sqrt(n)

SE_StDev <- classical_sd(boot_subsamples$sample_sd)
# 6.49 minutes --> average difference of a sample st dev from the true pop st dev is
#                  6.49 minutes

# Bootstrap Conf Interval for Mean with 97% conf
hist(boot_subsamples$sample_means) # Nice & Normal (CLT)
# Conf Int.: looked for two values in this Normal Dist between which
# we have conf level % of the sample means
# alfa/2 and 1-alfa/2
boot_ci_mean <- quantile(boot_subsamples$sample_means, probs = c(alpha/2, 1-alpha/2))
k <- qnorm(1-alpha/2) # sample size is large (n>50)
formula_ci_mean <- c(mean(NetUseData)-SE_Mean_Formula*k,
                     mean(NetUseData)+SE_Mean_Formula*k)

# Bootstrap Conf Interval for St Dev with 97% conf
hist(boot_subsamples$sample_sd) # something similar to normal, but not quite
# maybe different kurtosis maybe a bit of left tail? --> not sure
# i don't need to care
boot_ci_sd <- quantile(boot_subsamples$sample_sd, probs = c(alpha/2,1-alpha/2))
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

swimmer_population <- readxl::read_excel("LIDLBalaton2022.xlsx")
samples_100 <- readxl::read_excel("swim_samples.xlsx")

# 1. Null + Alternative Hypothesis (H0, H1)
# 2. Calculating test statistic (from observed sample data)
# 3. Calculating p-value
# 4. Based on the results --> H0 or H1 TRUE?

# Case of the mean --> parameter = population mean

# Statement #1: mean swimming time is smaller than 167.5 minutes
# Statement #2: mean swimming time is greater than 150 minutes


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

samples_100$means <- apply(samples_100,1,mean)
samples_100$corr_sd <- apply(samples_100[,1:100], 1,sd)

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

# Calc the p-value
samples_100$pvalue_H0true <- pt(samples_100$test_stat_H0true, df=100-1) # left-tailed
samples_100$pvalue_H0false <- 1-pt(samples_100$test_stat_H0false, df=100-1) # right-tailed
head(samples_100[,104:106])

# Type I Error: Rejecting a true H0
# Type II Error: Accepting a false H0

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

boxplot(samples_100[,105:106])

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

# H0: pop_mean = 12 (mean of educ years is not significantly diff from 12)
# H1: pop_mean > 12 (mean of educ years is significantly greater than 12)

# Step 2 and 3
t.test(ESS$Education_Years, mu = 12, alternative = "greater")

# p-value = 0.7% --> < 0.01=1% --> rejecting the H0 on all common signif levels
# --> H1: mean of educ years is significantly greater than 12

# n = 1830 ---> large sample size --> no need to check normal dist
# sample_mean = 12.19

# sample size increases --> smaller differences between sample and hypothetical means
# can be considered as significant

#-------------------------------------------------------------------------------


ESS <- read_excel("ESS2020.xlsx")
EducationData <- as.data.frame(ESS$Education_Years[!is.na(ESS$Education_Years)])
colnames(EducationData) <- "EducationYears"
rownames(EducationData) <- paste0("Observation", 1:nrow(EducationData))

set.seed(17)
samples <- sample(x = EducationData$EducationYears, size = 100, replace = TRUE)
for (i in 1:9999) {
  set.seed(17+i)
  samples <- rbind(samples, sample(x = EducationData$EducationYears, size = 100, replace = TRUE))
}

samples <- as.data.frame(samples)
colnames(samples) <- paste0("Observation", 1:100)
rownames(samples) <- paste0("Sample", 1:nrow(samples))

# Statement: average Hungarian in the population is spending more years in education than
#            12 years (primary+high school)

# H0: pop_mean = 12 (mean of educ years is not significantly diff from 12)
# H1: pop_mean > 12 (mean of educ years is significantly greater than 12)

n <- ncol(samples)
PopMean <- mean(EducationData$EducationYears)

sample_17 <- as.numeric(samples[17,])
sample_17_mean <- mean(sample_17)

# Step 2 --> Calculating the test statistic
# (sample_mean - theoretical_mean)/SE_mean
# SE_mean = corr_sd/sqrt(n)

samples$means <- apply(samples,1,mean)
samples$corr_sd <- apply(samples[,1:100],1,sd)

samples$test_stat_H0_true <- (samples$means - 12)/(samples$corr_sd/sqrt(n))

#calculate p value
samples$pvalue_H0_true <- 1 - pt(samples$test_stat_H0_true, df = n-1)
sample_17_p_value <- samples$pvalue_H0_true[17]

t.test(sample_17, mu = 12, alternative = "greater")


#-------------------------------------------------------------------------------

t.test(ESS$Education_Years, mu = 12, alternative = "greater")


# így kell mert ez már sokaság
# Compute mean and SD from full dataset
sample_mean <- mean(EducationData$EducationYears)
sample_sd <- sd(EducationData$EducationYears)
n <- nrow(EducationData)

# Test stat and p-value
test_stat <- (sample_mean - 12) / (sample_sd / sqrt(n))
p_value <- 1 - pt(test_stat, df = n - 1)

# Check results
cat("Test stat:", test_stat, "\n")
cat("P-value:", p_value, "\n")

#------------------------------------------------------------------------------

ESS <- readxl::read_excel("ESS2020.xlsx")

# HYPOTHESIS TESTING

# One-Sample Parametric Tests
# -> test for the population mean (t-test): Last Week
# -> test for the population proportion
# -> test for the population standard deviation

# 1) Proportion

# Statement: In the total Hungarian population the support for the governing
# Fidesz-KDNP party is lower than 20%

# P: proportion in the population

# Statement: P < 0.2

# 1) Formulating H0 and H1

# H0T: P = 0.2 (thinking: P >= 0.2)
# H1: P < 0.2
# Statement: H1

P_0 <- 0.2
# 2) Calculation the Test Statistic

# Test Stat = (p - P_0)/SE_P
# SE_P <- SE_P = sqrt(P_0*(1-P_0)/n)
# p: observed proportion in the sample

# proportion is just the mean of a 0/1 Bernoulli variable
# --> doesn't work here: SE_P contains P_0 instead of p

p <- mean(ESS$PoliticalPartyPref=="Fidesz-KDNP")
n <- nrow(ESS)
test_stat_p <- (p-P_0)/sqrt(P_0*(1-P_0)/n)

# test stat is very likely to occur in N(0,1) distribution -->
# --> N(0,1) is the distr of H0 --> H0 is very likely to be true

# Calculate p-value = Empirical Prob of Type I Error
# --> How likely to make an error if we reject H0
# (Type I Error = rejecting a true H0)

# p-value = 100% when it's most favorable for H0
# then I need to reduce it if we are further from this state

# H0T: P = 0.2 (thinking: P >= 0.2) --> most fav case for H0 is test stat = +Inf
# H1: P < 0.2

# Conclusion: p-value = P(N(0,1) < TestStat)
pnorm(test_stat_p) # 0.39

# 4) Decision

# signif level = alpha = allowed Type I Error Prob

# p-value < 0.01 --> reject H0 (lower than the smallest common signif level)
# 0.01 <= p-vale <= 0.1 --> safest not to decide --> increase sample size
# p-value > 0.1 --> fail to reject H0 (higher than the largest common signif level)

# --> fail to reject H0 --> P >= 0.2 --> 19.7% is NOT significantly diff from 20%


# 3/b) --> Two sided test

# H0: P = 0.2 --> most fav case for H0 is test stat = 0
# H1: P !=  0.2

# general rule --> distribution is symmetric to 0 (t(n-1) or N(0,1))
pnorm(-abs(test_stat_p))*2
pnorm(-abs(test_stat_p)) + (1-pnorm(abs(test_stat_p)))

# 4/b)

# p-value = 78% --> even higher Type I error prob than one-sided case -->
# --> fail to reject H0

# logical: test stat is closer to 0 (most fav case for H0 two-sided) than to
#          +Inf (most fav cas for H0 one-sided)

# CHECK if CLT works --> is the sample size large enough to use N(0,1) for p-value calc
n*P_0 >=10 & n*(1-P_0) >= 10 # TRUE --> sample size is large enough

# 2) Standard Deviation

# Statement: In the unobserved Hungarian population st dev of weekly work hours is
#            at most 15 hours

# Sigma: population standard deviation

# Statement: Sigma <= 15

# 1) formulating H0 and H1

# H0T: Sigma = 15 (thinking: Sigma <= 15)
# H1: Sigma > 15
# Statement: H0 --> rooting for this to be true

# 2) Calc Test Stat

# Sigma_0: theoretical value of the standard deviation

Sigma_0 <- 15

WorkHours_NonMissing <- ESS$WeeklyWork_Hours[!is.na(ESS$WeeklyWork_Hours)]
n <- length(WorkHours_NonMissing)
s <- sd(WorkHours_NonMissing)

test_stat_sd <- (n-1)*s^2/Sigma_0^2

# let's see this Chi-Squared(n-1) distribution by drawing its density function
# Median[Chi-Squared(k)] ~ k
x_axis <- seq(0,2*n,0.01)
y_axis <- dchisq(x_axis, df=(n-1))
plot(x_axis, y_axis, type = "l",
     xlab = "Test Statistics from Lots of Samples if H0 TRUE",
     ylab = "Probability")
# see where my test stat is in this distribution
abline(v = test_stat_sd, col="red")


# H0 seems to be true as our specific test stat seems to be occurring with a high
# probability if H0 is TRUE

# p-value = 100% when it's most favorable for H0
# then I need to reduce it if we are further from this state

# H0T: Sigma = 15 (thinking: Sigma <= 15)
# H1: Sigma > 15

# test_stat_sd <- (n-1)*s^2/Sigma_0^2 --> best case for H0 test stat = 0
# decrease the p-value if the test stat is further and further from 0

# P(Chi2 > 0) = 100%
# P(Chi2 > TestStat) = p-value
1-pchisq(test_stat_sd, df=(n-1)) # 17.4%

# 4) Decision

# p-value = 17.4% --> fail to reject the H0 on any common signif levels because
#                     p-value > largest common signif level of 10%
# --> Statement is TRUE --> st dev of weekly work hours is NOT significantly greater
#                           than 15 hours

# 3/b) Two Sided Option

# H0: Sigma = 15
# H1: Sigma != 15

# p-value = 100% when it's most favorable for H0
# then I need to reduce it if we are further from this state

# most fav test stat value for H0 = peak of the density curve = (n-1) = 685-1
# (n-1)*s^2/Sigma_0^2 ~H0~ (n-1)*1
abline(v=(n-1), lty="dotted")

# p-value = 100% when test stat = (n-1) AND decrease it if we are further away
# on any side
(1-pchisq(test_stat_sd, df=(n-1)))*2 # 34.7%

# What is the p-value formula if test stat < (n-1)?
#összefoglalva:
#         if test stat < (n-1) --» (pchisq(test_stat_sd, df=(n-1)))
#         if test stat > (n-1) --» (1-pchisq(test_stat_sd, df=(n-1)))
#         if test stat = (n-1) --» (pchisq(test_stat_sd, df=(n-1)))*2


#-------------------------------------------------------------------------------

# Two-Sample Parametric Tests

# One-Sample cases: statements were about
# how a stat parameter relates to a theoretical value

# Two-Sample cases: statement are about comparing stat parameters
# of two groups (two sub-populations)

# Are males significantly earn more than females?
# --> parameter: pop mean
# --> 2 groups: male + female

# Do people who are believing in conspiracy theories have a significantly higher
# proportion of those who do not trust the work of parliament?
# --> parameter: pop proportion of parliament work "trusters"
# --> 2 groups: believe or not believe consp theories

# Are males significantly earn more than females?
# --> parameter: pop mean
# --> 2 groups: male + female

# Statement: Pop_Mean_M > Pop_Mean_F
# Trick: Pop_Mean_M - Pop_Mean_F > 0

# H0T: Pop_Mean_M - Pop_Mean_F = 0 (thinking: Pop_Mean_M - Pop_Mean_F <= 0)
# H1: Pop_Mean_M - Pop_Mean_F > 0

# On average males earn 100$ more than females

# Statement: P_Mean_M > Pop_Mean_F + 100 

# Trick: Pop_Mean_M - Pop_Mean_F > 100

# H0T: Pop_Mean_M - Pop_Mean_F = 100 (thinking: Pop_Mean_M - Pop_Mean_F <= 100)
# H1: Pop_Mean_M - Pop_Mean_F > 100

# delta_0 = theoretical difference between the parameters (means, proportions, etc)

# Specifics using the ESS data

# In the Hungarian population those who do NOT believe that scientists deceive the public
# are more educated on average

# Statement: Pop_Mean_NB > Pop_Mean_B


# Statement: Pop_Mean_NB > Pop_Mean_B

# Pop_Mean_NB - Pop_Mean_B > 0

# Step #1

# H0T: Pop_Mean_NB - Pop_Mean_B = 0 (thinking: Pop_Mean_NB - Pop_Mean_B <= 0)
# H1: Pop_Mean_NB - Pop_Mean_B > 0
# Statement: H1

delta_0 <- 0

# Step 2:
# calculate test statistic
aggregate(Education_Years ~ ScientistsDecievePublic, data = ESS,
          FUN = mean)

# test_stat = (observed_diff - delta_0)/SE_Unified
# SE_Unified = sqrt(SE_1^2 + SE_2^2)

observed_diff <- mean(ESS$Education_Years[ESS$ScientistsDecievePublic == "No"],
                      na.rm = TRUE) - 
  mean(ESS$Education_Years[ESS$ScientistsDecievePublic == "Yes"],
       na.rm = TRUE)


# SE for the mean = corr st dev / sqrt(n)
sd_NB <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"],
            na.rm = TRUE)
sd_B <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"],
           na.rm = TRUE)

n_NB <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"])) 
n_B <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"]))

SE_Unified <- sqrt(sd_NB^2/n_NB + sd_B^2/n_B)

test_stat2_sample_mean <- (observed_diff - delta_0) / SE_Unified

p_value <- 1-pt(test_stat2_sample_mean, df = n_NB + n_B)
# 0.075 --»inconclusive



# Two-Sample Test for Proportions

# the proportion of those who trust in parliament is at most 2 percentage-points
# higher for those who believe a secret group influences world politics

# Paramenets: Prop of trust in Parliament
# 2 Groups: Believe vs NonBelieve

# Statement: P_B <= P_NB + 0.02

# P_B - P_NB <= 0.02

# Step #1

# H0T: P_B - P_NB = 0.02 (thinking: P_B - P_NB <= 0.02)
# H1: P_B - P_NB > 0.02
# Statement: H0

# Step #2 Sample Data --> Test Stat

n_B <- sum(ESS$SecretGroupInfluenceWorldPol == "Yes")
n_NB <- sum(ESS$SecretGroupInfluenceWorldPol == "No")

# k_i --> number of favorable cases in the proportion for group 'i'

k_B <- sum(ESS$SecretGroupInfluenceWorldPol=="Yes" &
             ESS$TrustInParlament=="Yes")
k_NB <- sum(ESS$SecretGroupInfluenceWorldPol=="No" &
              ESS$TrustInParlament=="Yes")

p_B <- k_B/n_B # 11.1%
p_NB <- k_NB/n_NB # 14.6%

delta_0 <- 0.02
observed_diff_p <- p_B - p_NB


# test_stat = (observed_diff_p - delta_0)/SE_Unified_p
# SE_Unified_p = sqrt(SE_1^2 + SE_2^2)
# SE_p = sqrt(p*(1-p)/n)

test_stat_2sample_p <- (observed_diff_p - delta_0)/sqrt(
  p_B*(1-p_B)/n_B + p_NB*(1-p_NB)/n_NB
)

p_value_prop <- 1-pnorm(test_stat_2sample_p)
# 99.9%

# Step #4 Decision

# p-value of 99.9% --> of course, fail to reject H0

# difference between sample proportions is not significantly larger than 2%-points






