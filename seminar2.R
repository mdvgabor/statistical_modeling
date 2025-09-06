setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

Tesla <- readxl::read_excel("TSLA.xlsx")

# histogram of price changes --> ggplot
library(ggplot2)

ggplot(data = Tesla, mapping = aes(x = TESLA)) +
  geom_histogram(aes(y = after_stat(density))) # y axis shows "relative freq of a
#                                                single value in the bin"
# mild left tail, but mostly symmetric
# kurtosis: pointy

# let's try to model this data with a normal distribution

mu_T <- mean(Tesla$TESLA)
sigma_T <- sd(Tesla$TESLA)

# i-th price change of Tesla = T_i
# T_i ~ N(1.78, 27)

# P(T_i = 30) = f(30)
dnorm(x = 30, mean = mu_T, sd = sigma_T)
# 0.85%

# P(T_i < -30) --> cumulative distribution function
pnorm(-30, mean = mu_T, sd = sigma_T)
# 12%

# P(T_i > +20)
1 - pnorm(20, mean = mu_T, sd = sigma_T)
# 25%

# P(-40 < T_i < -20)
pnorm(-20, mean = mu_T, sd = sigma_T) -
  pnorm(-40, mean = mu_T, sd = sigma_T)
# 14%

# What is the value that a bigger loss occurring has only 5%
# probability
# P(T_i < x) = 0.05 --> x=?
# quantile function
qnorm(0.05, mean = mu_T, sd = sigma_T)
# -42.9$
# Value at Risk 95%
# we have a 95% probability that we DON'T have bigger loss than this number

# FOR EVERY DISTRIBUTION:
# d prefix --> density function
# p prefix --> cumulative distr function
# q prefix --> quantile function

# Calculate the following things WITHOUT using the normal distribution

# P(-40 < T_i < -20)
Tesla_Filter <- Tesla[Tesla$TESLA < -20 & Tesla$TESLA > -40,]
nrow(Tesla_Filter)/nrow(Tesla)
# 3.6%
# vs Normal: 14%

# Value at Risk 95%
quantile(Tesla$TESLA, 0.05)
# -28.5$
# vs Normal: -42.9$

# let's plot the density function on this histogram
ggplot(data = Tesla, mapping = aes(x = TESLA)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm,
                args = list(mean = mu_T, sd = sigma_T),
                col = "pink")

# transform T_i ~ N(1.78,27) to z_i ~ N(0,1)

Tesla$z <- (Tesla$TESLA - mu_T)/sigma_T
mean(Tesla$z)
sd(Tesla$z)

# Optional Homework: see the same function for exponential distribution (lecture notes)

#-------------------------------------------------------------------------------

# Simulate/Generate data that behave like a specific prob distribution

# Basic tool: random numbers between 0 and 1

# random numbers from Uniform(0,1)
unlucky_numbers <- runif(n=50) # results are in a vector

# check if they are truly uniform --> histogram
unlucky_df <- as.data.frame(unlucky_numbers)

ggplot(data = unlucky_df, mapping = aes(x=unlucky_numbers)) +
  geom_histogram(bins = 6)
# expect freq for every bin: 50/6 = 8.3
50/6

# Transform these U(0,1) numbers to U(40,160)
unlucky_40_160 <- unlucky_numbers * (160-40) + 40
unlucky_df$unlucky_40_160 <- unlucky_40_160

# Histogram for the U(40,160)
ggplot(data = unlucky_df, mapping = aes(x=unlucky_40_160)) +
  geom_histogram(bins = 6)

# histogram for the U(40,160) with densities on y axis
ggplot(data = unlucky_df, mapping = aes(x=unlucky_40_160)) +
  geom_histogram(bins = 6, aes(y = after_stat(density))) +
  geom_hline(yintercept = 1/(160-40), color="red")

# density function for U(40,160) = 1/(160-40) = 0.0083
1/(160-40)

# histogram with densities on the y axis for U(0,1)
ggplot(data = unlucky_df, mapping = aes(x=unlucky_numbers)) +
  geom_histogram(bins = 6, aes(y = after_stat(density)))

# density function of U(0,1) = 1/(1-0) = 1


# Generate 50 random numbers with Exp(0.0125)

happy_exp_data <- qexp(unlucky_numbers, rate = 0.0125)
unlucky_df$exp <- happy_exp_data

ggplot(data = unlucky_df, mapping = aes(x=exp)) +
  geom_histogram(bins = 6, aes(y = after_stat(density))) +
  stat_function(fun = dexp,
                args = list(rate=0.0125),
                col="orange")

# prob distributions have r prefix function = random number gen

# VISION & MISSION --> X_i ~ N(80, 20) or Y_i ~ Exp(0.0125)

unlucky_df$norm <- rnorm(n = 50, mean = 80, sd = 20)

ggplot(data = unlucky_df, mapping = aes(x=norm)) +
  geom_histogram(bins = 6, aes(y = after_stat(density)))

#rexp()
#rpois()

set.seed(17)
rexp(n=5, rate = 0.0125)

# Exponential(0.0125)

th_mean <- 1/0.0125
th_sd <- 1/0.0125
th_median <- qexp(0.5, rate = 0.0125)

set.seed(17)
common_exp_data <- rexp(n=50, rate = 0.0125)

empirical_mean <- mean(common_exp_data)
empirical_sd <- sd(common_exp_data)
empirical_median <- median(common_exp_data)

# differences in the statistical measures empirical and theoretical versions
# --> SAMPLING ERROR

# generated data from a distribution --> IID Sample from a distribution
# IID = Independent and Identically Distributed
# Identically Distributed = sample elements (observations) are coming from the SAME dist
# Independent: sample observations are random --> independent from each other

# Sampling Error decreases as the sample size increases --> Glivenco-Cantelli Theorem

set.seed(17)
common_exp_data <- rexp(n=50000, rate = 0.0125)

empirical_mean <- mean(common_exp_data)
empirical_sd <- sd(common_exp_data)
empirical_median <- median(common_exp_data)

#------------------------------------------------------------------------------------

# Central Limit Theorem = CLT --> Simulate how this works

# Let's assume: X_1, X_2, ..., X_n random variables that are
#                                  independent and identically distributed
# E(X_i) = mu and SD(X_i) = sigma

# SUM(X_1, X_2, ..., X_n) ~ N(n*mu, sqrt(n)*sigma)

# 1st Simulation: X_i ~ Exp(0.1) and n=10

X_1_n <- rexp(n=10, rate=0.1)

# sum the values and repeat it lots of times
# "lots of times" = 10000 (replication count)

sapply # assumes the input is a vector and gives the result as a vector too

# sum of every integer from 1 to 100, 10 times

sapply(1:10, function(x) sum(1:100))

# use the sapply for the CLT

CLT_n10 <- sapply(1:10000, function(x) sum(rexp(n=10, rate = 0.1)))
hist(CLT_n10)
# E(X_i) = 1/0.1 = 10 = SD(X_i)
# N(10*(10), sqrt(10)*10)
mean(CLT_n10)
sd(CLT_n10)

sqrt(10)*10 # 31.6

CLT_n500 <- sapply(1:10000, function(x) sum(rexp(n=500, rate = 0.1)))
hist(CLT_n500)

#--------------------------------------------------------------------------------

# IID Sampling in Practice --> from a Population

# Swim across lake Balaton

swim_population <- readxl::read_excel("LIDLBalaton2022.xlsx")
# N = 9751

# take an IID sample of n=100 elements
# Independent --> Selection of sample elements must be random: P(Sample) = 1/N
# Identically Distributed --> Sampling MUST NOT CHANGE THE DISTR OF POP DATA
#                             WITH REPLACEMENT!!

# without replacement not very different than with replacement
# sampling makes sense: n << N
# in these selection ratio (n/N) is small --> very small chance of repetition to occur

# random sample with replacement from the population = IID sample from the pop.

set.seed(17)
rownumbers_sample <- sample(rownames(swim_population), size = 100, replace = TRUE)
swimmer_sample <- swim_population[rownumbers_sample,]

# Sampling Error: statistical measures of the population differ from
#                 the same stat measures calculated from the sample

# statistical measures of the population = statistical parameters
# statistical measures of the sample     = estimators

# Sampling Error for the Mean
PopMean <- mean(swim_population$TIME) # parameter
SampleMean <- mean(swimmer_sample$TIME) # estimator

PopMean - SampleMean # Sampling Error

# Sampling Error for the Median
PopMedian <- median(swim_population$TIME)
SampleMedian <- median(swimmer_sample$TIME)
PopMedian - SampleMedian

# VISION & MISSION: Calculate these Sampling Errors from one sample,
#                   WITHOUT knowing the population

# Step #1: get lots of IID samples of the same size from the population
#          Resampling
#          lots of times = 10000; n = 100

# Result: df[sample, observation] = df[10000, 100]

# 1st sample
samples <- swimmer_sample$TIME

# Rest of the samples (remaining 9999)
for (i in 1:9999) {
  set.seed(17+i)
  samples <- rbind(samples,sample(swim_population$TIME, size = 100, replace = TRUE))
}

samples <- as.data.frame(samples)

rownames(samples) <- paste0("Sample",1:10000)
colnames(samples) <- paste0("Observation", 1:100)

head(samples)

# save the results to Excel
writexl::write_xlsx(samples, "swim_samples.xlsx")

# Extra HW #1: Do simulations on the Poisson Limit Theorem (+1p)
# Extra HW #2: Write R functions to simulate Glivenco-Cantelli Theorem
#              lognormal and weibull distributions (+2p)
