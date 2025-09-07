# Read Balaton swim data and the 10000 IID samples generated from it
setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

library(readxl)

swim_population <- read_excel("LIDLBalaton2022.xlsx")
swim_samples <- read_excel("swim_samples.xlsx")
swim_samples <- as.data.frame(swim_samples)
rownames(swim_samples) <- paste0("Sample", 1:nrow(swim_samples))

head(swim_samples)

sample_5 <- as.numeric(swim_samples[5,])

#--------------------------------------------------------------------------

# Parameters: statistical measures computed from population data
PopMean <- mean(swim_population$TIME)

class_var <- function(x){ # assume: x is a numeric vector
  pop_variance <- mean((x - mean(x))^2)
  return(pop_variance)
} 

PopVariance <- class_var(swim_population$TIME)

PopProportion <- sum(swim_population$TIME>180)/
  nrow(swim_population)# swimmwers who took longer than 3 hours = 180 minutes


# Estimators: statistical measures computed from sample data

mean(sample_5)
class_var(sample_5)
sum(sample_5>180)/length(sample_5)

# Sampling Error = Estimator - Parameter

# Vision & Mission --> approximate Sampling Error from data of just one sample

# Deriving the approx for Sampling Error: "God Mode" --> lots of samples of the same size

# 1st Calculate every estimator in every sample

swim_samples$sample_means <- apply(swim_samples, 1, mean)
swim_samples$sample_vars <- apply(swim_samples[,1:100], 1, class_var)
swim_samples$sample_props <- apply(swim_samples[,1:100], 1,
                                   function(x) sum(x>180)/length(x))

head(swim_samples[,101:103])

# 2nd expected value (mean) of every estimator
mean(swim_samples$sample_means) # ~ 167.5 = Pop Mean
mean(swim_samples$sample_vars) # ~ 1920.6 < Pop Variance
mean(swim_samples$sample_props) # ~ 0.33 = Pop Proportion (Proportion is mean of
#                                                          a Bernoulli distribution)

# BIAS = E(Estimator) - Parameter
# sample means and props are UNBIASED ESTIMATORS --> Bias = 0 [every possible IID
#                                                              sample of size 100]

# sample variance is BIASED ESTIMATOR --> Bias < 0

# tied to the estimator

# 2/a) Heal the variance --> make it unbiased

# Bessel's correction

# E(sample variances) = population variance * (n-1)/n
mean(swim_samples$sample_vars)/PopVariance # ~ 0.99 = 99/100

# E(sample variances * n/(n-1)) = pop variance

swim_samples$corrected_vars <- swim_samples$sample_vars*100/99

mean(swim_samples$corrected_vars) # ~ 1940 ~ PopVariance [we need every possible sample
#                                                         to have an exact match]

swim_samples$corr_vars_v2 <- apply(swim_samples[,1:100], 1, var)
mean(swim_samples$corr_vars_v2)

# 3rd step --> standard deviation of the estimators in lots of samples

sqrt(class_var(swim_samples$sample_means)) # 4.4 minutes
sqrt(class_var(swim_samples$sample_props)) # 4.7 %-points

# If estimator is Unbiased --> these st deviations are the expected differences
# between one sample's estimator and the true parameter value = standard errors

# standard errors with the simplified formulas

# mean
sqrt(PopVariance)/sqrt(100) # 4.4 minutes

# proportions
sqrt(PopProportion*(1-PopProportion)/100) # 4.7 %-points

# apply simplified standard error formulas on 1 sample!!!

# mean
sqrt(var(sample_5)/100) # SE approximated as 4.19 minutes

mean(sample_5)
# Population Mean: 167.17 +- 4.19 minutes


# proportion
p_sample_5 <- sum(sample_5>180)/length(sample_5)
sqrt(p_sample_5*(1-p_sample_5)/100) # 4.7 %-points
# Population Proportion: 33% +- 4.7 %-points

# Sampling Error for Unbiased Estimators = Standard Error = SQRT(Var(Estimator))

#-----------------------------------------------------------------------------

# Sampling Error for Biased Estimators = Mean Squared Error (MSE)

# E[(Estimators - Parameter)^2] = MSE

# MSE(Uncorrected Variance)
MSE_Uncorr_Var <- mean((swim_samples$sample_vars - PopVariance)^2)

# MSE = SE^2 + Bs^2
SE_Uncorr_Var = sqrt(class_var(swim_samples$sample_vars))
Bs_Uncorr_Var = mean(swim_samples$sample_vars) - PopVariance
MSE_Uncorr_Var_v2 <- SE_Uncorr_Var^2 + Bs_Uncorr_Var^2


# MSE(Corrected Variance) = SE^2(Corrected Variance)
MSE_Corr_Var <- class_var(swim_samples$corrected_vars)


# MSE(Proportions) = SE^2 + Bs^2 = 0.047^2 + 0

# If we have two estimators of the same thing (both corrected and uncorrected variance
# are estimators for the true variance), then the one with smaller MSE is
# more EFFICIENT

# CONSISTENT ESTIMATOR: if n --> oo, then SE(Estimator) --> 0
# MEAN + PROPORTION ARE ALWAYS CONSISTENT

#---------------------------------------------------------------------------------------

# So far: IID Sample Data --> Estimators --> Population Parameters
# Now:    IID Sample Data --> Estimators --> Distribution Parameters

# Methods:
# 1) Method of Moments
# 2) Maximum Likelihood Method

surv <- read_excel("CancerSurvival.xlsx") # Survival times after cancer in months

num_claims <- read_excel("insurance_examples.xlsx",
                         sheet = "CarInsurance_NumberOfClaims")
# how many accidents (claim) the insured car caused in a year

# Frequency distribution
hist(surv$SurvMonth) # cont with long right tail --> Exponential(lambda)

barplot(table(num_claims$NumClaims)) # discrete long right tail --> Poi(lambda)

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

observed_freq <- table(num_claims$NumClaims)
observed_freq

expected_freq <- round(dpois(x=0:4, lambda = lambda_poi)*400,0)
names(expected_freq) <- 0:4
expected_freq

# New Data
car_fleet <- read_excel("insurance_examples.xlsx", sheet = "CarInsurance_Fleet")
# 1 row = 1 fleet of 'm' cars
# 2nd variable = number of cars that have caused an accident in a year

# freq distribution
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
dexp(x = 10.42, rate = 0.02)

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
ll_exp <- function(lambda){
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