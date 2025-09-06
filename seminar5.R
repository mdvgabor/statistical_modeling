setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
ESS <- readxl::read_excel("ESS2020.xlsx")

# Random Sample from the whole Hungarian Population
# n = 1849

# Revisit: confidence interval for the mean internet usage time with 97% confidence

# interval: sample_mean +- SE*k
# SE*k = margin of estimation error = delta (triangle)
# SE: average difference between the sample and population means
# k: set the desired level of confidence

# specifics: assume that our sample is IID

# SE = corrected_sample_st_dev/sqrt(n)
# k = qnorm(1-alpha/2) or qt(1-alpha/2, df=(n-1)) --> alpha = 1-conf_level
# large sample size (n >= 50) --> it does not matter

# Data "collect"
sample_mean <- mean(ESS$NetUsePerDay_Minutes, na.rm=TRUE)
s <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE) # correct st dev
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
alpha <- 1-0.97

# Calc "ingredients" for the CI
SE <- s/sqrt(n)
k <- qt(1-alpha/2, df=(n-1))

# Conf Interval itself
c(sample_mean-SE*k, sample_mean+SE*k)
# In the unobserevd Hungarian population the average internet usage time per day
# is between 172 and 191 minutes with prob of 97%

install.packages(c('DescTools', 'multcompView', 'plyr', 'coin', 'lmtest', 'nortest'))
install.packages("C:/Users/lkovacs/Downloads/rcompanion_2.4.30.tar.gz",
                 repos = NULL, type = "source")

# automated CI calculation
library(rcompanion)

groupwiseMean(NetUsePerDay_Minutes ~ 1, data = ESS, na.rm = TRUE, conf = 0.97,
              digits = 5)

# let's calculate the same confidence interval but for each political party preference
internetuse_party <- groupwiseMean(NetUsePerDay_Minutes ~ PoliticalPartyPref,
                                   data = ESS,
                                   na.rm = TRUE, conf = 0.97, digits = 5)
internetuse_party

# visualization with ggplot
library(ggplot2)

ggplot(data = internetuse_party, aes(x=PoliticalPartyPref, y=Mean,
                                     fill=PoliticalPartyPref)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper))

# used t-distr --> n<50 assumes that our variable is normally distr
hist(ESS$NetUsePerDay_Minutes)
# lomg right tail --> no way jose normal --> in the other group
# even the t-dist interval is unreliabe!! --> not neccesarily 97% accurate

#----------------------------------------------------------------------------

# let's estimate the proportion of Fidesz supporters in the whole population
# with 99% confidence

# conf interval for the mean of a variable where Fidesz=1 and EElse=0
# --> Bernoulli(p) --> p = proportion of Fidesz supporters
# Bernoulli expected value = p

ESS$Fidesz <- ifelse(ESS$PoliticalPartyPref=="Fidesz-KDNP",1,0)

groupwiseMean(Fidesz ~ 1, data = ESS, na.rm = TRUE, conf = 0.99,
              digits = 5)
# prop of Fidesz supporters in the sample = 19.7%
# pop of supporters in the whole Hungarian pop 17%-22% with 99% prob

# Number of Fidesz supporters in the whole Hung pop?
# answer: need N (population size)
N <- 7500000 # assumption on the voting population
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

# n = (sqrt(p*(1-p))*2.57/0.01)^2 <= (sqrt(0.25)*2.57/0.01)^2 = 16 513 people
(sqrt(0.25)*2.57/0.01)^2

# roughly 16 thousand people are needed if 1 have an opinion poll where
# we have 2 options and the race is tough --> p ~ 0.5
# Brexit vote

# original margin of error = 2.4 %-points = 0.398/sqrt(1849)*2.57
0.398/sqrt(1849)*2.57

# in the whole sample
# VS in the sample that can choose --> ignore the unknowns
table(ESS$PoliticalPartyPref)
# sample size = 1849 - 1189 = 660
# let's assume that the st deviation won't change just the sample size
# margin of error for the "sure voters/supporters"
0.398/sqrt(660)*2.57 # 4 %-points

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
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
s <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE)
alpha <- 1-0.97

# Conf Interval
c(sqrt((n-1)*s^2/qchisq(1-alpha/2,df=n-1)),
  sqrt((n-1)*s^2/qchisq(alpha/2,df=n-1)))
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

# Data "collect"
sample_mean <- mean(ESS$NetUsePerDay_Minutes, na.rm=TRUE)
s <- sd(ESS$NetUsePerDay_Minutes, na.rm = TRUE) # correct st dev
n <- sum(!is.na(ESS$NetUsePerDay_Minutes))
alpha <- 1-0.97
N <- 9700000

# Calc "ingredients" for the CI
SE <- s/sqrt(n)*sqrt(1-n/N)
k <- qt(1-alpha/2, df=(n-1))

# Conf Interval itself
c(sample_mean-SE*k, sample_mean+SE*k)
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

# Calc "ingredients" for the CI
SE <- s/sqrt(n)*sqrt(1-n/N)
k <- qt(1-alpha/2, df=(n-1))

# Conf Interval itself
c(sample_mean-SE*k, sample_mean+SE*k)
# average Hungarian Household yearly income is between 4.58 and 4.71 million HUF
# with a 95% prob

# PS Sampling --> SE = s_w/sqrt(n)*sqrt(1-n/N)

# s_w --> within-stratum (corrected) st deviation

# s_w <= s

# table: rows = strata; columns: {sample size, mean, sd}

helper_table <- aggregate(Income ~ Settlement, data = HH, FUN = mean)
helper_table$sd <- aggregate(Income ~ Settlement, data = HH, FUN = sd)[,2]
helper_table$sample_size <- table(HH$Settlement)
helper_table

s_w <- sqrt(sum(helper_table$sd^2*(helper_table$sample_size-1))/(n-1))

SE_PS <- s_w/sqrt(n)*sqrt(1-n/N)

# conf interval for PS sample
c(sample_mean-SE_PS*k, sample_mean+SE_PS*k)
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

# SE_IID >= SE_SR >= SE_PS