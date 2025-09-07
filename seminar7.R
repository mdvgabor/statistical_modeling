setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

ESS <- readxl::read_excel("ESS2020.xlsx")
# European Social Survey Data --> Random Sample from the Hungarian Population (n=1849)

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

# P_0: theoretical proportion
P_0 <- 0.2

# 2) Calculation the Test Statistic

# Test Stat = (p - P_0)/SE_P
# SE_P = sqrt(P_0*(1-P_0)/n)

# p: observed proportion in the sample

# for the mean: test stat = (sample_mean - theoretical_mean)/SE_mean
# SE_mean = corr_sample_st_dev/sqrt(n)
# calculation: t.test function

# proportion is just the mean of a 0/1 Bernoulli variable
# --> doesn't work here: SE_P contains P_0 instead of p

p <- mean(ESS$PoliticalPartyPref=="Fidesz-KDNP")
n <- nrow(ESS)

test_stat_p <- (p-P_0)/sqrt(P_0*(1-P_0)/n)
# -0.28
# H0 seems to be true as test stat is close to 0

# 3) Calculate a p-value
# need: probability distribution -->
# --> distribution of the test stat from lots of samples IF H0 IS TRUE

# Test Stat = (p - P_0)/SE_P ~ N(0,1) (if H0 is true--> P=P_0: CLT if sample size is large)
# SE_P = sqrt(P_0*(1-P_0)/n)

# let's see this N(0,1) distribution by drawing its density function
x_axis <- seq(-5,5,0.01) # gives every number between +-5 with 0.01 steps
y_axis <- dnorm(x_axis)
plot(x_axis, y_axis, type = "l",
     xlab = "Test Statistics from Lots of Samples if H0 TRUE",
     ylab = "Probability")
# see where my test stat is in this distribution
abline(v = test_stat_p, col="red")

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
pnorm(test_stat_p) # 39%

# 4) Decision

# signif level = alpha = allowed Type I Error Prob

# p-value < 0.01 --> reject H0 (lower than the smallest common signif level)
# 0.01 <= p-vale <= 0.1 --> safest not to decide --> increase sample size
# p-value > 0.1 --> fail to reject H0 (higher than the largest common signif level)

# --> fail to reject H0 --> P >= 0.2 --> 19.7% is NOT significantly diff from 20%

# 3/b) --> Two sided test

# H0: P = 0.2 --> most fav case for H0 is test stat = 0
# H1: P !=  0.2

# p-value = 100% when it's most favorable for H0
# then I need to reduce it if we are further from this state

# most fav case when test stat = 0 --> p-value = 100%
pnorm(0)*2
pnorm(0) + (1-pnorm(0))

# general rule --> distribution is symmetric to 0 (t(n-1) or N(0,1))
pnorm(-abs(test_stat_p))*2
pnorm(-abs(test_stat_p)) + (1-pnorm(abs(test_stat_p)))
# 78%

abline(v = -test_stat_p, col="blue")

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

# 3) Calc p-value

# Test Stat ~ Chi-Squared(n-1) if H0 is true (if Sigma=Sigma_0) + 
#                              if data is normal

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


# What if test stat = 550
abline(v=550, col="blue")
(pchisq(test_stat_sd, df=(n-1)))*2

# 4/b)
# Same as in the original because p-value = 34.7% > 10% (highest common signif level)

# Test if the data is normal because it was assumed when applying the chi-squared dist
hist(WorkHours_NonMissing) # meh: mostly symmetric but big outliers on the upper end

# 150 is stupid, so let's see the histogram without them
hist(WorkHours_NonMissing[WorkHours_NonMissing < 100])
# more symmetric but very peaked (high excess kurtosis)

# assumption of normality doesn't seem to hold

#--------------------------------------------------------------------------------

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

# Pop_Mean_NB - Pop_Mean_B > 0

# Step #1

# H0T: Pop_Mean_NB - Pop_Mean_B = 0 (thinking: Pop_Mean_NB - Pop_Mean_B <= 0)
# H1: Pop_Mean_NB - Pop_Mean_B > 0
# Statement: H1

delta_0 <- 0

# Step #2: Test Statistic

# let's check the two group means in the sample
aggregate(Education_Years ~ ScientistsDecievePublic, data = ESS,
          FUN = mean)
# H1 seems ok: sample mean NB > sample mean B
# observed_difference > 0
# this difference can come from sampling error alone --> need the p-value!!

# test_stat = (observed_diff - delta_0)/SE_Unified
# SE_Unified = sqrt(SE_1^2 + SE_2^2)

# Var(X+Y) = Var(X) + Var(Y)
# in this case: X=sample mean of group 1 + Y=sample mean of group 2
# ASSUMPTION: Group 1 is INDEPENDENT of Group 2 --> 2*Cov(X,Y)=0
# --> if someone is in Group 1 cannot be in Group 2

observed_diff <- mean(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"],
                      na.rm = TRUE) -
  mean(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"], na.rm = TRUE)

# SE for the mean = corr st dev / sqrt(n)

sd_NB <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"],
            na.rm = TRUE)
sd_B <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"],
           na.rm = TRUE)

n_NB <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"])) 
n_B <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"]))

SE_Unified <- sqrt(sd_NB^2/n_NB + sd_B^2/n_B)

test_stat_2sample_mean <- (observed_diff - delta_0)/SE_Unified

# Step #3 p-value calculation

# test_stat ~ t(v) if H0 is TRUE

# v = df_1 + df_2 = n_1 + n_2 - 2 --> two group's st deviations are equal in the pop: NOPE
# v = Welch's formula (correction) --> does NOT assume equal st dev for 2 groups

# t(v)-distribution --> calculate p-value same as last week (so far)

t.test(Education_Years ~ ScientistsDecievePublic, data = ESS,
       alternative = "greater", mu = delta_0)
# p-value = 7.5%

# Step #4 Decision

# p-value = 7.5%

# 1% < p-value < 10% --> inconclusive test, increase the sample size to be conclusive

# Demonstrate the unrobustness of the decision using two significance levels!

# alpha = 10% --> p-value < alpha --> H0 is rejected
# alpha = 5%  --> p-value > alpha --> fail to reject H0

# Assumptions of the Test --> when it's correct to use t distribution for p-value calc
# n_1 and n_2 are also > 50 (could also use N(0,1))
# even if one sample size < 50 --> small sample --> assume numeric data is normal

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

n_B <- sum(ESS$SecretGroupInfluenceWorldPol=="Yes")
n_NB <- sum(ESS$SecretGroupInfluenceWorldPol=="No")

# k_i --> number of favorable cases in the proportion for group 'i'

k_B <- sum(ESS$SecretGroupInfluenceWorldPol=="Yes" &
             ESS$TrustInParlament=="Yes")
k_NB <- sum(ESS$SecretGroupInfluenceWorldPol=="No" &
              ESS$TrustInParlament=="Yes")

p_B <- k_B/n_B # 11.1%
p_NB <- k_NB/n_NB # 14.6%

delta_0 <- 0.02
observed_diff_p <- p_B - p_NB
# already suspect --> H1 is very very unlikely (theoretical and observed diff
#                                               have different signs)

# test_stat = (observed_diff_p - delta_0)/SE_Unified_p
# SE_Unified_p = sqrt(SE_1^2 + SE_2^2)
# SE_p = sqrt(p*(1-p)/n)

test_stat_2sample_p <- (observed_diff_p - delta_0)/sqrt(
  p_B*(1-p_B)/n_B + p_NB*(1-p_NB)/n_NB
)

# Step #3 p-value

# test_stat ~ N(0,1) if H0 TRUE and large sample size (CLT)

# test for the large sample size to apply CLT and N(0,1) for p-value calc
(n_B*p_B >= 10 & n_B*(1-p_B) >= 10) & (n_NB*p_NB >= 10 & n_NB*(1-p_NB) >= 10)
# TRUE --> large sample size is OK here

# H0T: P_B - P_NB = 0.02 (thinking: P_B - P_NB <= 0.02)
# H1: P_B - P_NB > 0.02

# Best Case fro H0 if test stat = -Inf

# let's see this N(0,1) distribution by drawing its density function
x_axis <- seq(-5,5,0.01) # gives every number between +-5 with 0.01 steps
y_axis <- dnorm(x_axis)
plot(x_axis, y_axis, type = "l",
     xlab = "Test Statistics from Lots of Samples if H0 TRUE",
     ylab = "Probability")
# see where my test stat is in this distribution
abline(v = test_stat_2sample_p, col="red")

# p-value calc: 100% when test stat is -Inf and decrease id we get further (get larger)
1-pnorm(test_stat_2sample_p)
# 99.9%

# Step #4 Decision

# p-value of 99.9% --> of course, fail to reject H0

# difference between sample proportions is not significantly larger than 2%-points

# Step 3/b)

# H0T: P_B - P_NB = -0.02 (thinking: P_B - P_NB >= -0.02)
# H1: P_B - P_NB < -0.02

delta_0 <- -0.02

test_stat_2sample_p <- (observed_diff_p - delta_0)/sqrt(
  p_B*(1-p_B)/n_B + p_NB*(1-p_NB)/n_NB
)

# see where the new test stat is in N(0,1)
abline(v=test_stat_2sample_p, col="blue")

# best case for H0 if test stat = +Inf
# decrease p-value if test stat gets smaller and smaller than +Inf
pnorm(test_stat_2sample_p)
# 17.6%

# Step 4/b)

# p-value = 17.6% > 10% --> fail to reject H0
# difference between the proportions is not signif smaller than -0.02

# try: what is the maximum theoretical difference (in abs value) that is significant on all
# common levels!