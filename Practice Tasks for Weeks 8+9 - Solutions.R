setwd("~/Oktatás 2024252/Statistical Modeling") # change path to where you downloaded the 'PracticeData89.xlsx' file

#----------------------------------------------------
# Income worksheet
#----------------------------------------------------

library(readxl)
income_data <- read_excel("PracticeData89.xlsx", sheet = "Income")
str(income_data)

# 1)
library(ggplot2)

ggplot(income_data, aes(x=Education, y=Income, fill=Education)) + geom_boxplot()

# The typical 50% of income is highest for PhD graduates, it's even higher than for the third quartile of Master Degree.
# Lowest income is generally for those with no university degree, their median salary is at the level of the lowest 25% for
# graduates with Bachelor Degrees.
# Typical 50% of incomes for Bachelor and Master Degree graduates is similar, though it's a bit higher for Master Degrees as their
# median income is at the same level as the lower quartile of Bachelor Degree graduates.

# 2)

helper_table <- aggregate(Income ~ Education, data = income_data, FUN=sd)
helper_table$freq <- table(income_data$Education)
helper_table

within_sd <- sqrt(sum((helper_table$freq-1)*helper_table$Income^2)/(nrow(income_data)-1))
# A randomly selected employee's income is expected to differ from the mean income of their own eduaction level by
# +- 4222 USD.

# 3)

within_ss <- within_sd^2*(nrow(income_data)-1)
total_ss <- sd(income_data$Income)^2*(nrow(income_data)-1)

eta_sq <- 1-within_ss/total_ss
# Education level explains 15.5% of the variability of income for the observed employees. It is a moderate relationship as
# eta^2 is between 10% and 50%

oneway.test(Income ~ Education, data = income_data, var.equal = FALSE)
# p-value = 2.491e-08 < 0.01 --> H0 is rejected at every common significance levels
# The moderate-level relationship between income and education is significant for unobserved employees as well
# on every common significance levels.

# 4)
income_reg <- lm(Income ~ Experience+Training, data = income_data)
summary(income_reg)
# Work experience and number of trainings explain 44.1% in the variability of employee's income.

# 5)
income_reg

# A new employee with no work experience and who has not completed any trainings before is expected to earn
# 1479 USD a month according to our regression

# Out of two employees with the same number of trainings completed in the past 5 years, the one with 1 extra year of
# work experience is expected to earn 390 USD more a month.

# 6)

summary(income_data)

# Mean level
245.55*2.759/(1479.04+390.43*11.42+245.55*2.759)
# If for an employee the number of completed trainings increases by 1% compared to the mean value of 2.759,
# their income is expected to increase by 0.10%

# Median level
245.55*3/(1479.04+390.43*10+245.55*3)
# If for an employee the number of completed trainings increases by 1% compared to the median value of 3,
# their income is expected to increase by 0.12%

# 7)
summary(income_reg)

# Number of Trainings Completed as the p-value of its coefficient is larger than 10%, which means its (1-0.1)=90% confidence interval
# contains zero. So, with 90% we can say that the coefficient (it's marginal effect on income) can be considered as 0.

#----------------------------------------------------
# Apartment worksheet
#----------------------------------------------------

apartments <- read_excel("PracticeData89.xlsx", sheet = "Apartment")
str(apartments)

# 1)

# H0: Prices follow Exponential distribution
# H1: Prices don't follow Exponential distribution

# test statistic calculation
lambda <- 1/mean(apartments$Price)

exp_quintiles <- qexp(c(0,0.2, 0.4, 0.6, 0.8,1), rate = lambda)

observed_freq <- table(cut(apartments$Price, breaks = exp_quintiles))
observed_freq

# expected freq in the quintile bins is n*0.2 = 500*0.2 = 100 > 10 --> test assumption is met

chi2_result <- chisq.test(observed_freq)

# p-value calculation: number of estimated parameters for the test is 1 (lambda)
p_value_chi2 <- 1-pchisq(chi2_result$statistic, df = 5-1-1)
p_value_chi2*100 # in percentage format
# Our p-value is practically 0. Thus, H0 can be rejected on every common levels
# The price distribution cannot be considered exponenetial.

# 2)

# H0: bathroom distribution is the same for apartments with and without air conditioning
# H1: bathroom distribution is different for apartments with and without air conditioning

# check test assumptions
crosstab <- table(apartments[,c("Bathrooms", "AirCond")])
chi2_result <- chisq.test(crosstab)
chi2_result$expected
# The expected frequencies in the contingency table under H0 are lower than 5 for apartments with 6 bathrooms.
# Merge them with apartments with 5 bathrooms

apartments$BathMerged <- ifelse(apartments$Bathrooms >= 5, "5+", apartments$Bathrooms)

# check test assumptions again
crosstab <- table(apartments[,c("BathMerged", "AirCond")])
chi2_result <- chisq.test(crosstab)
chi2_result$expected
# We have at least 5 expected frequencies in every category now

# See the p-value
chi2_result

# p-value = 0.3812 > 0.1 --> H0 can't be rejected on any common significance levels -->
# --> The distribution for the number of bathrooms is not significantly different for apartments with and
#     without air conditioning on any common significance levels

# 3)
crosstab - chi2_result$expected
# We have about 3.9 more apartments with 3 bathrooms with air conditioning category than we expect to have in case of
# completely identical bathroom distribution. However, according to the result of Task 2 this difference is not significant.

# 4)
apart_reg <- lm(Price ~ Area+Bathrooms, data = apartments)
apart_reg
confint(apart_reg, level = 0.99)

# The apartment price is expected to increase by 36.35 thousand AED if area increases by 1 m^2 while
# the number of bathrooms remains the same.
# This effect is expected to be between 33.2 and 39.5 thousand AED for unobserved apartments with 99% confidence.

# 5)

# H0: R^2 = 0
# H1: R^2 > 0

# test statistics and p-value
summary(apart_reg)
# p-value: < 2.2e-16 < 0.01 --> H0 is rejected on every common significance levels -->
# --> The model has significantly positive explanatory power on all common significance levels
#     in the population of unobserved apartments

# 6)

direct_bath <- apart_reg$coefficients[3]

reg_area_bath <- lm(Area ~ Bathrooms, data = apartments)
indirect_bath <- reg_area_bath$coefficients[2] * apart_reg$coefficients[2]

total_bath <- direct_bath + indirect_bath

# check total effect
lm(Price ~ Bathrooms, data = apartments)$coefficients[2] - total_bath
# difference is practically 0, we are ok

# With area unchanged, one extra bathroom is expected to decrease the price by 665 thousand AED (direct effect)
# However adding one bathroom is expected to increase area and this leads to an expected price increase by 1912 thousand AED (indirect effect)
# These two effects result in a total expected price increase by 1247 thousand AED in case of one extra bathroom

#----------------------------------------------------
# Flats worksheet
#----------------------------------------------------

flats <- read_excel("PracticeData89.xlsx", sheet = "Flats")
str(flats)

# 1) 

# H0: sample is representative for settlement types
# H1: sample is not representative for settlement types

# settlement frequency distribution in the sample 
table(flats$Settlement)

# settlement distribution in the population
# give the percentages in the same order as in the outout of the 'table' function
settlement_pop_props <- c(0.6, 0.33, 0.02, 0.05)

# get the expected frequencies in the sample if H0 is true
expected_freq <- settlement_pop_props * nrow(flats)
expected_freq

# The expected frequencies under H0 are lower than 5 for municipalities.
# Merge them with towns
flats$SettlementSimple <- ifelse(flats$Settlement %in% c("municipality","town"),
                                 "municipality + town",
                                 flats$Settlement)

observed_freq <- table(flats$SettlementSimple)
observed_freq

settlement_pop_props <- c(0.6, 0.33, 0.02+0.05)
expected_freq <- settlement_pop_props * nrow(flats)
expected_freq
# We have at least 5 expected frequencies in every category now
# We can do the test of representativity now

# 2)

# H0: sample is representative for settlement types
# H1: sample is not representative for settlement types

chisq.test(observed_freq, p = settlement_pop_props)
# p-value = 0.007787 < 0.01 --> H0 is rejected on every common significance levels -->
# --> the sample is not representative for settlement types on all common significance levels

# 3)
flats_reg <- lm(Price ~ Rooms+Size+Floor, data = flats)
summary(flats_reg)
# Most important: Size --> lowest p-value = lowest error probability for considering it significant
# Least important: Rooms --> highest p-value = highest error probability for considering it significant

# 4)
direct_rooms <- flats_reg$coefficients[2]

total_rooms <- lm(Price ~ Rooms, data = flats)$coefficients[2]

indirect_rooms <- total_rooms - direct_rooms

cor(flats[,c(1,3:5)])

# With size and floor unchanged, one extra room is expected to increase the price by 0.37 million pounds (direct effect)
# However adding one bathroom is expected to increase size and flats with more rooms are generally on higher floors,
# and this leads to an expected price increase by 1.73 million pounds (indirect effect)
# These two effects result in a total expected price increase by 2.09 million pounds in case of one extra room

# 5/a)
summary(flats_reg)
# The number of rooms, the size and the floor together determine 72.85% of the variability in flat prices.

# 5/b)
confint(flats_reg, level = 0.95)
# If a flat is one floor higher with size and number of rooms unchanged, its price is expected to increase by
# 0.42 - 0.68 million pounds with 95% confidence

# 5/c)
flats_reg
flats[42,]
0.07294*76/(3.54736+0.36775*4+0.07294*76+0.55095*5)
# If the size of a flat increases by 1% while number of rooms, and floor remains the same, then its price
# is expected to grow by 0.42%