setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)
library(ggplot2)

df <- read_excel("PracticeData89.xlsx", sheet = "Income")
str(df)

# 1.
# Examine the nature of the relationship between income and education graphically
ggplot(df, aes(x = Education, y = Income, fill = Education)) +
  geom_boxplot()

# 2.
#Calculate and interpret the within-group standard deviation with respect to income and
# education.
helper_table <- aggregate(Income ~ Education, data = df, FUN = sd)
helper_table$freq <- table(df$Education)

within_sd <- sqrt(sum((helper_table$freq - 1)*helper_table$Income^2) / (nrow(df)-1))
# A randomly selected employee's income is expected to differ from the mean income of their own eduaction level by
# +- 4222 USD.


# 3.
# Evaluate the strength of the relationship in both the sample and the population.
# sample
within_ss <- within_sd^2*(nrow(df)-1)
total_ss <- sd(df$Income)^2*(nrow(df)-1)

eta_sq <- 1-within_ss/total_ss
# Education level explains 15.5% of the variability of income for the observed employees. It is a moderate relationship as
# eta^2 is between 10% and 50%

# population
oneway.test(Income ~ Education, data = df, var.equal = FALSE)
# p-value = 2.491e-08 --> the relationship is significant

# 4. 
# Build a regression model using income as the dependent variable, with the number of
# trainings and work experience as explanatory variables. Assess the explanatory power
# of the model
model_1 <- lm(Income ~ Training + Experience, data = df)
summary(model_1)
# Work experience and number of trainings explain 44.1% in the variability of employee's income.


# 5. Interpret the intercept and the coefficient of experience.
# A new employee with no work experience and who has not completed any trainings before is expected to earn
# 1479 USD a month according to our regression
# Out of two employees with the same number of trainings completed in the past 5 years, the one with 1 extra year of
# work experience is expected to earn 390 USD more a month.

# 6
# Calculate and interpret the elasticity of the number of trainings 
# at both the mean and median levels.
mean <- mean(df$Training) #2.76
median <- median(df$Training) # 3
summary(model_1)

# income_est = 1479 + 245.55*Training + 390.43*Experience
elasticity_mean_level = 245.55*2.76 / (1479.04+390.43*11.42+245.55*2.759)
# If for an employee the number of completed trainings increases by 1% compared to the mean value of 2.759,
# their income is expected to increase by 0.10%

elasticity_median_level = 245.55*3 / (1479.04+390.43*11.42+245.55*3)
# If for an employee the number of completed trainings increases by 1% compared to the median value of 3,
# their income is expected to increase by 0.12%

# 7.
# Which variables can be considered to have no effect in the population with 90%
# confidence? Justify your choice!

summary(model_1)
# training cannot be --» its p value is biger than 0.1, the rest are ok

# ------------------------------------------------------------------------------
# PART 2
library(readxl)
library(ggplot2)

apartments <- read_excel("PracticeData89.xlsx", sheet = "Apartment")
str(apartments)

# 1)
# check if apartment prices are exp. distr.
# H0: prices follow exp distr.

lambda <- 1/mean(apartments$Price)
exp_quintiles <- qexp(c(0,0.2,0.4,0.6,0.8,1), rate = lambda)
observed_freq <- table(cut(apartments$Price, breaks = exp_quintiles))
observed_freq
chi_sq_result <- chisq.test(observed_freq)
chi_sq_result$p.value
# Our p-value is practically 0. Thus, H0 can be rejected on every common levels
# The price distribution cannot be considered exponenetial.


# 2)
# H0: bathroom distribution is the same for apartments with and without air conditioning
# H1: bathroom distribution is different for apartments with and without air conditioning

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
chi2_result$p.value
# 0.3811858 --> fail to reject H0 --> the distr. is not the same


# 3)
# identify where and by how much the sample frequency deviates most from the expected
# frequencies under the assumption of identical number of bathrooms distributions.

crosstab - chi2_result$expected
# We have about 3.9 more apartments with 3 bathrooms with air conditioning category than we expect to have in case of
# completely identical bathroom distribution. However, according to the result of Task 2 this difference is not significant.


# 4)
# Construct a regression model in which apartment price is explained by area and number
# of bathrooms. Interpret the coefficient of area and its 99% confidence interval
apart_reg <- lm(Price ~ Area + Bathrooms,data = apartments)
summary(apart_reg)
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
# With area unchanged, one extra bathroom is expected to decrease the price by 665 thousand AED (direct effect)
# However adding one bathroom is expected to increase area and this leads to an expected price increase by 1912 thousand AED (indirect effect)
# These two effects result in a total expected price increase by 1247 thousand AED in case of one extra bathroom


# ------------------------------------------------------------------------------
# PART 3
library(readxl)
library(ggplot2)

flats <- readxl::read_excel("PracticeData89.xlsx", sheet = "Flats")
str(flats)

# test of representativity
# H0: it is representative
# H1: it is not representative

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

chisq.test(observed_freq, p = settlement_pop_props)
# p-value = 0.007787 < 0.01 --> H0 is rejected on every common significance levels -->
# not representative


# 3)
flats_reg <- lm(Price ~ Rooms + Size + Floor, data = flats)
summary(flats_reg)


# 4)

direct_rooms <- flats_reg$coefficients[2]
total_rooms <- lm(Price ~ Rooms, data = flats)$coefficients[2]
indirect_rooms <- total_rooms-direct_rooms
# With size and floor unchanged, one extra room is expected to increase the price by 0.37 million pounds (direct effect)
# However adding one bathroom is expected to increase size and flats with more rooms are generally on higher floors,
# and this leads to an expected price increase by 1.73 million pounds (indirect effect)
# These two effects result in a total expected price increase by 2.09 million pounds in case of one extra room



# 5:
summary(flats_reg)
# coef.of. det = 0.7285


confint(flats_reg, level = 0.96)
# If a flat is one floor higher with size and number of rooms unchanged, its price is expected to increase by
# 0.42 - 0.68 million pounds with 95% confidence

# elasticity of size for apartment 42
flats[42, ] # 76

summary(flats_reg)
# price = 3.54736 + 0.36775*Rooms + 0.07294*Size + 0.55095*Floor
# derivative acc. to size = 0.007294
# elastiicity at 76:
0.007294*76 / (3.54736 + 0.36775*4 + 0.07294*76 + 0.55095*5)
# If the size of a flat increases by 1% while number of rooms, and floor remains the same, then its price
# is expected to grow by 0.42%



                  