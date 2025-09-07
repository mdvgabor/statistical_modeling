setwd("~/Oktatás 2024252/Statistical Modeling") # change path to where you downloaded the data files

#----------------------------------------------------
# unemployed.csv
#----------------------------------------------------

unemp_data <- read.csv("unemployed.csv")
str(unemp_data)

unemp_model <- lm(UCOMP ~ ., data = unemp_data)

# 1)
unemp_model$coefficients['MARRIED']
# If we take two household heads who are identical according to every predictor in the model, the
# one who is married is expected to receive unemployment compensation thet is lower by about 770 USD.

# 2)
car::vif(unemp_model)
# strongest multicollinearity: MARRIED (biggest VIF = squared standard error is inflated by multicollinearity the most)
# weakest multicollinearity: EDUC (lowest VIF = squared standard error is inflated by multicollinearity the least)

1-1/car::vif(unemp_model)[c("MARRIED", "EDUC")]
# 59.4% of the variability in MARRIED is explained by the other predictors
# only 6.7% of the variability in EDUC is explained by the other predictors

# 3)
summary(unemp_model)
# Only two variables are non-significant at 1% level: SPOUSEY and WHITE
# The p-value of the partial t-tests is to be considered here as they are for a statistical test
# where the null hypothesis is that the coefficient of the given predictor can be considered 0 for
# unobserved households. Since these two variables have p-values greater than 1%, it is suggested that
# we make a larger error than allowed by alpha if we reject the H0 of their coefficients are 0 in the population

# 4)
unemp_restricted <- lm(UCOMP ~ .-SPOUSEY-WHITE, data = unemp_data)

# method 1: ICs
AIC(unemp_model, unemp_restricted) # prefers the original model
BIC(unemp_model, unemp_restricted) # prefers the new, restricted model

# method 2: Wald-test
anova(unemp_model, unemp_restricted) # p-value = 4.7% mixed case, a bit closer to H1 (prefering the restricted model)

# The IC that is more lenient with additional predictor (Akaike) prefers the original model, while the Bayesian IC
# that is more strict on new variables prefers the restricted model. It's a draw on this front. Wald-test is also
# in the inconlusive range (1%-10%) with its p-value, but slightly closer to 1%, so slightly preferring the restricted model,
# So, the decision is based on the modeller's preference on the number of predictors in the model.

# 5)
# H0: B_MALE + B_MARRIED = 0
# H1: B_MALE + B_MARRIED != 0
# --> two-sided test

sum_of_coeffs = sum(unemp_model$coefficients[c("MALE", "MARRIED")])

covariance_of_coeffs <- vcov(unemp_model)
squared_SE_MALE <- covariance_of_coeffs["MALE", "MALE"]
squared_SE_MARRIED <- covariance_of_coeffs["MARRIED", "MARRIED"]
cov_MALE_MARRIED <- covariance_of_coeffs["MALE", "MARRIED"]
SE_for_the_sum <- sqrt(squared_SE_MALE + squared_SE_MARRIED + 2*cov_MALE_MARRIED)

test_stat_for_the_sum <- (sum_of_coeffs-0)/SE_for_the_sum

df_for_t_distribution <- unemp_model$df.residual

p_value <- 2*pt(-abs(test_stat_for_the_sum), df = df_for_t_distribution)
# p-value = 88% --> >5% --> H0 can't be rejected, the sum of the two coefficients can be considered as 0 in the population
# this means that the marginal effects of these two variables on unemployment compensation cancel each other out:
# --> the amount of expected compensation for males disappears if there is also a wife in the picture
# --> probably the wife usually has a job, so the amount of compensation is reduced since the male doesn't need to
#     finance the whole household

# 6)
unemp_nonlin <- lm(UCOMP ~ . + I(UHOURS^2) + UHOURS*MALE, data = unemp_data)
unemp_nonlin

# d_pred_UCOMP/d_UHOURS = 3.299e+00 - 2*-4.062e-04*UHOURS^2 + 1.292e-01*MALE

# marg effect for a female household head with an average number of unemployed hours
avg_UHOURS <- mean(unemp_data$UHOURS)

3.299e+00 - 2*-4.062e-04*avg_UHOURS^2 + 1.292e-01*0
# If our female household head spend an additional hour unemployed compared to the average,
# while all other parameters remain the same, the household can expect an additional 600 USD compensation

# 7)
lmtest::resettest(unemp_model) # p-value = 0.0827
lmtest::resettest(unemp_nonlin) # p-value = 0.002261
# The p-value of the RESET specification test decreased by adding the non-linear terms, suggesting that the H0
# of good specification does not stand for the non-linear model, making the non-linear extensions worthless

library(ggplot2)
ggplot(unemp_data, aes(x=UHOURS, y=UCOMP)) + geom_point() + geom_smooth(method=lm) + geom_smooth(color="red")
# the fitted non-linear trendline does not seem to deviate much from the linear, suggesting there is no
# need to add the squared UHOURS to the model

ggplot(unemp_data, aes(x=UHOURS, y=UCOMP, color=as.factor(MALE))) + geom_point() + geom_smooth(method=lm)
# There is only a slight difference between the slopes of males and females, and the confidence intervals
# do not intersect only in a region where we have very few female observations, suggesting that adding the
# interaction term for the model is questionable

#----------------------------------------------------
# statistics_scores.xlsx
#----------------------------------------------------

scores <- readxl::read_excel("statistics_scores.xlsx")
str(scores)

scores_model <- lm(Score ~ PrevGrade + D_Always + D_Mostly + D_Sometimes +
                     D_Always*PrevGrade + D_Mostly*PrevGrade + D_Sometimes*PrevGrade,
                   data = scores)

# 1)
scores_model$coefficients['PrevGrade'] + scores_model$coefficients['PrevGrade:D_Mostly']
# by + 0.37 points

# 2)
scores_model$coefficients['PrevGrade:D_Sometimes']
# The additional benefit in midterm 3 scores of 1 extra Statistics I grade for someone who only
# attended seminars sometimes compared to never is lower by 0.45 points on average.

# 3)
scores_model_restricted <- lm(Score ~ PrevGrade + D_Always + D_Mostly + D_Sometimes,
                              data = scores)

# H0: the R^2 of the model without interaction is not significantly lower
#     compared to the model with interactions
# H1: the R^2 of the model without interaction is significantly lower
#     compared to the model with interactions

anova(scores_model, scores_model_restricted)
# test statistic = 2.5025
# p-value = 0.06133

# Since the p-value of 6.1% is bigger than the 1% level, H0 can't be rejected, so the model
# without interactions is preferred.

# 4)
summary(scores_model_restricted)
# The t-test p-value for D_Sometimes is 59%, which is way higher than 5%, so H0 can't be rejected,
# the variable has no significant effect on midterm scores. Therefore, it is not worth to attend seminars
# sometimes instead of never (the reference category). It is better to always stay at home,
# You can expect no additional points in the midterm by attending this rarely to seminars.

# 5)
skedastic::white(scores_model, interactions = FALSE)
# p-value = 0.943 > 0.05 --> H0 can't be rejected, the model is homoskedastic
# There is no need to apply GLS estimation.

# 6)
# GLS is an alternative estimation to OLS that uses the squared prediction errors of the original OLS estimation as
# weights in a second round of OLS estimations. Resulting in new coefficient estimates and producing a new model
# where the error term is now homoskedastic due to the weighting with the original squared errors.
# An alternative is White's robust standard errors. This method does not re-estimate the whole model, it just corrects
# the coefficient's standard error formula so that the test statistics calculated from them follow a t-distribution
# under H0, resulting in valid p-values for the partial t-tests of coefficients.

#----------------------------------------------------
# employee.xlsx
#----------------------------------------------------

employee <- readxl::read_excel("employee.xlsx")
str(employee)

employee$Settlement <- as.factor(employee$Settlement)
employee$Settlement <- relevel(employee$Settlement, ref = "Village")

employee_model <- lm(CurrentSalary ~ StartSalary+Settlement + StartSalary*Settlement, data = employee)

# 1)
employee_model

# d_Pred_CurrentSalary/d_StartSalary = 1.7769 - 0.5107*Budapest - 1.6555*Town

1.7769 - 0.5107*1 - 1.6555*0
# If an employee's initial salary was higher by 1000 USD, they can expect a current salary higher by 1266 USD
# if they work in Budapest

# 2)
residual_term <- employee_model$residuals

# Koenker correction for the BP test is needed if residual is not normal

# H0: residual is normally distributed
# H1: residual is not normally distributed

mean_resid <- mean(residual_term)
sd_resid <- sd(residual_term)

norm_quintiles <- qnorm(c(0,0.2,0.5,0.6,0.8,1), mean = mean_resid, sd = sd_resid)

observed_freq <- table(cut(residual_term, breaks = norm_quintiles))

chi2_result <- chisq.test(observed_freq)
1-pchisq(chi2_result$statistic, df = 5-2-1)
# p-value = 0.0 --> practically 0, H0 is rejected in all common signifance levels --> residual is NOT normal

# Koenker correction is needed for BP test

# H0: residual is homoskedastic
# H1: residual is heteroskedastic

lmtest::bptest(employee_model, studentize = TRUE)
# p-value = 3.528e-10 --> p-value < 0.05 --> H0 is rejected --> residual is heteroskedastic

# 3)
White_covariance_of_coeffs <- car::hccm(employee_model)
lmtest::coeftest(employee_model, vcov. = White_covariance_of_coeffs)

# 4)
# With White's robust standard errors we do not re-estimate the whole model, just correct the coefficient's
# standard error formula so that the test statistics calculated from them follow a t-distribution
# under H0, resulting in valid p-values for the partial t-tests of coefficients.
# The alternative solution is Generalized Least Squares (GLS) that uses the squared prediction errors of the
# original OLS estimation as weights in a second round of OLS estimations.
# Resulting in new coefficient estimates and producing a new model where the error term is now homoskedastic
# due to the weighting with the original squared errors.

# 5)
loglin <- lm(log(CurrentSalary) ~ StartSalary+Settlement, data = employee)
exp(loglin$coefficients['StartSalary'])
# For two employees from the same type of settlement, if one had an initial salary that is higher by 1000 USD,
# they can expect their current salary to be higher as well by 2.9%

# 6)
loglog <- lm(log(CurrentSalary) ~ log(StartSalary)+Settlement, data = employee)
loglog$coefficients['log(StartSalary)']
# For two employees from the same type of settlement, if one had an initial salary that is higher by 1%,
# they can expect their current salary to be higher as well by 0.79%

# 7)

# H0: B_log(StartSalary) = 1
# H1: B_log(StartSalary) < 1
# --> right-sided test

B_logStartSalary <- loglog$coefficients['log(StartSalary)']
SE_B_logStartSalary <- sqrt(vcov(loglog)["log(StartSalary)", "log(StartSalary)"])

test_stat <- (B_logStartSalary - 1)/SE_B_logStartSalary

pt(test_stat, df = loglog$df.residual)
# p-value = 2.665779e-08 --> p-value < 0.01 --> H0 rejected on all common alpha levels
# --> the coefficient of StartSalary is significantly lower than 1
# --> this suggests that an initial salary higher by 1% does not cause the same level of increase in current salary
#     but significantly lower (while assuming that settlement type did not change)

# 8)
lmtest::resettest(loglin) # p-value < 2.2e-16
lmtest::resettest(loglog) # p-value = 0.006651

# The log-log model has a higher p-value on the RESET test, suggesting the H0 of correct model specification is
# more acceptable there. So, the log-log model is prefered according to the RESET test

ggplot(employee, aes(x=StartSalary, y=log(CurrentSalary))) + geom_point() + geom_smooth(method=lm)
# Start salary has outliers on the right tail, needs to be logarithmized as well!

ggplot(employee, aes(x=log(StartSalary), y=log(CurrentSalary))) + geom_point() + geom_smooth(method=lm)
# Now the relationships is appropriately described by a linear trend, there are no outliers distorting the
# slope in either of the two variables.

# The log-log model is preferred according to the graphical test as well.