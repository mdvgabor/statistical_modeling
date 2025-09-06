# Accepting the null hypothesis of analysis of variance (ANOVA) means that the expected differ-
# ence of group means from the overall mean can be considered 0 in the population.
# TRUE

# Increasing the number of explanatory variables (parameters) in the regression model will cer-
# tainly increase the Akaike Information Criterion (AIC).
# FALSE

# The larger the beta coefficient of an explanatory variable in a linear regression, the more im-
# portant that variable is, ceteris paribus, in estimating the outcome variable of the model.
# FALSE

# The Ramsey RESET test is essentially an Wald-test with 2 restrictions.
# TRUE

# If the slope with respect to a continuous variable differs between groups, then an interaction is
# needed in the model.
# TRUE

# In the case of a categorical explanatory variable with 3 outcomes, 3 dummy variables can be cre-
# ated, and all of them should be included in the model.
# FALSE

setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
wines <- read.csv("wines.csv")

# SHORT CASE STUDY

# 1) Test whether the wine quality can be considered independent of the wine acidity
# H0: there is no sign. relationship
# H1: there is sign. relationship

# chi-squared test
crosstab <- table(wines[, c("quality_score", "pH_class")])
result <-  chisq.test(crosstab)
result$p.value
result$expected
# not all are > 5

wines$quality <- ifelse(wines$quality_score <= 4, "< 4", wines$quality_score )
crosstab <- table(wines[, c("quality", "pH_class")])
crosstab
result <-  chisq.test(crosstab)
result$p.value
# REJECT H0 --> relationship is significant


# 2)
# Identify where and by how much the sample frequency deviates most from the expected
# frequencies under the assumption of independence
result$expected - result$observed
result$expected - crosstab

# 3)
# Measure the strength of the relationship between the two variables in the observed sample
# with the appropriate statistical measure. Interpret the results.

# Cramer's V:
sqrt(result$statistic/(nrow(wines)*(2-1)))
# 0.1275077 --> Weak Relationship Between The Variables


# LONG CASE STUDY
houses <- read.csv("Households.csv")

# 1)
# Check whether the variable types are appropriate! If not, transform the variables accord-
# ingly so that they are suitable for model building

houses$Residence <- as.factor(houses$Residence)
houses$Gender <- as.factor(houses$Gender)

#  Build the original model described at the beginning of this task! Then build a new model
# including the following additional variables:

model_1 <- lm(FoodExp ~ Residence + HholdSize + RestMeals + Gender + Age + Income,
              data = houses)
summary(model_1)

model_2 <- lm(FoodExp ~ Residence + HholdSize + RestMeals + Gender + Age + Income +
                I(Income^2) + I(Age^2) + RestMeals*Income + RestMeals*HholdSize, data = houses)
summary(model_2)

# 3)
# Compare the two models using at least three criteria or indicators. 
# Which is the better model?

AIC(model_1, model_2) # model_2
BIC(model_1, model_2) # model_2

# H0: The beta coefficients of the added variables are 0
# H1: At least one beta of the added variables is not 0
anova(model_1, model_2)
# reject H0 --> model_2


# 4)
# Analyze the regional effects based on the dummy variable values! Are the results rational?
# Is each outcome significant? If not, can the non-significant outcomes be omitted from the
# model? Explain why or why not.
summary(model_2)

# each outcome is significant since the p values are < 0.01

# effect of Large City (2): all other variables unchanged, a person living in a Large City
# is expected to spend 1.802e+01 more on food than the same person living in Budapest

# effect of Other Town (3): same but with 1.621e+01
# effect of Village (4): same but with 1.999e+01


# 5)
# Provide the marginal effect of income for a household with an annual income of 3 million
# HUF and 10 restaurant meals! Interpret the result.
income <- 3000
rest_meals <- 10
1.171e-01 - 6.753e-06*2*income + 1.605e-03*rest_meals

# 6)
# How would the result in point (e) change if we used the logarithm of the dependent variable
# and built the same model on that?
(exp(1.171e-01 - 6.753e-06*2*income + 1.605e-03*rest_meals)-1)*100
# gives me the percentage change in the dependent variable in case of a 1000 HUF increase
# in the Income
# 9,7%









