setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)

# ------------------------------------------------------------------------------
# PART 1
wines <- read.csv("wines.csv")
# a) 
# Test whether the wine quality can be considered independent of the wine acidity on the
# common significance levels. Clearly state the null and alternative hypotheses, perform the
# necessary calculations, check the assumptions of the test 
# (and if necessary, logically modify the data), and provide a written conclusion.

# CHI-SQUARED TEST OF INDEPENDENCE
crosstab <- table(wines[, c("quality_score", "pH_class")])
result <-  chisq.test(crosstab)
result$expected
# not all are > 5

wines$quality <- ifelse(wines$quality_score <= 4, "< 4", wines$quality_score )
crosstab <- table(wines[, c("quality", "pH_class")])
result <-  chisq.test(crosstab)
result$p.value
# REJECT H0 --> not independent

# b)
# Identify where and by how much the sample frequency deviates most from the expected
# frequencies under the assumption of independence.
difference <- result$expected - crosstab
difference

# c) 
#Measure the strength of the relationship between the two variables in the observed sample
# with the appropriate statistical measure. Interpret the results.

# Cramers V
sqrt(result$statistic/(nrow(wines)*(2-1)))
# weak relationship

# ------------------------------------------------------------------------------
# PART 2
library(readxl)
houses <- read.csv("Households.csv")
str(houses)

# a)
# convert variable types
unique(houses$Residence)
unique(houses$Gender)

houses$Residence <- as.factor(houses$Residence)
houses$Residence <- relevel(houses$Residence, "4")
houses$Gender <- as.factor(houses$Gender)


food_exp_model <- lm(FoodExp ~ Residence + HholdSize + RestMeals + Gender +
                       Age + Income, data = houses)

model1_sum <- summary(food_exp_model)


food_exp_int <- lm(FoodExp ~ Residence + HholdSize + RestMeals + Gender +
                     Age + Income + I(Income^2) + I(Age^2) + RestMeals*Income +
                     RestMeals*HholdSize, data = houses)
model2_sum <- summary(food_exp_int)

# LM TEST
m <- length(coef(food_exp_int)) - length(coef(food_exp_model))
q <- length(coef(food_exp_int))
n <- nrow(houses)

Rsq_1 <- model2_sum$r.squared
Rsq_2 <- model1_sum$r.squared
TestFunction <- (n-q-m)*(Rsq_2-Rsq_1)/(1-Rsq_1)
1-pchisq(TestFunction,m)
# they shoud stay



summary(food_exp_int)





