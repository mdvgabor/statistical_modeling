setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)
# ------------------------------------------------------------------------------
# CHAPTER 12: NON-PARAMETRIC TESTS

# So far parametric tests.
# hypotheses — about possible population values of statistical indicator

# now:
# on-parametric tests
# hypotheses about the population distribution of variables

# SAME STEPS AS BEFORE

sfH <- readxl::read_excel("StackOverflowHungary2020.xlsx")
str(sfH)

# Goodness of fit tests:

# 1) Test of Representativity: 
# the sample’s distribution with respect to a specific variable is approximately 
# the same as the distribution of that variable in the entire (unobserved) population.

# is the Hungarian sample from the StackOverflow questionnaire representative 
# in terms of employment types?????

# H0: it is representative
# H1: it is not representative

# solution : right tailed chi-squared test

observed_freq <- table(sfH$Employment)
theor_probs <- c(0.85, 0.04, 0.11)

chisq.test(observed_freq, p = theor_probs)
# p value = 7% --> fail to reject H0 --> need larger sample

# what would we need to make H0 true:
representativity_result <- chisq.test(observed_freq, p = theor_probs)
representativity_result$expected # results are of course theoretical



# 2) Test of normality: is the data normally distributed
# Solution : Chi-Squared Test again

hist(sfH$Age1stCode)

# H0: The distribution is normal
# H1: The distribution is NOT normal

sample_mean <- mean(sfH$Age1stCode)
s <- sd(sfH$Age1stCode)

# get the quintiles of the normal distribution
norm_quintiles <- qnorm(c(0,0.2, 0.4, 0.6, 0.8,1), mean = sample_mean, sd = s)
norm_quintiles

observed_freq <- table(cut(sfH$Age1stCode, breaks = norm_quintiles))
observed_freq

# use chi-squared test
chi2_result <- chisq.test(observed_freq)

# get the p value --> use right-tailed test
# df = n-b-1 b = 2 --> estimated parameters of the distribution
p_value_chi2 <- 1-pchisq(chi2_result$statistic, df = 5-2-1)


# 3) Test of Homogenity: see if two or more groups have same distr. in the population
# H0:  The two groups (Windows and non-Windows) have identical distributions in the population
# H1: The two groups (Windows and non-Windows) have different distributions in the population

unique(sfH$OpSys)

sfH$OpSys_Grouped <- ifelse(sfH$OpSys == "Windows", "Windows", "NotWindows")
table(sfH$OpSys_Grouped)

# use crosstab
crosstab <- table(sfH[, c("JobSat", "OpSys_Grouped")])
crosstab

prop.table(crosstab, 2) # oszloponként

# use chi-squared test again
chisq.test(crosstab)
# pvalue = 0.204 --> Reject H0 --> not the same distr.

homogenity_result <- chisq.test(crosstab)
prop.table(homogenity_result$expected,2)
homogenity_result$observed - homogenity_result$expected

# assumption of the test is that all frequencies are >= 5
homogenity_result$expected>=5

# merging logically related categories
sfH$JobSat_v2 <- ifelse(sfH$JobSat %in% c("Very dissatisfied", "Slightly dissatisfied"), "Dissatisfied", sfH$JobSat)

crosstab_v2 <- table(sfH[, c("JobSat_v2", "OpSys_Grouped")])
crosstab_v2

chisq.test(crosstab_v2)
# p value = 0.1712
# We fail to reject H0 still

# ------------------------------------------------------------------------------
# CHAPTER 13: Testing of Bivariate Relationships
library(readxl)
CSOK <- read_excel("CSOK.xlsx")
str(CSOK)

CSOK$settlement <- as.factor(CSOK$settlement)
CSOK$type <- as.factor(CSOK$type)
CSOK$CSOK3children <- as.logical(CSOK$CSOK3children)
str(CSOK)

# ANOVA
# investigate the relationship between the housing price (a categorical variable) 
# and whether a property is eligible for CSOK benefits after having 3 children 
# (a numeric variable)

library(ggplot2)
ggplot(CSOK, aes(y = price, x = CSOK3children, fill = CSOK3children)) + geom_boxplot()

# what extent does the CSOK eligibility explain the fluctuations in housing prices?
# --> Variance ratio
aov(price ~ CSOK$CSOK3children, data = CSOK)

# variance total = 1956779 + 13970044
# SST = SSB + SSR
# variance explained = SSB/SST = 1956779 / (1956779 + 13970044) = 0.12
# only describes the strength of the relationship within the observed sample


# F-test for ANOVA
# Variance ratio and coefficient of variation only describe the strength 
# of the relationship within the observed sample! 
# A hypothesis test is needed to determine whether the observed relationship 
# can be generalized to the entire population

# H0: The relationship in the population is not significant, that is, variance ratio = 0 in the population
# H1: The relationship in the population is significant, that is, variance ratio > 0 in the population


# we use Welch test --> assumption = n > 100 in all groups
table(CSOK$CSOK3children)
# assumption met

# calc. pvalue
oneway.test(price ~ CSOK3children, data = CSOK, var.equal = FALSE)
# p-value < 2.2e-16 --> reject H0 --> var. ratio is not 0
# THE P VALUE DOESNT MEASURE STRENGHT, THE 12.3% INCREASE IS STILL HERE

# ------------------------------------------------------------------------------
# now lets examine the relationship between the Settlement type and csok eligibility
ggplot(CSOK, aes(x = settlement, fill = CSOK3children)) + geom_bar(position = "fill")
table(CSOK[, c("settlement", "CSOK3children")])
round(prop.table(table(CSOK[,c("settlement", "CSOK3children")]))*100, 1)

round(prop.table(table(CSOK[,c("settlement", "CSOK3children")]), 1)*100, 1)


# The Chi-Squared Test of Independence
# H0: There is no significant relationship in the population
# H1: There is a significant relationship in the population

test_of_indep <- chisq.test(table(CSOK[, c("settlement", "CSOK3children")]))
test_of_indep
# p-value < 2.2e-16 --> Reject H0 --> sign. rel.--> BUT DOESNT MEASURE STREGHT

# Cramer’s V
sqrt(test_of_indep$statistic/(nrow(CSOK)*(2-1))) # 2 = num. of cols.

# ------------------------------------------------------------------------------
#  Correlation and Simple Linear Regression
BP_Flats <- read_excel("BP_Flats.xlsx")
str(BP_Flats)

library(corrplot)
CorrMatrix <- cor(BP_Flats[, 1:9])
corrplot(CorrMatrix, method = "number")

# relationship between price and Area
ggplot(BP_Flats, aes(x = Area_m2, y = Price_MillionHUF)) + geom_point() +
  geom_smooth(method = "lm")

# check correlation
cor(BP_Flats$Area_m2, BP_Flats$Price_MillionHUF)

# R^2
cor(BP_Flats$Area_m2, BP_Flats$Price_MillionHUF)^2 * 100

# the lm function
model <- lm(Price_MillionHUF ~ Area_m2, data = BP_Flats)
summary(model)

# ------------------------------------------------------------------------------
# CHAPTER 14: Multivariate Linear Regression
COVID <- read.csv("COVID_Deaths_Hungary.csv")
str(COVID)

simple_model <- lm(DeathsCOVID ~ Nurses, data = COVID)
summary(simple_model)

library(corrplot)
CorrMatrix <- cor(COVID[, 2:5])
corrplot(CorrMatrix, method = "number")

# add more variables
multivar_model <- lm(DeathsCOVID ~ Nurses + Unemp + WomenOlder65, data = COVID) # note that predictors are separated by a + sign after a ~ sign
summary(multivar_model)

# Partial Elasticity
# elasticity of unemployment on COVID mortality is in the Pécs district, 
# where the number of nurses per 10,000 people is 6.5, unemployment is 222/10,000, 
# and the proportion of women over 65 is 13.3%

Betas <- coef(multivar_model)
Betas

elasticity <- Betas[3]*222 / (Betas[1]+Betas[2]*6.5+Betas[3]*222+Betas[4]*13.3)
elasticity

# partial correlation = only direct effect
# install.packages("ppcor")

library(ppcor)
pcor(COVID[,2:5])


# Predictor Importance Order based on t-tests
library(readxl)
BP_Flats <- read_excel("BP_Flats.xlsx")
str(BP_Flats)


bigmodel <- lm(Price_MillionHUF ~., data = BP_Flats)
summary(bigmodel)

# save the coefficients
BetaTable <- summary(bigmodel)$coefficients
BetaTable[order(BetaTable[,4]),]

# confinterval
confint(bigmodel)
confint(bigmodel, level = 0.99) # but it can be anything like 99%

# ------------------------------------------------------------------------------
# CHAPTER 16 : Model Selection

ind <- read.csv("industry.csv")
str(ind)

# Estimate Earnings with with all the variables
model_1 <- lm(Earnings ~ ., data = ind)
summary(model_1)

# only look at significant p val < 0.05
model_2 <- lm(Earnings ~ Workers + CAssets + LTLiab + 
                STLiab + MatExp + Dep, data = ind)
summary(model_2)

# Information Criteria
# ordered by strenght AIC<BIC<WALD
# the lower the value, the better

AIC(model_1, model_2) # model 1 wins
BIC(model_1, model_2) # model 1 wins

# since we know that we will always lose some R^2, we now only decide with hyp test.
# wheter the loss of R^2 is significant

# Wald test
anova(model_1, model_2) # H0 rejected --> there is at least one sign. variable dropped

# LM test: we can only do it manually
model_1_sum <- summary(model_1)
model_2_sum <- summary(model_2)

m <- length(coef(model_1))-length(coef(model_2)) # # of dropped variables
q <- length(coef(model_2)) # # of variables left in the model +1 because of the constant
n <- nrow(ind)

Rsq_1 <- model_1_sum$r.squared #Unrestricted

Rsq_2 <- model_2_sum$r.squared #Restricted

TestFunction <- (n-q-m)*(Rsq_1-Rsq_2)/(1-Rsq_2)

# p-value
1-pchisq(TestFunction,m)



# Model selection Trick:
# unify variables:
model_3 <- lm(Earnings ~ NrComps + Workers + FAssets + CAssets +
                Equity + Liab + MatExp + PersExp + Dep, data = ind)
summary(model_3)

AIC(model_1, model_3) # model 1 wins
BIC(model_1, model_3) # model 3 wins
anova(model_1, model_3)

model_4 <- lm(Earnings ~ NrComps + Workers + CAssets +
                Equity + Liab + MatExp + Dep, data = ind)
summary(model_4)

AIC(model_3, model_4)
BIC(model_3, model_4)
anova(model_3, model_4)

# ------------------------------------------------------------------------------
# CHAPTER 17: NON-LINEAR MODEL SPECIFICATIONS
library(readxl)
household <- readxl::read_excel("Household.xlsx")

household$Muni <- as.factor(household$Muni)

household$Muni <- relevel(household$Muni, ref = "Municipality")

base_model <- lm(ExpTot ~ IncTFt + Muni, data = household)
summary(base_model)

library(sjPlot)
plot_model(base_model, type = "pred", terms = c("IncTFt", 
                                                "Muni"))
           
library(ggplot2)
ggplot(data = household, aes(x = IncTFt, y = ExpTot, color =Muni)) +
  geom_point() +
  stat_smooth(method=lm)

int_model <- lm(ExpTot ~ IncTFt + Muni + Muni*IncTFt, data = household)
summary(int_model)
# itt megnézni az interpretációkat az együtthatóknál


anova(int_model, base_model)

plot_model(int_model, type = "pred", terms = c("IncTFt", "Muni"))

# other non-linear effects
ggplot(data = household, aes(x = IncTFt, y = ExpPle)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red")


quadratic_model <- lm(ExpPle ~ NrMems + NrRstrMeals + 
                        NrSmokers + NrCoffee + Gender + HhEd +
                        IncTFt + I(IncTFt^2) + NrSmokers*IncTFt +
                        NrCoffee*NrSmokers, data = household)
summary(quadratic_model)


# RESET TEST: 
# H0: we dont need non-linearity to be added
# H1: we do need

install.packages("lmtest")
library(lmtest)           

resettest(base_model) # fail to reject H0
resettest(int_model) # fail to reject H0
# we need non- linearity


# LOG_LOG Model
library(readxl)
ind <- readxl::read_excel("industry_filtered.xlsx")
str(ind)

ggplot(ind, aes(x=FAssets, y=NTurnover)) +
  geom_point() + geom_smooth(method = lm)

ggplot(ind, aes(x=log(FAssets), y=log(NTurnover))) +
  geom_point() + geom_smooth(method = lm)

loglog <- lm(log(NTurnover) ~ log(FAssets) + log(CAssets) + log(Liab) + log(PersExp) + log(Dep),
             data = ind)
summary(loglog) 

# calulating estimated revenue (y hat) for the first row
estimate <- predict(loglog, newdata = ind[1,])
# to the power of e because of the logarithm
estimate <- exp(estimate)

# saving beta of current assets
beta_cassets <- loglog$coefficients[3] # including the axis section it's the third coefficient

# marginal effect:
beta_cassets * estimate/ind$CAssets[1]

# constant return hypothesis:
#  if all factors of production in the production function are increased by 1%, 
# then production is expected to increase by 1%
sum_beta <- sum(loglog$coefficients[2:6])
sum_beta # 1.03

VarCov <- vcov(loglog)
VarCov

SE <- sqrt(sum(VarCov[-1,-1])) # -1 refers to the removal of values connected to the intercept
TestStat <- (sum_beta - 1)/SE

n <- nrow(ind)
p <- length(loglog$coefficients) # parameters = number of betas
deg_of_freedom <- n-p

pt(-abs(TestStat), deg_of_freedom)*2

# LOG_LIN Model
hh_filtered <- read_excel("Household_filtered.xlsx")
str(hh_filtered)

hist(hh_filtered$HousePrice)
hist(hh_filtered$HhEd)
hist(hh_filtered$HhAge)

# connection of HhEd and IncTFt
ggplot(hh_filtered, aes(x = HhEd, y = IncTFt)) + geom_point() + geom_smooth(method = "lm")

ggplot(hh_filtered, aes(x=HhEd, y=log(IncTFt))) +
  geom_point() + geom_smooth(method = lm)

# connection of HhAge and IncTFt
ggplot(hh_filtered, aes(x=HhAge, y=IncTFt)) +
  geom_point() + geom_smooth(method = lm)

ggplot(hh_filtered, aes(x=HhAge, y=log(IncTFt))) +
  geom_point() + geom_smooth()

# we need to square the age variable
loglin <- lm(log(IncTFt) ~ NrMems + HhEd + HhAge + I(HhAge^2), data = hh_filtered)
summary(loglin)

# interpretation of the beta of HhAge
exp(loglin$coefficients[3])
# All else being unchanged, if the education level of the head of household increases by one year, 
# his or her income is expected to change by a factor of 1.077, or 7.7%


# looking at marginal effect of age
# take a 40 year old head of household 
exp(loglin$coefficients[4]+loglin$coefficients[5]*2*40)
# if the age of the head of household increases ceteris paribus by 1 year compared to 40, 
# then income is expected to change by a factor of 1.0007, i.e. increase by 0.07%

# Mixed model
ggplot(hh_filtered, aes(x=log(HousePrice), y=log(IncTFt))) +
  geom_point() + geom_smooth(method = lm)

mixed <- lm(log(IncTFt) ~ NrMems + HhEd + HhAge + I(HhAge^2) + log(HousePrice), data=hh_filtered)
summary(mixed)


# LIN-LOG MODEL
BP_Flats <- readxl::read_excel("BpFlats_New.xlsx")
str(BP_Flats)

ggplot(BP_Flats, aes(x=log(area), y=price)) +
  geom_point() + geom_smooth(method = lm)

linlog <- lm(price ~ log(area), data=BP_Flats)
summary(linlog)


# RECIPROCAL MODEL
IMRData <- read.csv("IMR.csv")
str(IMRData)

ggplot(IMRData, aes(x=GDP, y=IMR)) +
  geom_point() + geom_smooth()

reciprocal <- lm(IMR ~ I(1/GDP), data = IMRData)
summary(reciprocal)

# estimated imr = 12.362 + 61171.207/GDP
# we need partail derivative = - 61171.207/(GDP)^2 = -0.00023


# elasticity = marginal effect * (x/y(est))
#for example for Yemen: GDP = 645
# y(est) = 12.362 + 61171.207/645 = 107.2

# elasticity = - 61171.207/(645*107.2)


# ------------------------------------------------------------------------------
# CHAPTER 18: Multicollinearity and Principal Component Analysis
library(readxl)
FAT <- read_excel("FAT_Data.xlsx")
str(FAT)

# fat is explained by every numerical varriable
model_1 <- lm(Fat ~ ., data = FAT)
summary(model_1)

# lot of extreme correlation
install.packages("car")
library(car)

vif(model_1)
# The resulting 𝑉𝐼𝐹𝑗 indicator values show how many times 
# the squared standard error, i.e. the variance of the coefficient estimate, 
# of 𝑋𝑗 has increased just because of multicollinearity

TolWeight <- 1/vif(model_1)[2]
1-TolWeight # Weight is 97% explained by the other explanatory variables. !!!!!!

# Solution --> PCA
full_pc_analysis <- prcomp(FAT[,2:15], scale = TRUE, center = TRUE)
summary(full_pc_analysis)

# adding the principal components to the dataframe
FAT <- cbind(FAT, full_pc_analysis$x[,1:3])
str(FAT)

# look at the corr. matrix
Corr <- cor(FAT[, 2:ncol(FAT)])
library(corrplot)
corrplot(Corr, method="color")

# removing current PC1-3 from the data frame
FAT[,c("PC1", "PC2", "PC3")] <- NULL

# new PCs without Age --> not correlated with anything --» remove
full_pc_analysis <- prcomp(FAT[,3:15], center = TRUE, scale=TRUE)
summary(full_pc_analysis)

FAT <- cbind(FAT, full_pc_analysis$x[,1:2])
Corr <- cor(FAT[,2:ncol(FAT)])
corrplot(Corr, method="color")


# 4. Principal Components in OLS Regression
model_2 <- lm(Fat ~ Age + PC1 + PC2, data = FAT)
summary(model_2)

vif(model_2)
anova(model_1, model_2)

# ------------------------------------------------------------------------------
# CHAPTER 19: Visualizing Heteroskedasticity
library(readxl)
household <- read_excel("Household.xlsx")

base_model <- lm(ExpPle ~ NrMems + NrRstrMeals + NrSmokers + NrCoffee +
                   Gender + HhEd + IncTFt, data = household)
summary(base_model)

household$sq_errors <- base_model$residuals^2

library(ggplot2)
ggplot(household, aes(x = ExpPle, y = sq_errors)) + geom_point()
# heteroscedasticity: when the squares of the forecast errors, 
# i.e., the residual squares 𝜖2𝑖=(𝑦𝑖−𝑦̂𝑖)2, are not random
# but depend on the actual values of the dependent variable (𝑦)!! 

# WHITE TEST
# H0 : there is no hetereosked. in the model: squares of the residuals (𝜖𝑖) are not well explaine
# H1 : there is

install.packages("skedastic")
library(skedastic)

white_basic <- white(base_model, interactions = FALSE)
white_basic$statistic
white_basic$p.value
# H0 rejected --> heterosked.

white_inter <- white(base_model, interactions = TRUE)
# test statistic
white_inter$statistic
white_inter$p.value

# if the square-only White test rejects 𝐻0, 
# then the interaction-based White test will definitely reject it too!

# The Breusch-Pagan Test
library(lmtest)
bptest(base_model, studentize = TRUE) # the parameter 'studentize' turns the Koenker correction on
bptest(base_model, studentize = FALSE) # the parameter 'studentize' turns the Koenker correction off

# kolmogorov smirov test: more like to check whether the distr. fits
ks.test(base_model$residuals, "pnorm")



#  Addressing Heteroskedasticity
library(car)
coeftest(base_model)
coeftest(base_model, vcov = hccm(base_model))

# Estimating Coefficients with Generalized Least Squares (GLS)
table_white_test <- household[,2:8]
table_white_test$sq_errors <- household$sq_errors

helper_reg_gls <- lm(log(sq_errors) ~ ., data = table_white_test) 
elements_of_omega <- exp(fitted(helper_reg_gls)) # when predicting, we need to give the 'log' back with an 'exp'

base_model_gls <- lm(ExpPle ~ NrMems + NrRstrMeals + NrSmokers + NrCoffee +
                       Gender + HhEd + IncTFt, weights = 1/elements_of_omega, data = household)
summary(base_model_gls)

# eteroscedasticity is actually caused 
# by the long right tail distribution of the outcome variable
hh_positive <- household[household$ExpPle > 0 & household$IncTFt > 0,]

base_model_log <- lm(log(ExpPle) ~ NrMems + NrRstrMeals + NrSmokers + NrCoffee +
                       Gender + HhEd + log(IncTFt), data = hh_positive)
summary(base_model_log)

hh_positive$sq_errors_log <- base_model_log$residuals^2
ggplot(hh_positive, aes(x=ExpPle, y=sq_errors_log)) + geom_point()

white_inter_log <- white(base_model_log, interactions = TRUE)
# test statistic
white_inter_log$statistic
white_inter_log$p.value


white_inter$p.value

         