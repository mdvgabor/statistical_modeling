setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
# --- 1. Data ----
library(readxl)
library(ggplot2)
library(corrplot)
library(car)
filename = "HouseData.xlsx"
houses <- read_excel(filename)
houses[houses$ocean_proximity %in% c('NEAR BAY', 'NEAR OCEAN', 'ISLAND'),]$ocean_proximity <- 'NEAR WATER'
houses$ocean_proximity <- as.factor(houses$ocean_proximity)
houses <- houses[,colnames(houses)[3:10]]

houses_clean <- na.omit(houses) # null values out
c(nrow(houses), nrow(houses_clean)) # we will use cleaned

# NEAR WATER category

# INLAND : Cheapest --> relevel to theat basis
houses_clean$ocean_proximity <- relevel(houses_clean$ocean_proximity, ref = "INLAND")

# ---- 1. Plots ----
# target variable: median_house_value

# ---- 1.1. Median house value ----
ggplot(data = houses_clean, aes(x = median_house_value)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(
      mean = mean(houses_clean$median_house_value, na.rm = TRUE), 
      sd = sd(houses_clean$median_house_value, na.rm = TRUE)
    ),
    color = "red", size = 1
  ) # long right-tail
hist(houses_clean$median_house_value) # long right tail

ggplot(data = houses_clean, aes(x = log(median_house_value))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(
      mean = mean(log(houses_clean$median_house_value), na.rm = TRUE), 
      sd = sd(log(houses_clean$median_house_value), na.rm = TRUE)
    ),
    color = "red", size = 1
  ) # looks really good
hist(log(houses_clean$median_house_value))

# ---- 1.2. Median house age ----
ggplot(data = houses_clean, aes(x = housing_median_age)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(
      mean = mean(houses_clean$housing_median_age, na.rm = TRUE), 
      sd = sd(houses_clean$housing_median_age, na.rm = TRUE)
    ),
    color = "red", size = 1
  )
hist(houses_clean$housing_median_age) # nearly symmetric, idk distr

# ---- 1.3. Median income ----
ggplot(data = houses_clean, aes(x = median_income)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(houses_clean$median_income, na.rm = TRUE), 
               sd = sd(houses_clean$median_income, na.rm = TRUE)),
    #fun = dchisq, 
    #args = list(df = mean(houses_clean$median_income)),
    color = "red", size = 1
  ) # long right-tail, normal maybe fits better than Chisq but hard question
hist(houses_clean$median_income) # long right tail

ggplot(data = houses_clean, aes(x = log(median_income))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(log(houses_clean$median_income), na.rm = TRUE), 
                sd = sd(log(houses_clean$median_income), na.rm = TRUE)),
    color = "red", size = 1
  ) # normal
hist(log(houses_clean$median_income)) # normal

# ---- 1.4. Total bedrooms ----
ggplot(data = na.omit(houses_clean[, "total_bedrooms"]), aes(x = total_bedrooms)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    #fun = dnorm, 
    #args = list(mean = mean(na.omit(houses_clean[, "total_bedrooms"])$total_bedrooms, na.rm = TRUE), 
    #            sd = sd(na.omit(houses_clean[, "total_bedrooms"])$total_bedrooms, na.rm = TRUE)),
    fun = dexp, 
    args = list(rate = 1/mean(na.omit(houses_clean[, "total_bedrooms"])$total_bedrooms)),
    color = "red", size = 1
  ) # quite exponential --> long right-tail
hist(houses_clean$total_bedrooms) # right tail

ggplot(data = na.omit(houses_clean[, "total_bedrooms"]), aes(x = log(total_bedrooms))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(log(na.omit(houses_clean[, "total_bedrooms"])$total_bedrooms), na.rm = TRUE), 
                sd = sd(log(na.omit(houses_clean[, "total_bedrooms"])$total_bedrooms), na.rm = TRUE)),
    color = "red", size = 1
  )
hist(log(houses_clean$total_bedrooms)) # bit better

# ---- 1.5. Total rooms ----
ggplot(data = na.omit(houses_clean[, "total_rooms"]), aes(x = total_rooms)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dexp, 
    args = list(rate = 1/mean(na.omit(houses_clean[, "total_rooms"])$total_rooms)),
    color = "red", size = 1
  ) # quite exponential --> long right-tail
hist(houses_clean$total_rooms) # right tail

ggplot(data = na.omit(houses_clean[, "total_rooms"]), aes(x = log(total_rooms))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(log(na.omit(houses_clean[, "total_rooms"])$total_rooms), na.rm = TRUE), 
                sd = sd(log(na.omit(houses_clean[, "total_rooms"])$total_rooms), na.rm = TRUE)),
    color = "red", size = 1
  ) # same as 1.4. Bedrooms
hist(log(houses_clean$total_bedrooms)) # bit better


# ---- 1.6. Population ----
ggplot(data = na.omit(houses_clean[, "population"]), aes(x = population)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dexp, 
    args = list(rate = 1/mean(na.omit(houses_clean[, "population"])$population)),
    color = "red", size = 1
  ) 
hist(houses_clean$population) # long right tail

ggplot(data = na.omit(houses_clean[, "population"]), aes(x = log(population))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(log(na.omit(houses_clean[, "population"])$population), na.rm = TRUE), 
                sd = sd(log(na.omit(houses_clean[, "population"])$population), na.rm = TRUE)),
    color = "red", size = 1
  ) 
hist(log(houses_clean$population)) # better

# plots at 1.4 and 1.6 are really similar

# ---- 1.7 Boxplots ----
# box plot on the median house value based on ocean prox.
ggplot(houses_clean, aes(x = ocean_proximity, y = median_house_value, fill = ocean_proximity)) +
  geom_boxplot()

# histogram on the same thing
ggplot(houses_clean, aes(x = median_house_value, fill = ocean_proximity)) +
  geom_histogram()

# ---- 2. Modeling ----
# ---- 2.1. Build basic model ----
# model 1: all variables are included
model1 <- lm(median_house_value ~ ., data = houses_clean)
summary(model1)
# all variables are significant

# ---- 3.2. Correlation ----
# lets check correlation
CorrMatrix <- cor(houses_clean[, 1:(ncol(houses_clean)-1)])
corrplot(CorrMatrix, method = "color")

# lets check multicollinearity
vif(model1)
test <- 1/vif(model1)[3] # test with total bedrooms
1-test # 97% of households is explained by the other expl. variables
# disgusting amount of multicollinearity

# ---- 2.3. PCA ----
# only numeric columns AND no null values
# ITT HIÁNYZOTT AZ 1 VALAMIÉRT
full_pc_analysis <- prcomp(houses_clean[, c(1,2,3,4,5,6,7)], scale = TRUE, center = TRUE)
summary(full_pc_analysis)
# PC 1-3 have SD > 1 and explain 92% of variance

# Adding PC-s to the dataframe
houses_clean <- cbind(houses_clean, full_pc_analysis$x[, 1:2])
str(houses_clean)

# look at the corr. matrix
# make the target var the first
houses_clean <- houses_clean[, c("median_house_value", setdiff(names(houses_clean), "median_house_value"))] 
Corr <- cor(houses_clean[, c(2,3,4,5,6,7,9,10)])
corrplot(Corr, method = "color")


# ---- 3. Unifying variables ----
# total_bedrooms = total_rooms + population + households
# többit a vif nézegetésével egyesével kibaszogattam amíg ki nem jött 1 körüli eredmény

model2 <- lm(median_house_value ~ ocean_proximity + housing_median_age + 
             + PC1 + PC2, data = houses_clean)
vif(model2)
summary(model2)
anova(model1,model2)
AIC(model1, model2)
BIC(model1, model2)
# model 2 is better

model3 <- lm(median_house_value ~ ocean_proximity + 
               + PC1 + PC2, data = houses_clean)
vif(model3)
summary(model3)
anova(model2,model3)
AIC(model2, model3)
BIC(model2, model3)
# szar

model4 <- lm(median_house_value ~ ocean_proximity + 
               + PC1 + median_income, data = houses_clean)
vif(model4)
summary(model4)
anova(model2,model4)
AIC(model2, model4)
BIC(model2, model4)
# szar

# ---- 4. Heteroskedasticity ----
# install.packages("skedastic")
library(skedastic)

# ---- 4.1. White test ----
# H0 : there is no heterosked. in the model: squares of the residuals (𝜖𝑖) are not well explained
# H1 : there is heterosked.

white_test <- white(model2, interactions = TRUE)
white_test$p.value # reject H0 --> there is hetereosked. in the model

# Addressing heterosked.
library(car)
houses_clean$sq_errors <- model2$residuals^2
table_white_test <- houses_clean[, c("ocean_proximity", "PC1", "PC2")]
table_white_test$sq_errors <- houses_clean$sq_errors

ggplot(houses_clean, aes(x=median_house_value, y=sq_errors)) + geom_point() + geom_smooth()

# ---- 4.2. Breusch-Pagan Test ----
library(lmtest)
hist(model2$residuals)
mean_resid <- mean(model2$residuals)
sd_resid <- sd(model2$residuals)
norm_quintiles <- qnorm(c(0,0.2,0.4,0.6,0.8,1), mean=mean_resid, sd=sd_resid)
observed_freq <- table(cut(model2$residuals, breaks=norm_quintiles))
chi2_result <- chisq.test(observed_freq)
chi2_result$p.value
  # H0 rejected --> not normally distr
  # Koenker correction is needed

bptest(model2, studentize = TRUE)
# H0 rejected --> heterosked

# ---- 5. GLS method -----
helper_reg_gls <- lm(log(sq_errors) ~ ., data = table_white_test)
elements_of_omega <- exp(fitted(helper_reg_gls))

model2_gls <- lm(median_house_value ~ ocean_proximity + housing_median_age + 
                       + PC1 + PC2, data = houses_clean, weights = 1/elements_of_omega)

model2_gls_log <- lm(log(median_house_value) ~ ocean_proximity + housing_median_age + 
                   + PC1 + PC2, data = houses_clean, weights = 1/elements_of_omega)

summary(model2_gls_log)
AIC(model1, model2_gls_log)
BIC(model1, model2_gls_log)
AIC(model1, model2_gls_log)$AIC[1]/AIC(model1, model2_gls_log)$AIC[2]
BIC(model1, model2_gls_log)$BIC[1]/BIC(model1, model2_gls_log)$BIC[2]
 # jóvanazúgy

# ---- 5.1. Reset test ----
# H0: we dont need non-linearity to be added
# H1: we do need
library(lmtest)
resettest(model2)
resettest(model2_gls_log) #p-value < 2.2e-16 --> H0 reject --> we need non-linear terms

model2_gls_log2 <- lm(log(median_house_value) ~ ocean_proximity + housing_median_age + 
                         + PC1*(ocean_proximity) + PC2*ocean_proximity, data = houses_clean, weights = 1/elements_of_omega)

resettest(model2_gls_log2)

# model2_gls_log is the best we can do

# check p values again
houses_clean$sq_errors_log <- model2_gls_log$residuals^2
white_gls_log <- white(model2_gls_log, interactions = TRUE)
white_gls_log$p.value

white_base <- white(model2_gls, interactions = TRUE)
white_base$p.value

c(white_base$p.value, white_gls_log$p.value)

summary(model2_gls_log)
BIC(model2_gls_log)

ggplot(data = houses_clean, aes(x = PC1, y = median_house_value)) +
  geom_point() + geom_smooth(method = lm)

ggplot(data = houses_clean, aes(x = PC2, y = median_house_value)) +
  geom_point() + geom_smooth(method = lm)

ggplot(data = houses_clean, aes(x = housing_median_age, y = median_house_value)) +
  geom_point() + geom_smooth(method = lm)

model5 <- lm(data = houses_clean, median_house_value ~ PC2)
summary(model5)
BIC(model5)

model6 <- lm(data = houses_clean, median_house_value ~ PC2+ocean_proximity)
summary(model6)

anova(model5,model6)
# model6 win

# equation
# y_^ = 75607.5 *PC2 + 34294.6*<1H OCEAN + 43399.7 *NEAR WATER  + 181287.8 
# (y^= est. median_house_value)







