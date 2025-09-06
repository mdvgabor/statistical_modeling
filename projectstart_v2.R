# --- 1. Data ----
library(readxl)
library(ggplot2)
library(corrplot)
library(car)
filename = "California.xlsx"
houses <- read_excel(filename)
houses$ocean_proximity <- as.factor(houses$ocean_proximity)
houses <- houses[,colnames(houses)[3:10]]

houses_clean <- na.omit(houses) # null values out
c(nrow(houses), nrow(houses_clean)) # we will use cleaned

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
test <- 1/vif(model1)[7] # test with total bedrooms
1-test # 97% of households is explained by the other expl. variables
# disgusting amount of multicollinearity

# ---- 2.3. PCA ----
# only numeric columns AND no null values
full_pc_analysis <- prcomp(houses_clean[, c(1,2,3,4,5,6,7,8)], scale = TRUE, center = TRUE)
summary(full_pc_analysis)
# PC 1-3 have SD > 1 and explain 86% of variance

# Adding PC-s to the dataframe
houses_clean <- cbind(houses_clean, full_pc_analysis$x[, 1:3])
str(houses_clean)

# look at the corr. matrix
# make the target var the first
houses_clean <- houses_clean[, c("median_house_value", setdiff(names(houses_clean), "median_house_value"))] 
Corr <- cor(houses_clean[, c(2,3,4,5,6,7,8,9,11,12,13)])
corrplot(Corr, method = "color")

# ---- 3. Unifying variables ----
# total_bedrooms = total_rooms + population + households
# többit a vif nézegetésével egyesével kibaszogattam amíg ki nem jött 1 körüli eredmény

model2 <- lm(median_house_value ~ ocean_proximity
             + PC1 +PC2 +PC3, data = houses_clean)
vif(model2)
summary(model2)
anova(model1,model2)

# ---- 4. Heteroskedasticity ----
# install.packages("skedastic")
library(skedastic)

# ---- 5. White test ----
# H0 : there is no hetereosked. in the model: squares of the residuals (𝜖𝑖) are not well explained
# H1 : there is

white_test <- white(model2, interactions = FALSE)
white_test$p.value # reject H0 --> there is hetereosked. in the model

# Addressing heterosked.

library(car)
houses_clean$sq_errors <- model2$residuals^2
table_white_test <- houses_clean[, c("ocean_proximity", "PC1", "PC2", "PC3")]
table_white_test$sq_errors <- houses_clean$sq_errors

# ---- 6. GLS method -----
helper_reg_gls <- lm(log(sq_errors) ~ ., data = table_white_test)
elements_of_omega <- exp(fitted(helper_reg_gls))

model2_gls <- lm(median_house_value ~ ocean_proximity
                 + PC1 +PC2 +PC3, weights = 1/elements_of_omega, data = houses_clean)
summary(model2_gls)

AIC(model1, model2_gls)
BIC(model1, model2_gls)
(517759.9/513231.9)-1 # jóvanazúgy

# ---- 7. Reset test ----
# H0: we dont need non-linearity to be added
# H1: we do need
library(lmtest)
resettest(model2)
resettest(model2_gls) #p-value < 2.2e-16 --> H0 reject --> we need non-linear terms

model2_gls_log <- lm(
  log(median_house_value) ~ ocean_proximity * (PC1 + PC2 + PC3),
  weights = 1 / elements_of_omega,
  data = houses_clean
)

resettest(model2_gls_log)

# check p values again
houses_clean$sq_errors_log <- model2_gls_log$residuals^2
white_inter_log <- white(model2_gls_log, interactions = TRUE)
white_inter_log$p.value

white_base <- white(model2_gls, interactions = TRUE)
white_base$p.value

white_base$p.value / white_inter_log$p.value
# we still go with H1 on all common levels , BUT THE P VALUE DECREASED
# KURVAANYÁÁÁÁÁÁD








