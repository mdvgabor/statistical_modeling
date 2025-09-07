setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)
library(ggplot2)
library(corrplot)
library(sjPlot)
library(lmtest)
houses <- readxl::read_excel("HouseData.xlsx")
houses$total_bedrooms <- NULL

# target variable = median_house_value

# distribution of the target variable
# histogram
ggplot(houses, aes(x = median_house_value, fill = ocean_proximity)) +
  geom_histogram()

# boxplot
ggplot(houses, aes(x = ocean_proximity, y = median_house_value, fill = ocean_proximity)) +
  geom_boxplot()

# logarithmized median_house_value
ggplot(houses, aes(x = log(median_house_value))) +
  geom_histogram()

# correlation matrix: 
CorrMatrix <- cor(houses[,1:(ncol(houses)-1)])
#corrplot(CorrMatrix, method = "color")

# building the first model
# converting the ocean-proximity into factor
houses$ocean_proximity <- as.factor(houses$ocean_proximity)

# releveling to inland --> we will compare every change in price to inland
houses$ocean_proximity <- relevel(houses$ocean_proximity, ref = "INLAND")

# model_1
model_1 <- lm(median_house_value ~ ., data = houses)
summary(model_1)
# every variable is significant on all common significanc levels

# RESET TEST: 
# H0: we dont need non-linearity to be added
# H1: we do need
resettest(model_1)
# p-value < 2.2e-16 --> reject H0 --> we need non-linearity

# Examining the variables to logarithmize
# median income
ggplot(houses, aes(x = log(median_income), y = log(median_house_value))) + geom_point() +
  geom_smooth(method = lm) # ezt így

# total rooms
ggplot(houses, aes(x = total_rooms , y = log(median_house_value))) + geom_point() +
  geom_smooth(method = lm) 

# households
ggplot(houses, aes(x = log(households) , y = log(median_house_value))) + geom_point() +
  geom_smooth(method = lm) # ezt is kéne

# population 
ggplot(houses, aes(x = log(population) , y = log(median_house_value))) + geom_point() +
  geom_smooth(method = lm) # ezt is kéne

# median age
ggplot(houses, aes(x = housing_median_age , y = log(median_house_value))) + geom_point() +
  geom_smooth(method = lm) # ezt csak félig










