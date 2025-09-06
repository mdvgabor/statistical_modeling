setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

#Generate a population from exponential distribution for which the value of lambda is
#0.02! Calculate the theoretical expected value and standard deviation! Compare them to
#the observed values! (2 points)

#Take 100 IID samples (with replacement) from the population given in a). The number
#of observations is 100 in each! Calculate the sample mean for each of the samples!
#Depict the histogram of the sample means! What is your conclusion? (1.5 points)

#Calculate the 95% confidence intervals of the means for the samples! (1 point)

#Calculate the ratio of the samples in which the confidence interval covers the population
#mean! How does it connect to the confidence level? (1.5 points)

#-------------------------------------------------------------------------------
set.seed(17)
n <- 10000
lambda <- 0.02
population <- rexp(n = n, rate = lambda)

#theoretical values
th_mean <- 1/lambda
th_std <- sqrt(1/lambda^2)

#observed values
obs_mean <- mean(population)
obs_std <- sd(population)

# comparison
sampling_error_mean <- obs_mean - th_mean
sampling_error_std <- obs_std - th_std
#-------------------------------------------------------------------------------

#Take 100 IID samples (with replacement) from the population given in a). The number
#of observations is 100 in each! Calculate the sample mean for each of the samples!
#Depict the histogram of the sample means! What is your conclusion? (1.5 points)

class_var <- function(x){
  pop_variance <- mean((x-mean(x))^2)
  return(pop_variance)
}

PopMean <- mean(population)
PopVariance <- class_var(population)
samples <- as.numeric(sample(population, size = 100, replace = TRUE))
samples <- as.data.frame(t(samples))  # make it a row

for (i in 2:100) {
  set.seed(17 + i)
  new_sample <- as.numeric(sample(population, size = 100, replace = TRUE))
  samples <- rbind(samples, new_sample)
}

colnames(samples) <- paste0("Observation", 1:100)
rownames(samples) <- paste0("Sample", 1:100)


samples$means <- apply(samples, 1, mean)
hist(samples$means) #nearly normally distr, if we increased n it would get closer
#-------------------------------------------------------------------------------

#Calculate the 95% confidence intervals of the means for the samples! (1 point)

SE_mean <-sqrt(PopVariance) / sqrt(100)
alpha <- 1-0.95
k <- qt(1-alpha/2, df = n-1)

samples$mean_low <- samples$means - SE_mean*k
samples$mean_up <- samples$means + SE_mean*k

#-------------------------------------------------------------------------------
#Calculate the ratio of the samples in which the confidence interval covers the population
#mean! How does it connect to the confidence level? (1.5 points)

samples$iscovered <- (PopMean >= samples$mean_low) & (PopMean <= samples$mean_up)
covered_ratio <- mean(samples$iscovered)
covered_ratio

#-------------------------------------------------------------------------------
#Estimate the 90% confidence interval of the proportion of the houses with excellent
#condition! (1 point)
#b) Estimate the 98% confidence interval of the average price for each town! Depict a bar
#chart of the mean prices with the confidence intervals! (2 points)
#c) Test the following statement: the average price is more than 25 million HUF!
#  (alpha=0.05) Calculate the p-value manually! (2 points)
#d) According to a survey, 48% of properties for sale are larger than 100 square metres.
#Test that fact in this sample! (alpha=0.05) Calculate the p-value manually! (2 points)
#e) We assume that, the properties are cheaper on average in Abony compared to Aszód.
#Test that assumption! (alfa=0.05) Calculate the p-value manually! (2 points)
#-------------------------------------------------------------------------------

#Estimate the 90% confidence interval of the proportion of the houses with excellent
#condition! (1 point)
Houses <- readxl::read_excel("PracticeData.xlsx")
Houses$Excellent <- ifelse(Houses$Condition =="excellent",1,0)

library(rcompanion)
groupwiseMean(Excellent  ~ 1, data = Houses, na.rm = TRUE, conf = 0.9, digits=5)
#-------------------------------------------------------------------------------
#b) Estimate the 98% confidence interval of the average price for each town! Depict a bar
#chart of the mean prices with the confidence intervals! (2 points)

town_means <- groupwiseMean(Price ~ Town, data = Houses, na.rm = TRUE, conf = 0.98, digits = 5)

library(ggplot2)
ggplot(data = town_means, mapping = aes(x = Town, y = Mean, fill = Town)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper))

#-------------------------------------------------------------------------------

#c) Test the following statement: the average price is more than 25 million HUF!
#  (alpha=0.05) Calculate the p-value manually! (2 points)

#H0 mu=25000 (<=)
#H1 mu>25000
#
sample_mean <- mean(Houses$Price)
sample_sd <- sd(Houses$Price)
alpha <- 1-0.95
n <- length(Houses$Price)

test_stat_mean <- (sample_mean-25000) / (sample_sd/sqrt(n))
p_value_mean <- 1-pt(test_stat_mean, df = n-1) #0.08 inconclusive test

#-------------------------------------------------------------------------------

#d) According to a survey, 48% of properties for sale are larger than 100 square metres.
#Test that fact in this sample! (alpha=0.05) Calculate the p-value manually! (2 points)

#H0: p = 0.48
#H1 p != 0.48

Houses <- readxl::read_excel("PracticeData.xlsx")

P_0 <- 0.48
p <- mean(Houses$Size > 100)
n <- nrow(Houses)
SE_P = sqrt(P_0*(1-P_0)/n)
test_stat <- (p - P_0)/SE_P

pnorm(-abs(test_stat))*2

#result <- pnorm(-abs(test_stat))*2
#-------------------------------------------------------------------------------

#e) We assume that, the properties are cheaper on average in Abony compared to Aszód.
#Test that assumption! (alfa=0.05) Calculate the p-value manually! (2 points)

#H0 Aszód_mean - Abony_mean = 0 (>=0)
#H1 Aszód_mean - Abony_mean < 0

delta_0 <- 0

#quick check
groupwiseMean(Price ~ Town, data = Houses, conf = 0.95, digits = 5)

observed_diff <- mean(Houses$Price[Houses$Town == "Aszód"], na.rm = TRUE) -
  mean(Houses$Price[Houses$Town == "Abony"], na.rm = TRUE)

# test_stat = (observed_diff - delta_0)/SE_Unified
# SE_Unified = sqrt(SE_1^2 + SE_2^2)
# SE for the mean = corr st dev / sqrt(n)


sd_Aszód <- sd(Houses$Price[Houses$Town == "Aszód"], na.rm = TRUE)
sd_Abony <- sd(Houses$Price[Houses$Town == "Abony"], na.rm = TRUE)

n_Aszód <- sum(!is.na(Houses$Price[Houses$Town == "Aszód"]))
n_Abony <- sum(!is.na(Houses$Price[Houses$Town == "Abony"]))

SE_unified <- sqrt(sd_Aszód^2/n_Aszód + sd_Abony^2/n_Abony)

test_stat_difference <- (observed_diff-delta_0) / SE_unified  

p_value_diff <- 1-pt(test_stat_difference, df = n_Aszód + n_Abony - 2)  
                                                #assuming equal sd for the groups

#-------------------------------------------------------------------------------

Houses_sub <- subset(Houses, Town %in% c("Aszód", "Abony"))

t.test(Price ~ Town, data = Houses_sub, alternative = "less")
  