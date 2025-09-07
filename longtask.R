setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
Houses <- readxl::read_excel("PracticeData.xlsx")

#e) We assume that, the properties are cheaper on average in Abony compared to Aszód.
#Test that assumption! (alfa=0.05) Calculate the p-value manually! (2 points)

#mu_Abony < mu_Aszód --» mu_Abony - mu_Aszód < 0

# H0: mu_Abony - mu_Aszód = 0 (>=)
# H1: mu_Abony - mu_Aszód < 0

delta0 <- 0
obs_diff <- mean(Houses$Price[Houses$Town == "Abony"], na.rm = TRUE) - 
  mean(Houses$Price[Houses$Town == "Aszód"], na.rm = TRUE)

sd_Abony <- sd(Houses$Price[Houses$Town == "Abony"], na.rm = TRUE)
sd_Aszód <- sd(Houses$Price[Houses$Town == "Aszód"], na.rm = TRUE)

n_Abony <- sum(!is.na(Houses$Price[Houses$Town == "Abony"]))
n_Aszód <- sum(!is.na(Houses$Price[Houses$Town == "Aszód"]))

SE_unified <- sqrt(sd_Abony^2/n_Abony + sd_Aszód^2/n_Aszód)

test_stat <- (obs_diff-delta0)/SE_unified

pvalue <- pt(test_stat, df = 62.532)

#Houses_sub <- subset(Houses, Town %in% c("Abony", "Aszód"))
#t.test(Price ~ Town, Houses_sub, alternative = "less") #df = 62.532

