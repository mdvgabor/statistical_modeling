setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

library(readxl)
ESS <- read_excel("ESS2020.xlsx")

# 97% confidence for std of the netusedata
NetUseData <- ESS$NetUsePerDay_Minutes[!is.na(ESS$NetUsePerDay_Minutes)]
n <- length(NetUseData)
s <- sd(NetUseData)
alpha <- 1-0.97

L <- sqrt((n-1)*s^2 / qchisq(1-alpha/2, df = n-1))
U <- sqrt((n-1)*s^2 / qchisq(alpha/2, df = n-1))

#conf int.
c(L,U)
# st deviation of daily internet usage usage time in the Hungarian Population
# is between 139 and 152 minutes with a prob of 97%

# ASSUMPITON: variable we are examining is normally distributed
hist(NetUseData) # long right tail :(
# confidence is NOT 97% but something lower


#BOOTSTRAP
set.seed(17)
boot_subsamples <- sample(NetUseData, size = length(NetUseData), replace = TRUE)

for (i in 1:9999) {
  set.seed(17+i)
  boot_subsamples <- rbind(boot_subsamples, sample(NetUseData, size = length(NetUseData), replace = TRUE))
}

boot_subsamples <- as.data.frame(boot_subsamples)
colnames(boot_subsamples) <- paste0("Observation", 1:length(NetUseData))
rownames(boot_subsamples) <- paste0("Subsample", 1:nrow(boot_subsamples))

#estimators in every sample
boot_subsamples$means <- apply(boot_subsamples,1,mean)
boot_subsamples$corr_sd <- apply(boot_subsamples[1:1099],1,sd)

#classical sd function
classical_sd <- function(x) {
  result <- sqrt(mean((x-mean(x))^2))
  return(result)
}

SE_Mean_Boot <- classical_sd(boot_subsamples$means)
SE_Mean_Formula <- s/sqrt(n)

SE_sd <- classical_sd(boot_subsamples$corr_sd)

boot_ci_mean <- quantile(boot_subsamples$means, probs = c(alpha/2, 1-alpha/2))

k <- qnorm(1-alpha/2) # sample size is large (n>50)
formula_ci_mean <- c(mean(NetUseData)-SE_Mean_Formula*k,
                     mean(NetUseData)+SE_Mean_Formula*k)

boot_ci_sd <- quantile(boot_subsamples$corr_sd, probs = c(alpha/2,1-alpha/2))



