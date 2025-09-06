setwd("YOUR WD HERE")
library(ggplot2)

# NORMAL DISTRIBUTION

# parameters
n <- 50
mu <- 40
sigma <- 10
num_samples <- 20000  # number of samples

set.seed(17)
# first sample
samples <- rnorm(n = n, mean = mu, sd = sigma)

# Generate and rbind the remaining 19999 samples
for (i in 1:(num_samples - 1)) {
  set.seed(17 + i)
  samples <- rbind(samples, rnorm(n = n, mean = mu, sd = sigma))
}

# convert to data frame and add column and row names
samples <- as.data.frame(samples)
colnames(samples) <- paste0("Observation", 1:ncol(samples))
rownames(samples) <- paste0("Sample", 1:nrow(samples))

# sample variances row-wise
samples$vars <- apply(samples[, 1:n], 1, var)

# chi-squared ratios
samples$ratios <- (n - 1) * samples$vars / sigma^2

# plot
ggplot(data = samples, mapping = aes(x = ratios)) +
  geom_histogram(aes(y = after_stat(density)),bins = 50, fill = "pink", color = "purple") +
  stat_function(fun = dchisq, args = list(df = n-1), col = "blue", linewidth = 1) +
  labs(title = "Ratios follow a chi-squared distribution")


# EXPONENTIAL DISTRIBUTION

lambda <- 0.0125

# first sample
set.seed(17)
exp_samples <- rexp(n = n, rate = lambda)

# generate 19999 more samples
for (i in 1:(num_samples - 1)) {
  set.seed(17 + i)
  exp_samples <- rbind(exp_samples, rexp(n = n, rate = lambda))
}

# convert to data frame and add column and row names
exp_samples <- as.data.frame(exp_samples)
colnames(exp_samples) <- paste0("Observation", 1:ncol(exp_samples))
rownames(exp_samples) <- paste0("Sample", 1:nrow(exp_samples))

# sample variances row-wise
exp_samples$vars <- apply(exp_samples[, 1:n], 1, var)

# variance of Exp(λ) = 1/λ^2
PopVariance <- 1 / lambda^2

# chi-squared ratios
exp_samples$ratios <- (n - 1) * exp_samples$vars / PopVariance


# Plot
ggplot(data = exp_samples, mapping = aes(x = ratios)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "pink", color = "purple") +
  stat_function(fun = dchisq, args = list(df = n-1), col = "red", linewidth = 1) +
  labs(title = "Ratios DO NOT follow a chisquared distribution")




