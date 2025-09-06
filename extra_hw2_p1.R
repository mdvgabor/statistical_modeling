setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(ggplot2)

lognormal_fn <- function(sample_size, mean, sd) {
  set.seed(17)
  sample <- rlnorm(sample_size, meanlog = mean, sdlog = sd)
  
  theoretical_mean <- exp(mean + ((sd^2) / 2))
  theoretical_sd <- sqrt((exp(sd^2) - 1) * exp(2 * mean + sd^2))
  theoretical_median <- exp(mean)
  theoretical_95th <- exp(mean + qnorm(0.95) * sd)
  
  empirical_mean <- mean(sample)
  empirical_sd <- sd(sample)
  empirical_median <- median(sample)
  empirical_95th <- quantile(sample, 0.95)
  
  lognormal_df <- data.frame(Sample = sample)
  
  cat("Theoretical Mean:", theoretical_mean, "\n")
  cat("Theoretical Standard Deviation:", theoretical_sd, "\n")
  cat("Theoretical Median:", theoretical_median, "\n")
  cat("Theoretical 95th Percentile:", theoretical_95th, "\n")
  cat("\n")
  cat("Empirical Mean:", empirical_mean, "\n")
  cat("Empirical Standard Deviation:", empirical_sd, "\n")
  cat("Empirical Median:", empirical_median, "\n")
  cat("Empirical 95th Percentile:", empirical_95th, "\n")
  
  return(list(
    data = lognormal_df,
    theoretical = list(
      mean = theoretical_mean,
      sd = theoretical_sd,
      median = theoretical_median,
      percentile_95 = theoretical_95th
    ),
    empirical = list(
      mean = empirical_mean,
      sd = empirical_sd,
      median = empirical_median,
      percentile_95 = empirical_95th
    )
  ))
}

result <- lognormal_fn(100000, mean = 1, sd = 2)

ggplot(data = result$data, mapping = aes(x = log(Sample))) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "blue") +
  stat_function(fun = function(x) dlnorm(exp(x), meanlog = 1, sdlog = 2) * exp(x), 
                col = "red", size = 1.2) +
  labs(title = "Lognormal Distribution (Log Scale)", x = "Log(Sample Values)", y = "Density") +
  theme_minimal()





