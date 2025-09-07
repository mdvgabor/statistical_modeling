setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(ggplot2)

gamma_fn <- function(sample_size, shape, scale) {
  set.seed(17)
  sample <- rgamma(sample_size, shape = shape, scale = scale)
  
  # Theoretical statistics
  theoretical_mean <- shape * scale
  theoretical_sd <- sqrt(shape) * scale
  theoretical_median <- qgamma(0.5, shape = shape, scale = scale)
  theoretical_95th <- qgamma(0.95, shape = shape, scale = scale)
  
  # Empirical statistics
  empirical_mean <- mean(sample)
  empirical_sd <- sd(sample)
  empirical_median <- median(sample)
  empirical_95th <- quantile(sample, 0.95)
  
  gamma_df <- data.frame(Sample = sample)
  
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
    data = gamma_df,
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

# Generate gamma-distributed data
result <- gamma_fn(100000, shape = 5, scale = 2)

ggplot(data = result$data, mapping = aes(x = log(Sample))) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "blue") +
  stat_function(fun = function(x) dgamma(exp(x), shape = 5, scale = 2) * exp(x), 
                col = "red", size = 1.2) +
  labs(title = "Gamma Distribution (Log Scale)", x = "Log(Sample Values)", y = "Density") +
  theme_minimal()
