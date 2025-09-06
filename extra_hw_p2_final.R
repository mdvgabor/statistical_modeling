setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(ggplot2)

gamma_fn <- function(sample_size,shape,scale) {
  set.seed(17)
  #sampling based on user input
  sample <- rgamma(sample_size, shape =shape, scale= scale)
  
  # theoretical measures
  theoretical_mean <- shape*scale
  theoretical_sd <- sqrt(shape)*scale
  theoretical_median <- qgamma(0.5,shape =shape,scale = scale)
  theoretical_95th <- qgamma(0.95, shape = shape, scale = scale)
  
  # empirical measures
  empirical_mean <- mean(sample)
  empirical_sd <- sd(sample)
  empirical_median <- median(sample)
  empirical_95th <- quantile(sample, 0.95)
  
  # creating the result table for theoretical and empirical values
  results_table <- data.frame(
    statistics = c("mean","sd", "median", "95th_perc"),
    theoretical = c(theoretical_mean,theoretical_sd, theoretical_median,theoretical_95th),
    empirical = c(empirical_mean, empirical_sd,empirical_median, empirical_95th)
  )
  print(results_table)
  
  # Create the histogram with logarithmizing the x values
  plot <- ggplot(data = data.frame(Sample = sample), mapping =aes(x = log(Sample))) +
    geom_histogram(aes(y = after_stat(density)), bins=100, fill = "blue", alpha = 0.5) +
    stat_function(fun = function(x) dgamma(exp(x), shape = shape, scale = scale)*exp(x), 
                  col = "red") +
    labs(title = "gamma distribution (logarithmized)", x = "log(sample x-s)", y = "density") +
    theme_minimal()
  
  print(plot) # calling the plot
  return(list(Data = results_table, Plot = plot))
}
# generateing data and the plot
result <- gamma_fn(100000, shape = 1, scale = 2)











