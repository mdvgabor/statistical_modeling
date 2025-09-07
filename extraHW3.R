setwd("YOUR WD HERE")

library(readxl)

# read in data
Heights <- readxl::read_excel("StudentHeight.xlsx")

# using the 2nd column as data
height_values <- Heights$Height

# number of values
n <- length(height_values)

# defining empty list and counter
samples <- list()
count <- 1  # start count at 1

# generate all possible samples
for (i in 1:(n - 2)) {
  for (j in (i + 1):(n - 1)) {
    for (k in (j + 1):n) {
      # getting the samples
      samples[[count]] <- c(height_values[i], height_values[j], height_values[k])
      count <- count + 1
    }
  }
}

# convert list to dataframe
samples_df <- do.call(rbind, samples)
samples_df <- as.data.frame(samples_df)

# renaming columns
colnames(samples_df) <- paste0("Observation", 1:3)
# setting row names
rownames(samples_df) <- paste0("Sample", 1:nrow(samples_df))

# BIAS = E(Estimator) - Parameter

# ------------------------------------------------------------------------------

# BIAS of the mean

# mean for each sample and adding it as a new column
samples_df$sample_mean <- apply(samples_df, 1, mean)

# calc. population mean
PopMean <- mean(height_values) #166.95

# expected value of the sample means
E_sample_means <- mean(samples_df$sample_mean)

# BIAS of the mean
BIAS_Mean <- E_sample_means - PopMean # 0 --> UNBIASED

# ------------------------------------------------------------------------------

# BIAS of the variance

# function to calculate variance
class_var <- function(x){
  pop_variance <- mean((x - mean(x))^2)
  return(pop_variance)
}

# calc. population variance
PopVariance <- class_var(Heights$Height) #230.6475

# calc. the variance for all samples
samples_df$sample_var <- apply(samples_df[1:3], 1, class_var)

# calc. the expected value of the sample variances
E_sample_vars <- mean(samples_df$sample_var)

# calc. the BIAS of the mean
BIAS_Variance <- E_sample_vars - PopVariance # -68.79 != 0 --> BIASED

# ------------------------------------------------------------------------------

sample_size <- 3 #n

# E(sample variances * n/(n-1)) = pop variance

samples_df$corrected_var <- samples_df$sample_var*3/2
BIAS_corrected_variance <- mean(samples_df$corrected_var) #we need every possible
                      #sample to have an exact match with the population variance

# ------------------------------------------------------------------------------

# Standard error for the mean

# SE = st_dev/sqrt(n)

# theoretical Standard Error of the sample mean
PopSD <- sqrt(PopVariance)
SE_theoretical <- PopSD / sqrt(sample_size)

# empirical Standard Error from the sampling distribution
SE_empirical <- sd(samples_df$sample_mean)

# difference between the two
SE_difference <- SE_theoretical - SE_empirical # not 0, 
#                                 but: higher 'n' = lower SE = lower margin of error



