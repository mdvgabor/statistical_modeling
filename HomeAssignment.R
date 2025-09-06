setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

Houses <- readxl::read_excel("CaliHouses.xlsx")

#setting seed
set.seed(17)

#sample size
n <- 100
n_samples <- 500

# selected variable: median_house_value

# 500 IID samples of size 100
samples_iid <- sample(Houses$median_house_value, size = n, replace = TRUE)
for (i in 1:(n_samples-1)) {
  set.seed(17+i)
  samples_iid <- rbind(samples_iid,
                       sample(Houses$median_house_value, size = n, replace = TRUE))
}

samples_iid <- as.data.frame(samples_iid)
rownames(samples_iid) <- paste0("Sample", 1:n_samples)
colnames(samples_iid) <- paste0("Observation", 1:1:n)

# 500 SR random samples of size 100
set.seed(18)
samples_sr <- sample(Houses$median_house_value, size = n, replace = FALSE)
for (i in 1:(n_samples-1)) {
  set.seed(18+i)
  samples_sr <- rbind(samples_sr,
                      sample(Houses$median_house_value, size = n, replace = FALSE))
}
samples_sr <- as.data.frame(samples_sr)
rownames(samples_sr) <- paste0("Sample", 1:n_samples)
colnames(samples_sr) <- paste0("Observation", 1:n)

# 500 PS samples of size 100

# startification based on: ocean_proximity

# Get proportions and target size per group
# Stratified sampling (PS) by ocean_proximity
set.seed(19)
groups <- table(Houses$ocean_proximity)
proportions <- prop.table(groups)
sample_size <- 100
n_per_group <- round(proportions * sample_size)

# Adjust rounding if total != 100
diff <- sample_size - sum(n_per_group)
if (diff != 0) {
  max_group <- which.max(n_per_group)
  n_per_group[max_group] <- n_per_group[max_group] + diff
}

# Prepare to store samples
# number of samples to draw
n_samples <- 500

# empty matrix to store the sampled values
# rows: number of samples, columns: sample size
samples_ps <- matrix(nrow = n_samples, ncol = sample_size)

# names of the groups (strata) to sample from
group_names <- names(n_per_group)

# loop through the number of samples to generate
for (i in 1:n_samples) {
  
  # vector to store the sampled house values for this sample
  sample_rows <- c()
  
  # different random seed for each sample
  set.seed(19 + i)
  
  # loop through each group (stratum)
  for (group in group_names) {
    
    # filter the dataset for the current group
    group_data <- Houses[Houses$ocean_proximity == group, ]
    
    # number of samples to take from this group
    n_group <- n_per_group[group]
    
    # randomly sample row indices from the current group's data
    sampled_indices <- sample(1:nrow(group_data), n_group)
    
    # sampled median house values to sample_rows
    sample_rows <- c(sample_rows, group_data[sampled_indices, ]$median_house_value)
  }
  
  # collecting samples from all groups, storeing the result in the  matrix
  samples_ps[i, ] <- sample_rows
}
samples_ps <- as.data.frame(samples_ps)
rownames(samples_ps) <- paste0("Sample", 1:n_samples)
colnames(samples_ps) <- paste0("Observation", 1:sample_size)

#calculating the mean for each sample

samples_iid$means <- apply(samples_iid, 1, mean)
samples_sr$means <- apply(samples_sr,1,mean)
samples_ps$means <- apply(samples_ps,1,mean)

# ranking in efficiency: PS > SR > IID

# 5. Mean and StDev 
# and theoretical MSE
PopMean <- mean(Houses$median_house_value)
PopStdev <- sd(Houses$median_house_value)
N_pop <- sum(!is.na(Houses$median_house_value))
# since we are calculating MSE for Mean --> Mean has no Bias, MSE = SE^2
mse_iid_theoretical <- PopStdev^2 / n
se_iid_theoretical <- sqrt(mse_iid_theoretical)
correction_factor <- 1 - (n/N_pop)
mse_sr_theoretical <- mse_iid_theoretical * correction_factor
se_sr_theoretical <- sqrt(mse_sr_theoretical)

helper_table <- aggregate(median_house_value ~ ocean_proximity, data = Houses, FUN = mean)
helper_table$sd <- aggregate(median_house_value ~ ocean_proximity, data = Houses, FUN = sd)[,2]
helper_table$sample_size <- table(Houses$ocean_proximity)

within_sd <- sqrt(sum((helper_table$sample_size-1)*helper_table$sd^2)/(N_pop-1))

mse_ps_theoretical <- within_sd^2 / n * correction_factor
se_ps_theoretical <- sqrt(mse_ps_theoretical)


# 6. Calculate the empirical mean squared error for each of the three sampling 
# methods, defined as the variance of the sample means around the population mean.

mse_iid <- mean((samples_iid$means - PopMean)^2)
mse_sr <- mean((samples_sr$means - PopMean)^2)
mse_ps <- mean((samples_ps$means - PopMean)^2)


# 7. Calculate the empirical mean squared errors using bias and empirical standard error.
SE_iid_emp <- sd(samples_iid$means)
SE_sr_emp <- sd(samples_sr$means)
SE_ps_emp <- sd(samples_ps$means)

Bs_iid_emp <- mean(samples_iid$means) - PopMean
Bs_sr_emp <- mean(samples_sr$means) - PopMean
Bs_ps_emp <- mean(samples_ps$means) - PopMean

mse_iid_7 <- Bs_iid_emp^2 + SE_iid_emp^2
mse_sr_7 <- Bs_sr_emp^2 + SE_sr_emp^2
mse_ps_7 <- Bs_ps_emp^2 + SE_ps_emp^2

# 8. Analysis
# empirical vs theoretical SE
abs(1-(SE_iid_emp / se_iid_theoretical))
abs(1-(SE_ps_emp / se_ps_theoretical))
abs(1-(SE_sr_emp / se_sr_theoretical))

# relative efficiency
SE_sr_emp^2 / SE_iid_emp^2
SE_ps_emp^2 / SE_sr_emp^2
SE_ps_emp / SE_sr_emp

# How do the standard errors compare to the theoretical standard error of IID sampling?

SE_iid_emp / se_iid_theoretical
SE_sr_emp / se_iid_theoretical
SE_ps_emp / se_iid_theoretical

