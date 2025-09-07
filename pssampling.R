setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
Houses <- readxl::read_excel("CaliHouses.xlsx")
set.seed(17)

# Get proportions and target size per group
groups <- table(Houses$ocean_proximity)
proportions <- prop.table(groups)
sample_size <- 100
n_per_group <- round(proportions * sample_size)

# Adjust rounding if needed
diff <- sample_size - sum(n_per_group)
if (diff != 0) {
  max_group <- which.max(n_per_group)
  n_per_group[max_group] <- n_per_group[max_group] + diff
}

# Prepare to collect sample indices (or unique identifiers)
n_samples <- 500
samples_ps <- matrix(nrow = n_samples, ncol = sample_size)

group_names <- names(n_per_group)

for (i in 1:n_samples) {
  sample_rows <- c()
  
  for (group in group_names) {
    group_data <- Houses[Houses$ocean_proximity == group, ]
    n <- n_per_group[group]
    sampled_indices <- sample(1:nrow(group_data), n)
    sample_rows <- c(sample_rows, group_data[sampled_indices, ]$median_house_value)  # or any unique ID
  }
  
  samples_ps[i, ] <- sample_rows
}

# Convert to dataframe and name rows/columns
samples_ps <- as.data.frame(samples_ps)
rownames(samples_ps) <- paste0("Sample", 1:n_samples)
colnames(samples_ps) <- paste0("Observation", 1:sample_size)

# Preview
head(samples_ps)

is this ok for ps sampling