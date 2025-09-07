setwd("YOUR WD HERE")

# NOTE: this is only the task worth 1 point.

# packages needed
library(readxl)
library(foreach)
library(doParallel)

# Load Excel data and extract salary column
employee_data <- read_excel("EmployeeSalary.xlsx")
salary_vector <- employee_data$Salary

# Detect number of CPU cores and create a cluster (leave one core for the system)
num_cores <- detectCores()
num_workers <- num_cores - 1
parallel_cluster <- makeCluster(num_workers)
registerDoParallel(parallel_cluster)

# total number of possible 50-element samples (without replacement)
total_possible_samples <- choose(length(salary_vector), 50)

### Attempting huge number of samples : TOO BIG, DOESNT RUN
# num_attempt <- 300^50
# results <- foreach(i = 1:num_attempt) %dopar% {
#   sample(salary_vector, size = 50, replace = TRUE)
# }
# I got this error: "result would be too long a vector"

# i generated 123456 samples instead
num_samples <- 123456
iid_samples <- foreach(i = 1:num_samples) %dopar% {
  sample(salary_vector, size = 50, replace = TRUE)
}

# stop the parallel cluster
stopCluster(parallel_cluster)


