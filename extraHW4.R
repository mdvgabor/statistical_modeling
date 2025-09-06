setwd("YOUR WD HERE")
library(ggplot2)
library(readxl)

# reading in the employee data
EmployeeData <- read_excel("EmployeeSalary.xlsx")

# setting seed and defining sample size
sample_size <- 10
set.seed(17)

# generating 20000 IID samples
samples <- sample(EmployeeData$Age, sample_size, replace = TRUE)
for (i in 1:19999) {
  set.seed(17+i)
  samples <- rbind(samples,sample(EmployeeData$Age, size = sample_size,replace = TRUE))
}

# converting to dataframe, naming rows and cols.
samples <- as.data.frame(samples)
rownames(samples)<- paste0("Sample", 1:20000)
colnames(samples) <- paste0("Observation",1:10)

# function to calculate population variance
class_var <- function(x){
  mean((x - mean(x))^2)
}

# calc. population parameters
PopMean <- mean(EmployeeData$Age)
PopVariance<-class_var(EmployeeData$Age)
PopSd <- sd(EmployeeData$Age)

# calculating mean and sd for each sample
samples$sample_means <- apply(samples, 1, mean)
samples$sample_sds <- apply(samples[1:10],1, sd)  # corrected sample SD

# calculating SE using population SD
SE_Mean <- sqrt(PopVariance)/sqrt(sample_size)

# calculating z values assuming population SD ~ N(0,1)
samples$z <- (samples$sample_means - PopMean)/SE_Mean

# calculating SE using corrected sample SD
samples$SE_Mean_Corrected <- samples$sample_sds / sqrt(sample_size)

# calculating t values assuming sample sd ~ t(n-1)
samples$t <- (samples$sample_means - PopMean) /samples$SE_Mean_Corrected

# plot 1
ggplot(data = samples, aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins = 50, fill = "pink", color = "purple") +
  stat_function(fun = dnorm, args=list(mean = 0,sd = 1), col = "blue", linewidth = 1) +
  labs(title = "z values using population sd")

# plot 2
ggplot(data = samples, aes(x = t)) +
  geom_histogram(aes(y = after_stat(density)),bins = 50, fill= "pink", color = "purple") +
  stat_function(fun=dt, args = list(df = sample_size -1), col = "blue", linewidth = 1) +
  labs(title="t values using sample sd")


