setwd("YOUR WD HERE")

library(ggplot2)

# my parameters for n, p, lambda and the required number of 10k runs
n <- 10000
p <- 0.12345
lambda <- n*p
simulations <- 10000

bin_samples <- rbinom(simulations, size = n, prob = p)
poi_samples <- rpois(simulations, lambda = lambda)

# createing frequency tables and converting them to data frames
bin_df <- as.data.frame(table(bin_samples)/simulations)
poi_df <- as.data.frame(table(poi_samples)/simulations)

# renaming the  columns
colnames(bin_df) <- c("k", "Binomial_Prob")
colnames(poi_df) <- c("k", "Poisson_Prob")

# converting k to numeric to merge the dataframes into one to plot them later
bin_df$k <- as.numeric(as.character(bin_df$k))
poi_df$k <- as.numeric(as.character(poi_df$k))

# merging the df-s to one
merged_df <- merge(bin_df, poi_df, by = "k", all = TRUE)

# i replace NA values with 0 because some k values exist in one distribution 
#but not the other
merged_df[is.na(merged_df)] <- 0

# plotting the rsults
ggplot(merged_df, aes(x = k)) +
  geom_point(aes(y = Binomial_Prob, color = "binomial"), size = 1) +
  geom_point(aes(y = Poisson_Prob, color = "poisson"), size = 1) +
  labs(title = "Poisson Limit Theorem Approximation",
       x = "number of sucesses (k)", y = "probability") +
  scale_color_manual(name = "distribution", values = c("binomial" = "blue", "poisson" = "red")) +
  theme_minimal()




