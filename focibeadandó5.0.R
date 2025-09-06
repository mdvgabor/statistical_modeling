# Set working directory
setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

# Load required libraries
library(ggplot2)
library(ggpmisc)

#install.packages("corrr")
library('corrr')

#install.packages("ggcorrplot")
library(ggcorrplot)

#install.packages("FactoMineR")
library("FactoMineR")

#install.packages("factoextra")
library(factoextra)


df_original <- read.csv("pl_ll_data_2022-2025.csv")

cols_to_convert <- 2:(ncol(df_original) - 2)
for (col in cols_to_convert) {
  df_original[[col]] <- suppressWarnings(as.numeric(as.character(df_original[[col]])))
}

df_clean <- df_original[, sapply(df_original, is.numeric)]
df_clean$Team <- df_original$Team

team_order <- match(unique(df_clean$Team), df_clean$Team)

# Aggregate numeric values by Team
df_aggregated <- aggregate(. ~ Team, data = df_clean, FUN = sum)

# Reorder aggregated data to match original team order
df_aggregated <- df_aggregated[match(unique(df_original$Team), df_aggregated$Team), ]

rownames(df_aggregated) <- NULL
df_aggregated$Country <- c(rep("ENG", 24), rep("ESP", nrow(df_aggregated) - 24))

df_clean$Country <- c(rep("ENG", 60), rep("ESP", nrow(df_clean) - 60))

df <- df_clean


# Build linear regression model
model <- lm(Points ~ ., data = df_clean[, sapply(df_clean, is.numeric)])

# Summary to see coefficients and p-values
summary(model)


# Variable importance plot (based on absolute t-values)
importance <- abs(summary(model)$coefficients[-1, "t value"])
barplot(sort(importance, decreasing = TRUE),
        main = "Variable Importance (|t-value|)",
        las = 2)

# Select only numeric columns
numeric_data <- df_clean[, sapply(df_clean, is.numeric)]

# Calculate correlations with Points
cor_with_points <- cor(numeric_data, use = "complete.obs")[, "Points"]

# Remove self-correlation
cor_with_points <- cor_with_points[names(cor_with_points) != "Points"]

# Sort by strength of correlation
cor_with_points <- sort(cor_with_points, decreasing = TRUE)

# Print
print(round(cor_with_points, 3))

barplot(cor_with_points,
        main = "Correlation of Variables with Points",
        las = 2, col = "skyblue",
        ylab = "Correlation Coefficient")
abline(h = 0, col = "red", lty = 2)



