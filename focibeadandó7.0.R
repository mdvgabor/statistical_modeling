setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
# ------------------------------------------------------------------------------
# felhasznált könyvtárak
library(ggplot2)
library(ggpmisc)
library(ggcorrplot) # ez lehet nem kell majd

# ------------------------------------------------------------------------------
# forrásfájl beolvasása
df <- read.csv("pl_ll_data_2022-2025.csv")

# oszlopok adattípusának megváláltozatatása
keep_cols <- c(1, ncol(df) - 1, ncol(df))
df[-keep_cols] <- lapply(df[-keep_cols],function(x) as.numeric(as.character(x)))
df_numerical <- df[-keep_cols]
# ------------------------------------------------------------------------------

# Az xG és a szerzett gólok kapcsolata 2022 és 2025 között

ggplot(df, aes(x = xG, y = Goals, color = Country, fill = Country, label = Team)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x,
               parse = TRUE,
               method = "lm",
               label.x = "left",
               label.y = "top") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  scale_fill_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  labs(title = "Az xG és a szerzett gólok kapcsolata 2022 és 2025 között",
       x = "xG",
       y = "Gólok",
       color = "Liga",
       fill = "Liga") +
  theme_minimal()

# ------------------------------------------------------------------------------

# A szerzett pontok és szerzett gólok kapcsolata 2022 és 2025 között

ggplot(df, aes(x = Goals, y = Points, color = Country, fill = Country, label = Team)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    method = "lm",
    label.x = "left",
    label.y = "top"
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  scale_color_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  scale_fill_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  labs(
    title = "A szerzett pontok és szerzett gólok kapcsolata 2022 és 2025 között",
    x = "Szerzett Gólok",
    y = "Szerzett Pontok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------

# szerzett gólok és szögletek kapcsolata
# a szöglet nem fél gól

ggplot(df, aes(x = Corner.Kicks, y = Goals, color = Country, fill = Country, label = Team)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    method = "lm",
    label.x = "left",
    label.y = "top"
  ) +
  geom_abline(intercept = 0, slope = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  scale_fill_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  labs(
    title = "A szerzett pontok és a szögletek számának kapcsolata",
    x = "Szögletek száma",
    y = "Szerzett Gólok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# Load required libraries
#install.packages("glmnet")
library(glmnet)

#install.packages("randomForest")
library(randomForest)

#install.packages("caret")
library(caret)

#install.packages("dplyr")
library(dplyr)

# Set seed for reproducibility
set.seed(17)

# 1. Preprocessing
# Remove rows with missing values (if any)
df_numerical <- na.omit(df_numerical)

# Define predictors and target
X <- df_numerical %>% select(-Points)
y <- df_numerical$Points

# Convert to matrix for glmnet
X_matrix <- model.matrix(~ ., data = X)[, -1]

# 2. Train-Test Split
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_matrix[train_index, ]
X_test <- X_matrix[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# 3. Linear Regression
lm_model <- lm(Points ~ ., data = df_numerical[train_index, ])
lm_preds <- predict(lm_model, newdata = df_numerical[-train_index, ])
lm_rmse <- sqrt(mean((lm_preds - y_test)^2))
cat("Linear Regression RMSE:", lm_rmse, "\n")

# 4. Lasso Regression
lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_best_lambda <- lasso_cv$lambda.min
lasso_preds <- predict(lasso_cv, s = lasso_best_lambda, newx = X_test)
lasso_rmse <- sqrt(mean((lasso_preds - y_test)^2))
cat("Lasso Regression RMSE:", lasso_rmse, "\n")

# 5. Ridge Regression
ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0)
ridge_best_lambda <- ridge_cv$lambda.min
ridge_preds <- predict(ridge_cv, s = ridge_best_lambda, newx = X_test)
ridge_rmse <- sqrt(mean((ridge_preds - y_test)^2))
cat("Ridge Regression RMSE:", ridge_rmse, "\n")

# 6. Random Forest
rf_model <- randomForest(x = X_train, y = y_train, importance = TRUE)
rf_preds <- predict(rf_model, newdata = X_test)
rf_rmse <- sqrt(mean((rf_preds - y_test)^2))
cat("Random Forest RMSE:", rf_rmse, "\n")

# 7. Feature Importance from Random Forest
importance_df <- as.data.frame(importance(rf_model))
importance_df <- importance_df %>%
  arrange(desc(IncNodePurity)) %>%
  mutate(Feature = rownames(importance_df))

# Display top features
print(head(importance_df, 10))

plot(y_test, lm_preds,
     main = "Actual vs Predicted (Linear Regression)",
     xlab = "Actual Points", ylab = "Predicted Points",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# Get coefficients
coefs <- coef(lm_model)

# Print the equation
cat("Points = ", round(coefs[1], 3))  # Intercept
for (i in 2:length(coefs)) {
  cat(" +", round(coefs[i], 3), "*", names(coefs)[i])
}
cat("\n")


















