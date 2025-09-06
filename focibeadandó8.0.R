setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

# ------------------------------------------------------------------------------
# Könyvtárak betöltése
library(caret)
library(dplyr)

# ------------------------------------------------------------------------------
# Adat beolvasása és előfeldolgozása
df <- read.csv("pl_ll_data_2022-2025.csv")

# Csak numerikus oszlopok megtartása
keep_cols <- c(1, ncol(df) - 1, ncol(df))
df[-keep_cols] <- lapply(df[-keep_cols], function(x) as.numeric(as.character(x)))
df_numerical <- df[-keep_cols]
df_numerical <- na.omit(df_numerical)

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
# Prediktorok és célváltozó definiálása
X <- df_numerical %>% select(-Points)
y <- df_numerical$Points

# ------------------------------------------------------------------------------
# Train-test osztás
set.seed(17)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- df_numerical[train_index, ]
test_data <- df_numerical[-train_index, ]
y_test <- y[-train_index]

# ------------------------------------------------------------------------------
# Lineáris regresszió
lm_model <- lm(Points ~ ., data = train_data)
lm_preds <- predict(lm_model, newdata = test_data)
lm_rmse <- sqrt(mean((lm_preds - y_test)^2))
r_squared <- 1 - sum((y_test - lm_preds)^2) / sum((y_test - mean(y_test))^2)

cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Linear Regression R²:", round(r_squared, 3), "\n")
# ------------------------------------------------------------------------------
# Linear regression vizualizáció
plot(y_test, lm_preds,
     main = "Actual vs Predicted (Linear Regression)",
     xlab = "Actual Points", ylab = "Predicted Points",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# ------------------------------------------------------------------------------
# Egyenlet kiíratása
coefs <- coef(lm_model)
cat("Points = ", round(coefs[1], 3))
for (i in 2:length(coefs)) {
  cat(" +", round(coefs[i], 3), "*", names(coefs)[i])
}
cat("\n")

# ------------------------------------------------------------------------------



# Új modell Goals, xAG, Shots, Corner.Kicks nélkül
drop_vars <- c("Goals", "xAG", "Shots", "Corner.Kicks")
train_reduced <- train_data %>% select(-all_of(drop_vars))
test_reduced <- test_data %>% select(-all_of(drop_vars))

# Új modell illesztése
lm_model_reduced <- lm(Points ~ ., data = train_reduced)
lm_preds_reduced <- predict(lm_model_reduced, newdata = test_reduced)
lm_rmse_reduced <- sqrt(mean((lm_preds_reduced - y_test)^2))
cat("Reduced model RMSE:", lm_rmse_reduced, "\n")
summary(lm_model_reduced)


