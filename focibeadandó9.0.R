setwd("~/Desktop/focibeadandó/scriptek:források")
# ezt a saját working directoryra kell állítani

# ------------------------------------------------------------------------------
# felhasznált könyvtárak
library(ggplot2)
library(ggpmisc)
library(ggcorrplot)

# ------------------------------------------------------------------------------
# forrásfájl beolvasása
df <- read.csv("pl_ll_data_2022-2025.csv")

# oszlopok adattípusának megváláltozatatása
keep_cols <- c(1, ncol(df) - 1, ncol(df))
df[-keep_cols] <- lapply(df[-keep_cols],function(x) as.numeric(as.character(x)))

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
    title = "A szerzett gólok és a szögletek számának kapcsolata",
    x = "Szögletek száma",
    y = "Szerzett Gólok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# lapok számának kapcsolata a megszerzett ponntokkal

ggplot(df, aes(x = Yellow.Cards + Red.Cards, y = Points, color = Country, fill = Country, label = Team)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    method = "lm",
    label.x = "right",
    label.y = "top"
  ) +
  scale_color_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  scale_fill_manual(values = c("ENG" = "blue", "ESP" = "red")) +
  labs(
    title = "A kapott lapok és a szerzett pontok számának kapcsolata",
    x = "Kapott Lapok",
    y = "Pontok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# átlagos lövéstávolság kapcsolata a szerzett gólokkal

ggplot(df, aes(x = Avg.Shot.Distance, y = Goals)) +
  geom_point(size = 2) +
  geom_text(aes(label = Team), vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "red") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    method = "lm",
    label.x = "right",
    label.y = "top"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# A statisztikai mutatók korrelációja
df_numeric <- df[, 2:(ncol(df) - 2)]
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Plot
ggcorrplot(cor_matrix, 
           lab = TRUE, 
           type = "full", 
           colors = c("red", "yellow", "green"),
           title = "A statisztikai mutatók korrelációja",
           lab_size = 3)
# ------------------------------------------------------------------------------
# mi befolyásolja az xG-t
df_numeric <- df[, sapply(df, is.numeric)]
df_model<- df_numeric[, !(names(df_numeric) %in% c("Goals", "Assists", "xAG", "Points"))]

model_1 <- lm(xG ~ ., data = df_model)
#summary(model_1)

# nem szignifikáns:p>0.05: Red.Cards, Free.Kicks, TouchesAtt3rd, Passes.Live

model_2 <- lm(xG ~ Shots + Shots.on.Target + Avg.Shot.Distance +
                Possession + Corner.Kicks, data = df_numeric)
summary(model_2)

# a model_2 egyenlete
# xG = 42.588 
# + 0.078 * Shots 
# + 0.148 * Shots.on.Target 
# - 3.123 * Avg.Shot.Distance 
# + 0.306 * Possession 
# - 0.065 * Corner.Kicks

# ------------------------------------------------------------------------------
# tesztadatok a modellhez (Premier League és La Liga 2021-2022-es szezon)
newdata <- read.csv("testdata.csv")

# oszlopok adattípusának megváláltozatatása
keep_cols <- c(1, ncol(newdata) - 1, ncol(newdata))
newdata[-keep_cols] <- lapply(newdata[-keep_cols],function(x) as.numeric(as.character(x)))

newdata_numeric <- newdata[, sapply(newdata, is.numeric)]
newdata_model <- newdata_numeric[, c("Shots", "Shots.on.Target", 
                                     "Avg.Shot.Distance", 
                                     "Possession", "Corner.Kicks")]

predicted_xG <- predict(model_2, newdata = newdata_model)

# valódi és prediktált xG értékek
results <- data.frame(
  Team = newdata$Team,
  Actual_xG = newdata_numeric$xG,
  Predicted_xG = predicted_xG
)

ggplot(results, aes(x = Actual_xG, y = Predicted_xG, label = Team)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 5,
    label.x = "left",
    label.y = "top"
  ) +
  labs(
    title = "Prediktált és valós xG értékek összehasonlítása",
    x = "Valós xG",
    y = "Prediktált xG"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# Hibák kiszámítása
errors <- results$Actual_xG - results$Predicted_xG

# MAE – Átlagos abszolút hiba
mae <- mean(abs(errors))

# MAPE – Átlagos százalékos hiba
mape <- mean(abs(errors / results$Actual_xG)) * 100

# Eredmények kiírása
cat("Átlagos abszolút hiba: ", round(mae, 2), "\n")
cat("Átlagos százalékos hiba: ", round(mape, 2), "%\n")

# ------------------------------------------------------------------------------