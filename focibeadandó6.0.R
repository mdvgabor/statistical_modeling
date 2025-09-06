setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
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
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Plot
ggcorrplot(cor_matrix, 
           lab = TRUE, 
           type = "full", 
           colors = c("red", "yellow", "green"),
           title = "A statisztikai mutatók korrelációja",
           lab_size = 3)
# ------------------------------------------------------------------------------
# kiveszem a gólokat, asszisztokat, kaput eltaláló lövéseket, passes.live
# minden ki kivéve: pontok, xG, lövés, támadóharmad érintés, labdabirtoklás
# egyesíteni a szögletet és szabadrúgást : pontrúgás

model_df <- df_numeric[, names(df_numeric) %in% c("Points","xG", "Shots", 
                                                  "TouchesAtt3rd", "Possession", 
                                                  "Free.Kicks", "Corner.Kicks")]
# pontrúgás oszlop
model_df$Set.Piece <- model_df$Free.Kicks + model_df$Corner.Kicks

# model_1
model_1 <- lm(Points ~ . - Corner.Kicks - Free.Kicks, data = model_df)
summary(model_1)

# csak az xG és a érintések a támadóharmadban (kicsit) szignifikáns

# model_2
model_2 <- lm(Points ~ xG + TouchesAtt3rd, data = model_df)
summary(model_2)

# points = -10.647171 + 0.821552*xG + 0.003602*TouchesAtt3rd

# ------------------------------------------------------------------------------