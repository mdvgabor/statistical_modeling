# Set working directory
setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")

# Load required libraries
library(ggplot2)
library(ggpmisc)

# Read CSV
df_original <- read.csv("pl_ll_data_2022-2025.csv")

# Convert relevant columns to numeric
cols_to_convert <- 2:(ncol(df_original) - 2)
for (col in cols_to_convert) {
  df_original[[col]] <- suppressWarnings(as.numeric(as.character(df_original[[col]])))
}

# Keep only numeric columns, and re-add the Team column
df_clean <- df_original[, sapply(df_original, is.numeric)]
df_clean$Team <- df_original$Team

# Preserve the original order of teams
team_order <- match(unique(df_clean$Team), df_clean$Team)

# Aggregate numeric values by Team
df_aggregated <- aggregate(. ~ Team, data = df_clean, FUN = sum)

# Reorder aggregated data to match original team order
df_aggregated <- df_aggregated[match(unique(df_original$Team), df_aggregated$Team), ]

rownames(df_aggregated) <- NULL
df_aggregated$Country <- c(rep("ENG", 24), rep("ESP", nrow(df_aggregated) - 24))

df_clean$Country <- c(rep("ENG", 60), rep("ESP", nrow(df_clean) - 60))

df <- df_clean

# ------------------------------------------------------------------------------
ggplot(df_aggregated, aes(x = xG, y = Goals, color = Country, fill = Country, label = Team)) +
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
  labs(title = "Összesített xG és Gólok (mindhárom szezonban szereplő csapatok)",
       x = "Összesített xG",
       y = "Összesített Gólok",
       color = "Liga",
       fill = "Liga") +
  theme_minimal()

# ------------------------------------------------------------------------------
# szerzett pontok és gólok kapcsolata

ggplot(df_aggregated, aes(x = Goals, y = Points, color = Country, fill = Country, label = Team)) +
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
    title = "Szerzett pontok és szerzett gólok kapcsolata",
    x = "Szerzett Gólok",
    y = "Szerzett Pontok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# szerzett gólok és szögletek kapcsolata
# a szöglet nem fél gól

ggplot(df_aggregated, aes(x = Corner.Kicks, y = Goals, color = Country, fill = Country, label = Team)) +
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
    title = "Szerzett gólok és a szögletek számának kapcsolata",
    x = "Szögletek száma",
    y = "Szerzett Gólok",
    color = "Liga",
    fill = "Liga"
  ) +
  theme_minimal()



