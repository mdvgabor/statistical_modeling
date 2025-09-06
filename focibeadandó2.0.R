setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(ggplot2)
library(ggpmisc)

df <- read.csv("pl_ll_data_2022-2025.csv")

# Convert appropriate columns safely
cols_to_convert <- 2:(ncol(df) - 2)

# Convert only if coercion results in numeric
for (col in cols_to_convert) {
  df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
}

# Remove non-numeric columns before aggregating
# Keep only numeric columns + 'Team'
df_clean <- df[, sapply(df, is.numeric)]
df_clean$Team <- df$Team  # add back Team column

# Now aggregate
df_aggregated <- aggregate(. ~ Team, data = df_clean, FUN = sum)

# ------------------------------------------------------------------------------
# várható és lőtt gólok kapcsolatának vizsgálata
team_seasons <- table(df$Team, df$Season)
teams_all_seasons <- rownames(team_seasons)[rowSums(team_seasons > 0) == 3]

# Filter only those teams
df_filtered <- df[df$Team %in% teams_all_seasons, ]

# Aggregate: sum of xG and Goals per team
df_summed <- aggregate(cbind(xG, Goals) ~ Team + Country, data = df_filtered, sum)

# Plot
ggplot(df_summed, aes(x = xG, y = Goals, color = Country, fill = Country, label = Team)) +
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

ggplot(df, aes(x = Goals, y = Points)) +
  geom_point(size = 2) +
  geom_text(aes(label = rownames(df)), vjust = -0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    method = "lm",
    label.x = 0.05,
    label.y = 0.95
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  labs(
    title = "Szerzett pontok és szerzett gólok kapcsolata",
    x = "Szerzett Gólok",
    y = "Szerzett Pontok"
  ) +
  theme_minimal()
# ------------------------------------------------------------------------------
# szerzett pontok és szögletek kapcsolata








