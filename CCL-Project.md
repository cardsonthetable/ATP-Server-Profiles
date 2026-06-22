---
title: "Serve Profiles on the ATP Tour (2020–2024)"
date: "March 6, 2025"
author: "Isaac Stiepleman"
output:
  html_document:
    keep_md: TRUE
fontsize: 11pt
geometry: margin = 1in
---
# Introduction

The ATP Tour represents the highest level of professional men's tennis where small differences in serve performance can significantly influence match outcomes. While serve ability is often summarized for the average fan with ace counts or serve speed, in reality, it consists of multiple correlated components. This write-up analyzes serve ability using match-level data from the ATP Tour between 2020 and 2024. I will utilize Principal Components Analysis (PCA) to create various server profiles and determine whether differences in those profiles are meaningfully related to players' match win percentages.



# Data Construction


```r
suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(viridis)
})

matches <- read_csv("atp 2020_2024.csv") |>
  mutate(year = as.integer(substr(tourney_date, 1, 4))) |>
  filter(year >= 2020, year <= 2024) |> #restrict analysis to 2020-2024
  filter(!is.na(w_ace)) #remove matches with no point-level data
```

I restrict the dataset to matches played between 2020 and 2024 in order to focus on the contemporary ATP game. Furthermore, matches with missing point-level statistics are excluded. 


```r
winners <- matches |>
  transmute(player_id = winner_id, player_name = winner_name,
            aces = w_ace, double_faults = w_df, 
            serve_points = w_svpt, first_in = w_1stIn, 
            first_won = w_1stWon, second_won = w_2ndWon, 
            won_match = 1L)

losers <- matches |>
  transmute(player_id = loser_id, player_name = loser_name,
            aces = l_ace, double_faults = l_df,
            serve_points = l_svpt, first_in = l_1stIn, 
            first_won = l_1stWon, second_won = l_2ndWon,
            won_match = 0L)
```

Since each row corresponds to a single match and contains point-level data for both players, I convert each match into two player-level observations: one for the winner and one for the loser.


```r
player_summary <- bind_rows(winners, losers) |>
  group_by(player_id, player_name) |>
  summarize(n_matches = n(),
            aces = sum(aces, na.rm = TRUE),
            double_faults = sum(double_faults, na.rm = TRUE),
            serve_points = sum(serve_points, na.rm = TRUE),
            first_in = sum(first_in, na.rm = TRUE),
            first_won = sum(first_won, na.rm = TRUE),
            second_won = sum(second_won, na.rm = TRUE),
            win_pct = mean(won_match, na.rm = TRUE), .groups = "drop") |>
  mutate(ace_rate = aces / serve_points,
         df_rate = double_faults / serve_points,
         first_serve_pct = first_in / serve_points,
         first_serve_pts_won = first_won / first_in,
         second_serve_pts_won = second_won / (serve_points - first_in)) |>
  filter(n_matches >= 30) #for stability
```

I aggregate raw totals of various serve metrics across each player’s matches before calculating rates. I chose the following serve metrics to capture different aspects of serve performance:

- Ace rate $\left(\frac{\sum \text{aces}}{\sum \text{serve points}}\right)$ measures a player's ability to generate unreturnable serves.
- Double-fault rate $\left(\frac{\sum \text{double faults}}{\sum \text{serve points}}\right)$ measures serve errors that directly concede points.
- First-serve percentage $\left(\frac{\sum \text{first serves in}}{\sum \text{serve points}}\right)$ measures how frequently the first serve is successfully put into play.
- First-serve points won $\left(\frac{\sum \text{first serves won}}{\sum \text{first serves played}}\right)$ measures the effectiveness of the first serve once it lands.
- Second-serve points won $\left(\frac{\sum \text{second serves won}}{\sum \text{second serves played}}\right)$ measures how well a player avoids being disadvantaged when forced to rely on a second serve.

# PCA of Serve Metrics

Because the serve metrics are highly correlated, I use Principal Components Analysis to reduce dimensionality and build serve profiles.


```r
serve_vars <- player_summary |>
  select(ace_rate, df_rate, first_serve_pct, first_serve_pts_won, 
         second_serve_pts_won)

serve_pca <- prcomp(serve_vars, scale. = TRUE)

summary(serve_pca)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5
## Standard deviation     1.4986 1.2580 0.8449 0.61869 0.27392
## Proportion of Variance 0.4491 0.3165 0.1428 0.07655 0.01501
## Cumulative Proportion  0.4491 0.7657 0.9084 0.98499 1.00000
```

PC1 explains $44.9\%$ of total variance while PC2 explains $31.7\%$ for a cumulative $76.6\%$ captured by the first two components. This is sufficient explanation to focus solely on them.


```r
serve_pca$rotation
```

```
##                               PC1        PC2         PC3         PC4
## ace_rate             -0.593522493 -0.2512643 -0.22751390 -0.32267595
## df_rate              -0.413584671  0.4584968 -0.37427757  0.69173930
## first_serve_pct       0.324218919 -0.4008035 -0.84358256  0.00565506
## first_serve_pts_won  -0.609506188 -0.2704963  0.02274624 -0.16031640
## second_serve_pts_won -0.007941617 -0.7020223  0.30984642  0.62581439
##                              PC5
## ace_rate             -0.65476330
## df_rate              -0.01189185
## first_serve_pct       0.15025030
## first_serve_pts_won   0.72740308
## second_serve_pts_won -0.13947479
```

PC1 loads most heavily on ace rate $(-0.594)$ and first-serve points won $(-0.610)$, with an opposing contribution from first-serve percentage $(0.324)$. As a result, PC1 appears to primarily captures the trade off between first-serve dominance and first-serve reliability.

- Lower PC1 values correspond to players whose first-serves generate more aces and win a higher share of first-serve points but have a lower first-serve percentage.
- Higher PC1 values correspond to players who achieve higher first-serve consistency but generate fewer aces and exhibit less dominance once the serve lands.

PC2 loads very strongly on second-serve points won $(-0.702)$ and first-serve percentage $(-0.401)$, with an opposing contribution from double-fault rate $(0.458)$. This structure indicates that PC2 primarily captures the tradeoff between overall serve dependability and error avoidance.

- Lower PC2 values correspond to players who serve more dependably and win a higher share of second-serve points while committing fewer double faults.
- Higher PC2 values correspond to playesr who commit more errors with weak second-serve performance and more double faults.

# Serve Profiles and Match Success


```r
pca <- player_summary |>
  bind_cols(as.data.frame(serve_pca$x[, 1:2]))  # PC1, PC2

ggplot(pca, aes(PC1, PC2, color = win_pct)) +
  geom_point() +
  scale_color_viridis_c() + #Change colors on the scale
  labs(x = "PC1: Serve Outcome Dominance",
       y = "PC2: Serve Dependability",
       color = "Win %",
       title = "Serve Styles and Match Success on the ATP Tour") +
  theme_minimal()
```

The scatterplot shows substantial clustering, with players exhibiting more dominant first serves outcomes and greater overall dependability generally achieving higher win percentages.

To quantify the relationship with match outcomes, I regress win percentage on PC1 and PC2.


```r
model <- lm(win_pct ~ PC1 + PC2, data = pca); summary(model)
```

```
## 
## Call:
## lm(formula = win_pct ~ PC1 + PC2, data = pca)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.203248 -0.051326 -0.004501  0.047199  0.264535 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.470992   0.005911  79.681  < 2e-16 ***
## PC1         -0.018287   0.003955  -4.624 6.96e-06 ***
## PC2         -0.053582   0.004711 -11.374  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0819 on 189 degrees of freedom
## Multiple R-squared:  0.4437,	Adjusted R-squared:  0.4378 
## F-statistic: 75.38 on 2 and 189 DF,  p-value: < 2.2e-16
```

Both components are statistically significant predictors of match success. The coefficient on PC1 is $-0.018$ $(p = 6.96 \times 10^{-6})$, while PC2 has a larger magnitude of $-0.054$ $(p < 2 \times 10^{-16})$. Together, the model explains $44.4\%$ of the variation in player win percentages.

Given the flipped orientation of the PCA axes, these negative coefficients indicate that players with less dominant first-serve outcomes or less dependable overall serving achieve lower win rates. Of particular interest, the larger coefficient on PC2 indicates that overall serve dependability is more important for sustained success than an overtly dominant first-serve.

# Conclusion

This analysis shows that professional serve ability is multidimensional. PCA shows that the differences in serve-profiles across all ATP players can be pretty well defined by first-serve outcome dominance and overall serve dependability. The first dimension (PC1) captures a player's ability to generate free points off of aces and win a higher share of points when the first serve lands in, while the second dimension (PC2) captures consistency and error avoidance, particularly on the second serve. These two dimensions explain nearly half of the variation in match win percentage across all players which shows their joint importance for competitive success.

Most importantly, high productivity in one dimension alone is not enough. Players who max out on first-serve dominance but struggle with their serve reliability will struggle, while players who serve consistently but are unable to win points through aces or aggressive first-serves are similarly limited. Success on the ATP Tour therefore requires both the ability to generate advantages early in points and the ability to avoid costly errors when under pressure. The results suggest that elite serve performance is best understood as the balance between aggression and reliability rather than as a single trait.
