#covariate exploration and graphs

# load libraries

library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)
library(patchwork)


###########################
# START HERE
###########################

df <- readRDS("data/complete_noise_dataset.rds")

covs <- c("lowfreq_ratio", "rms",
          "bgn_bird_med", "bgn_bird_max", "bgn_low_med",
          "pow_bird_med", "pow_bird_max", "pow_low_med",
          "ent_bird_med", "ent_bird_max", "ent_low_med")

df_covs <- df %>% select(lowfreq_ratio, rms, bgn_bird_med, bgn_bird_max, bgn_low_med,
                         pow_bird_med, pow_bird_max, pow_low_med, ent_bird_med, ent_bird_max, ent_low_med)

## Check for outliers

##scatter plots

scatter_all <- pairs(df_covs,
         cex = 0.5,
         cex.labels = 0.9,
         main = "Scatterplot Matrix of Covariates") 

scatter_all

# 1. Correlogram

#Choose Pearson correlation when your variables have a linear relationship and 
#no extreme outliers; choose Spearman correlation when the relationship is 
#monotonic (consistently increasing or decreasing, even if curved) or involves
#ordinal data, skewed distributions, or outliers.

## single correlogram


corr_pearson <- ggcorr(df_covs,
                         method = c("pairwise", "pearson"),
                         label = TRUE, label_size = 3, label_round = 2,
                         hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates (Pearson)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

corr_pearson

corr_spearman <- ggcorr(df_covs,
                       method = c("pairwise", "spearman"),
                       label = TRUE, label_size = 3, label_round = 2,
                       hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates (Spearman)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

corr_spearman


## df of covariates for each habitat type
boreal_covs <- df[df$habitat == "boreal", covs]
grassland_covs <- df[df$habitat == "grassland", covs]

##correlogram for boreal
boreal_corr <- ggcorr(boreal_covs,
       method = c("pairwise", "spearman"),
       label = TRUE, label_size = 3, label_round = 2,
       hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates (Boreal)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
boreal_corr

##correlogram for grassland
grassland_corr <- ggcorr(grassland_covs,
       method = c("pairwise", "spearman"),
       label = TRUE, label_size = 3, label_round = 2,
       hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates (Grassland)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
grassland_corr

##side by side
boreal_corr + grassland_corr

# 2. Scatterplots
##boreal scatterplot
boreal_scatter <- pairs(boreal_covs,
        cex = 0.5,
        cex.labels = 0.9,
      main = "Scatterplot Matrix of Covariates (Boreal)") 
boreal_scatter

##grassland scatter
grassland_scatter <- pairs(grassland_covs,
                        cex = 0.5,
                        cex.labels = 0.9,
                        main = "Scatterplot Matrix of Covariates (Grassland)") 
grassland_scatter

# 3. Boxplots by quality of recording

df2 <- df |>
  filter(acceptable %in% c("y", "n", "b")) |> ##filtering out malfunction
  mutate(acceptable = factor(acceptable,
                             levels = c("y", "n", "b"),
                             labels = c("Good", "Bad", "Borderline"))) |> ##creating labels for boxplots
  pivot_longer(cols = c(lowfreq_ratio, rms, bgn_bird_med, bgn_bird_max, bgn_low_med,
                        pow_bird_med, pow_bird_max, pow_low_med, ent_bird_med, ent_bird_max, ent_low_med),
               names_to = "variable",
               values_to = "value") |> ##adding variable column
  mutate(variable = factor(variable, levels = c(
    "bgn_bird_med", "pow_bird_med", "ent_bird_med", "lowfreq_ratio", ##row 1
    "bgn_bird_max", "pow_bird_max", "ent_bird_max", "rms", ##row 2
    "bgn_low_med", "pow_low_med", "ent_low_med" ##row 3
  ))) ##ordering how the boxplots will appear

quality_bps <-  ggplot(df2, aes(x = acceptable, y = value, fill = acceptable)) +
  geom_boxplot(alpha = 0.7, notch = TRUE) +
  facet_wrap(~ variable, scales = "free_y", ncol = 4) + 
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Covariates by Recording Quality",
    x = "Recording Quality",
    y = "Measurement Value"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        legend.position = "none")

quality_bps ##printing plot


                       