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

#############################
### scale the variables 

covs <- c("lowfreq_ratio", "rms",
          "bgn_bird_med", "bgn_bird_max", "bgn_low_med",
          "pow_bird_med", "pow_bird_max", "pow_low_med",
          "ent_bird_med", "ent_bird_max", "ent_low_med")

z_df <- df |>
  mutate(across(all_of(covs),
                ~ as.numeric(scale(.x))))

# 1. Correlogram
## df of covariates for each habitat type
boreal_covs <- z_df[z_df$habitat == "boreal", covs]
grassland_covs <- z_df[z_df$habitat == "grassland", covs]

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
z_df <- z_df |>
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

quality_bps <-  ggplot(z_df, aes(x = acceptable, y = value, fill = acceptable)) +
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

 