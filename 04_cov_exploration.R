#covariate exploration and graphs

# load libraries

library(tidyverse)
library(GGally)
library(ggplot2)


# 1. read in all scored data with metrics

file_list <- list.files(path = "data/", pattern = "\\.csv$", full.names = TRUE)

# 2. Read and combine them all at once
# The 'id' argument adds a column showing which file the data came from

df <- read_csv(file_list, id = "source_file")


# 3. Correlogram
covariates <- df |>
  filter(is.na(error)) |>
  select(lowfreq_ratio, rms,
         matches("^(bgn|pow|ent)_(bird_med|bird_max|low_med)$"))

ggcorr(covariates,
       method = c("pairwise", "spearman"),
       label = TRUE, label_size = 3, label_round = 2,
       hjust = 0.9, layout.exp = 2,) +
  labs(title = "Correlogram of Covariates") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 4. Scatterplots
scatterplots <- pairs(covariates,
        cex = 0.5,
        cex.labels = 0.9,
      main = "Scatterplot Matrix of Covariates")

# 5. Boxplots by quality of recording
##reading data in
scored_data <- list.files(path = "C:/Users/KokoszkaN/Desktop/recordings", pattern = "\\.csv$", full.names = TRUE) ##from my directory but can paste with folder from git
scored_datadf <- read_csv(scored_data)

##adding "acceptable" column to df
df_scored <- left_join(df, select(scored_datadf, file, acceptable), by = "file")

 df_scored |>
   ##selecting acceptable and covariates
  select(acceptable, lowfreq_ratio, rms, bgn_bird_med, bgn_bird_max, bgn_low_med,
         pow_bird_med, pow_bird_max, pow_low_med, ent_bird_med, ent_bird_max, ent_low_med) |>
   ##filtering out malfunction (m)
    filter(acceptable %in% c("y", "n", "b")) |> 
   ##adding labels
    mutate(acceptable = factor(acceptable,
                             levels = c("y", "n", "b"),
                             labels = c("Good", "Bad", "Borderline"))) |> 
   ##changing the df from every covariate having its own column to all covariates being in the variable column
   pivot_longer(cols = c(lowfreq_ratio, rms, bgn_bird_med, bgn_bird_max, bgn_low_med,
               pow_bird_med, pow_bird_max, pow_low_med, ent_bird_med, ent_bird_max, ent_low_med),
               names_to = "variable",
                          values_to = "value") |> 
   ##reordering how the boxplots order appears
    mutate(variable = factor(variable, levels = c(
     "bgn_bird_med", "pow_bird_med", "ent_bird_med", "lowfreq_ratio", ##row 1
     "bgn_bird_max", "pow_bird_max", "ent_bird_max", "rms", ##row 2
     "bgn_low_med", "pow_low_med", "ent_low_med" ##row 3
   ))) |> 
   ##creating boxplots
   ggplot(aes(x = acceptable, y = value, fill = acceptable)) +
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
 