
#first, put the multiple uofs files together and join with noise metrics
## only needed to run this once to save the merged data

#file_list <- list.files(path = "data/scored/UofS/", pattern = "\\.csv$", full.names = TRUE)
#scores <- file_list[-3]
#df <- read_csv(scores, id = "source_file")
#nm <- read_csv("data/scored/UofS/Round_1_and_2_covariate_output_4_Nov.csv")
#nm <- nm %>% mutate(name = str_remove(file, "\\.wav$"))
#all <- left_join(df, nm, by = "name")
#uofs <- all %>% select(-file.y, -folder) %>% rename(file = file.x)
#write_csv(uofs, "data/metrics/UofS_grassland_2025_scored_metrics.csv")

uofs <- read_csv("data/metrics/UofS_grassland_2025_scored_metrics.csv")

uofs <- uofs %>% rename(amphibian_insect = insect_amphibian)


# Janine messed up and saved the metrics solo without the classifications, code 
# here just needs to run one time to create the complete dataset

scores <- list.files(path = "data/scored/", pattern = "\\.csv$", full.names = TRUE)

dfs <- read_csv(scores, id = "source_file")


metrics <- list.files(path = "data/metrics/", pattern = "\\.csv$", full.names = TRUE)

dfm <- read_csv(metrics, id = "source_file")

full <- left_join(dfs, dfm, by = "file")

full <- full %>% select(-source_file.x, -folder) %>% rename(source_file = source_file.y)

final <- rbind(full, uofs)

final <- final %>% drop_na(acceptable)

saveRDS(final, "data/complete_noise_dataset.rds")
