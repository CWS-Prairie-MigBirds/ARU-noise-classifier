#### Developing a bespoke recognizer of good vs poor recordings
#### Steven Van Wilgenburg
# Janine McManus

library(tidyverse)
library(lubridate)

# Root directory to search
startingDir <- "D:/ARU RECORDINGS/2022/BC/BC-15"

# Find all files in all subdirectories containing the prefix
filez <- list.files(
  path = startingDir,
  pattern = "\\.wav$",
  recursive = TRUE,
  full.names = TRUE
)

head(filez)


filez <- map_dfr(filez, ~as.data.frame(t(.x)))

filez <- filez %>% rename(file = V1) %>% 
  mutate(name = tools::file_path_sans_ext(file),
         parts = str_split(name, "_"),
         site = map_chr(parts, 1),
         date = ymd(map_chr(parts, 2)),
         time = hms::as_hms(strptime(map_chr(parts, 3), "%H%M%S")),
         datetime = ymd_hms(paste(date, time)),
         hour = hour(datetime),
         period = case_when(hour>= 3 & hour < 8 ~ "dawn",
                            hour >= 8 & hour < 12 ~ "morning",
                            hour >= 12 & hour < 18 ~ "afternoon",
                            hour >=18 & hour < 22 ~ "dusk",
                            TRUE ~ "night")
  ) %>% 
  select(-parts)


# JMM: For now we are including all dates available, randomly selecting 3 "dawn" recordings and
# and 3 dusk recordings per location. In order to have more accurate time windows we would
# need to upload a location table to get sunrise and sunset. I think these broad categories
# work for now.

set.seed(33)

sampled <- filez %>% 
  filter(period %in% c("dawn", "dusk")) %>% 
  group_by(site, period) %>% 
  slice_sample(n = 3) %>% ungroup()

# add broad habitat type based on the program who deployed the ARUs

sampled$habitat <- "grassland"

# save csv for step 2

write_csv(sampled, "CWS_PRA_SBCR22_model_train_recording_sample.csv")
