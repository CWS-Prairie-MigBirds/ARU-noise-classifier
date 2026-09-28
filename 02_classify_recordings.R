## Classify recordings

library(tuneR)

# set the path to where you have Audacity installed on your computer.

setWavPlayer(shQuote("C:/Program Files/Audacity/Audacity.exe"))

# load classify function

source("classify_function_v2.R")

# read in sampled file csv
# NOTE: IF YOU ARE RESUMING WORK ON THE SAME FILE, SKIP TO CODE LINE 25

noise_metrics <- read_csv("CWS_PRA_model_train_recording_sample.csv")


#### begin recording review/scoring
# follow the prompts in the Console below

scored <- score_recordings(noise_metrics, out_file = "scored_recordings.csv")


# RESUMING A PARTIALLY COMPLETE FILE

noise_metrics <- read_csv("scored_recordings.csv")

scored <- score_recordings(noise_metrics)
