## Classify recordings

library(tuneR)

# CODE AUDACITY SETTINGS DIRECTLY?? Use the settings in this pdf

# https://sk.birdatlas.ca/wp-content/uploads/2018/12/Audacity-Specs.pdf

# set the path to where you have Audacity installed on your computer.

setWavPlayer(shQuote("C:/Program Files/Audacity/Audacity.exe"))

audacity_path <- "C:/Program Files/Audacity/Audacity.exe"

# load classify function

source("classify_function_v2.R")

# read in sampled file csv
# NOTE: IF YOU ARE RESUMING WORK ON THE SAME FILE, SKIP TO CODE LINE 25

noise_metrics <- read_csv("CWS_PRA_SBCR22_model_train_recording_sample.csv")

#### begin recording review/scoring
# follow the prompts in the Console below

scored <- score_recordings(noise_metrics, out_file = "data/CWS_PRA_SBCR22_scored_recordings.csv")


###############################################
# RESUMING A PARTIALLY COMPLETE FILE
###############################################

noise_metrics <- read_csv("data/CWS_PRA_SBCR22_scored_recordings.csv")

scored <- score_recordings(noise_metrics)
