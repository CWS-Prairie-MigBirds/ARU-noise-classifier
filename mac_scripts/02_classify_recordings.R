## Classify recordings

library(tuneR)
library(readr)  #bhavya

# CODE AUDACITY SETTINGS DIRECTLY?? Use the settings in this pdf

# https://sk.birdatlas.ca/wp-content/uploads/2018/12/Audacity-Specs.pdf

# set the path to where you have Audacity installed on your computer.

setWavPlayer("open -a Audacity")   #bhavya
# location of Audacity on a Mac
audacity_path <- "/Applications/Audacity.app/Contents/MacOS/Audacity"
#Load classify function
source("classify_function_v2.R")      #bhavya

# project Audacity settings, written as "Section/name=value"
# to change the project settings, edit this list only
# IMPORTANT: Audacity must be CLOSED before running set_audacity_settings()

audacity_settings <- c(
  "SamplingRate/DefaultProjectSampleRate=44100",               # default sample rate
  "SamplingRate/DefaultProjectSampleFormatChoice=Format32BitFloat", # default sample format
  "GUI/DefaultViewModeNew=Spectrogram",                        # default view mode
  "GUI/DefaultViewModeChoiceNew=Spectrogram",                  # default view mode (newer Audacity)
  "Spectrum/ScaleType=0",                                      # scale: 0 = linear
  "Spectrum/MinFreq=0",                                        # minimum frequency (Hz)
  "Spectrum/MaxFreq=12000",                                    # maximum frequency (Hz)
  "Spectrum/Gain=15",                                          # gain (dB)
  "Spectrum/Range=80",                                         # range (dB)
  "Spectrum/FrequencyGain=15",                                 # frequency gain (dB/dec)
  "Spectrum/FFTSize=2048",                                     # window size
  "Spectrum/WindowType=5",                                     # window type: 5 = Blackman-Harris
  "Spectrum/ZeroPaddingFactor=1"                               # zero padding factor
)
#bhavya, change settings here
set_audacity_settings(audacity_settings)
# set_audacity_settings(), THEN SKIP TO THE "RESUMING" SECTION BELOW
# read in sampled file csv
# NOTE: IF YOU ARE RESUMING WORK ON THE SAME FILE, SKIP TO CODE LINE 51
noise_metrics <- read_csv("CWS_PRA_SBCR22_model_train_recording_sample.csv")

#### begin recording review/scoring
# follow the prompts in the Console below

scored <- score_recordings(noise_metrics, out_file = "data/CWS_PRA_SBCR22_scored_recordings.csv")


###############################################
# RESUMING A PARTIALLY COMPLETE FILE
###############################################

noise_metrics <- read_csv("data/CWS_PRA_SBCR22_scored_recordings.csv")

scored <- score_recordings(noise_metrics)
