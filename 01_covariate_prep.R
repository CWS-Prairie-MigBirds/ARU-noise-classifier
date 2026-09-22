#### Developing a bespoke recognizer of good vs poor recordings
#### Steven Van Wilgenburg

library(tuneR)
library(seewave)
library(soundecology)
library(dplyr)
library(tuneR)
library(ARUtools)
library(ARUtoolsExtra)
library(Ruido)


install.packages("Ruido")


#setwd("D:/BBMP/2026/") #### set the working directory to the location of recordings on hard drive

# Root directory to search
startingDir <- "D:/BBMP/2026/"

# Find all files in all subdirectories containing the prefix
filez <- list.files(
  path = startingDir,
  pattern = "*.wav",
  recursive = TRUE,
  full.names = TRUE
)

head(filez)

 
## set the number of .wav files to sample from each sub-folder (assuming unique 
# sites are stored in their own sub-folders, this can be used to generate samples 
#with n samples per location)

sample_size <- 6  #if using more than 1 recording per spatial location (folder) 
                  #then maybe get >= 6 so random effects can be included to account 
                  #for repeated measures

file_df <- data.frame(
  file = filez,
  folder = dirname(filez)
)

sampled_files <- file_df %>%
  group_by(folder) %>%
  sample_n(size = min(sample_size, n())) %>%
  ungroup()
  
head(sampled_files)

#### Function to extract metrics of wind
# NAs can be produced by ACI because implementations use a logarithmic 
# transformation internally and if a file or a frequency bin contains only zeros 
# then log(0) = -Inf

### set a standardized recording length to read in (in seconds)
standardlength <- 180 #### 3 minute recording

get_noise_metrics <- function(files){

  bind_rows(
    lapply(files, function(file){

      tryCatch({

        # Read WAV
        t_read <- system.time({
          wav <- readWave(
            file,
            from = 0,
            to = standardlength,
            units = "seconds"
          )
        })

        # Mean spectrum
        t_spec <- system.time({

          spec <- meanspec(
            wav,
            plot = FALSE
          )

          freq_hz <- spec[,1] * 1000

          lowfreq_ratio <-
            sum(spec[freq_hz < 250, 2]) /
            sum(spec[,2])

        })

        # RMS amplitude
        t_rms <- system.time({

          rms_val <- seewave::rms(wav@left)

        })

        # Ruido background noise and soundscape power
        t_BGN <- system.time({

          bgn_out <- Ruido::bgNoise(
            wav,
            channel = "stereo",
            timeBin = 60,
            dbThreshold = -90
          )

          bgn_val <- max(
            aci_out$AciTotAll_left,
            aci_out$AciTotAll_right,
            na.rm = TRUE
          )

        })

        # Ruido spectral temporal entropy index
        t_ENT <- system.time({
          
          ent_out <- Ruido::ENTspec(
            wav,
            channel = "stereo",
            timeBin = 60
          )
          
          ent_val <- max(
            aci_out$AciTotAll_left,
            aci_out$AciTotAll_right,
            na.rm = TRUE
          )
          
        })
        
        data.frame(
          file = file,
          folder = dirname(file),

          lowfreq_ratio = lowfreq_ratio,
          rms = rms_val,
          bgn = bgn_val,
          ent = ent_val,

          time_read = t_read["elapsed"],
          time_spec = t_spec["elapsed"],
          time_rms = t_rms["elapsed"],
          time_bgn = t_bgn["elapsed"],
          time_ent = t_ent["elapsed"],
          time_total =
            t_read["elapsed"] +
            t_spec["elapsed"] +
            t_rms["elapsed"] +
            t_bgn["elapsed"] +
            t_ent["elapsed"]
        )

      }, error = function(e){

        message(
          "Failed: ",
          basename(file),
          " | Reason: ",
          e$message
        )

        data.frame(
          file = file,
          folder = dirname(file),

          lowfreq_ratio = NA_real_,
          rms = NA_real_,
          bgn = NA_real_,
          ent = NA_real_,

          time_read = NA_real_,
          time_spec = NA_real_,
          time_rms = NA_real_,
          time_bgn = NA_real_,
          time_ent = NA_real_,
          time_total = NA_real_
        )

      })

    })
  )
}

system.time(get_noise_metrics(sampled_files$file[2]))


noise_metrics <- get_noise_metrics(sampled_files$file)

head(noise_metrics)

noise_metrics <- noise_metrics %>%
  mutate(
    z_lowfreq = scale(lowfreq_ratio)[,1],
    z_rms = scale(rms)[,1],
    z_aci = scale(aci)[,1],
    WCI = z_lowfreq + z_rms - z_aci
  ) %>%
  arrange(desc(WCI))


