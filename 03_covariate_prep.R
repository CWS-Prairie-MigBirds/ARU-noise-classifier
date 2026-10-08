#### Function to extract metrics of wind
# NAs can be produced by ACI because implementations use a logarithmic 
# transformation internally and if a file or a frequency bin contains only zeros 
# then log(0) = -Inf

library(tidyverse)
library(tuneR)
library(seewave)
library(soundecology)
library(dplyr)
library(tuneR)
library(ARUtools)
library(ARUtoolsExtra)
library(Ruido)


sample <- read_csv("CWS_PRA_BRNWA25_model_train_recording_sample.csv")

### set a standardized recording length to read in (in seconds)
standardlength <- 180 #### 3 minute recording


##function to extract netrics of noise

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

          bgn_val <- ??? #### What do we want to extract out of the noise matrix here?
          )

        })

        # Ruido spectral temporal entropy index
        t_ENT <- system.time({
          
          ent_out <- Ruido::ENTspec(
            wav,
            channel = "stereo",
            timeBin = 60
          )
          
          ent_max1 <- max(
            ent_out@values$left$ENT$ENT1,
            ent_out@values$right$ENT$ENT1,
            na.rm = TRUE
          )
          
          ent_max2 <- max(
            ent_out@values$left$ENT$ENT2,
            ent_out@values$right$ENT$ENT2,
            na.rm = TRUE
          )
          
          ent_max3 <- max(
            ent_out@values$left$ENT$ENT3,
            ent_out@values$right$ENT$ENT3,
            na.rm = TRUE
          )
          
          ent_mean1 <- mean(
            ent_out@values$left$ENT$ENT1,
            ent_out@values$right$ENT$ENT1,
            na.rm = TRUE
          )
          
          ent_mean2 <- mean(
            ent_out@values$left$ENT$ENT2,
            ent_out@values$right$ENT$ENT2,
            na.rm = TRUE
          )
          
          ent_mean3 <- mean(
            ent_out@values$left$ENT$ENT3,
            ent_out@values$right$ENT$ENT3,
            na.rm = TRUE
          )
          
        })
        
        data.frame(
          file = file,
          folder = dirname(file),

          lowfreq_ratio = lowfreq_ratio,
          rms = rms_val,
          bgn = ??????, #UPDATE
          ent_max1 = ent_max1,
          ent_max2 = ent_max2,
          ent_max3 = ent_max3,
          ent_mean1 = ent_mean1,
          ent_mean2 = ent_mean2,
          ent_mean3 = ent_mean3,

          time_read = t_read["elapsed"],
          time_spec = t_spec["elapsed"],
          time_rms = t_rms["elapsed"],
          time_bgn = t_BGN["elapsed"],
          time_ent = t_ENT["elapsed"],
          time_total =
            t_read["elapsed"] +
            t_spec["elapsed"] +
            t_rms["elapsed"] +
            t_BGN["elapsed"] +
            t_ENT["elapsed"]
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
          bgn = NA_real_, ##UPDATE
          ent_max1 = NA_real_,
          ent_max2 = NA_real_,
          ent_max3 = NA_real_,
          ent_mean1 = NA_real_,
          ent_mean2 = NA_real_,
          ent_mean3 = NA_real_,

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

#test run time

system.time(get_noise_metrics(sample$file[2]))

#run on full sample of recordings
noise_metrics <- get_noise_metrics(sample$file)

head(noise_metrics)

#save file
write_csv(noise_metrics, "CWS_PRA_BRNWA25_model_train_recording_sample.csv")

