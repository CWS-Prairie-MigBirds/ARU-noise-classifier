#### Function to extract metrics of wind

library(tidyverse)
library(tuneR)
library(seewave)
library(Ruido)

standardlength <- 180   # 3 minute recordings (seconds)

############
# Helpers ---------------------------------------------------------------
############

# Pull one index's matrix (freq bin x time bin) for one channel
get_mat <- function(obj, index, channel = 1){
  as.matrix(obj@values[[channel]][[index]])
}

# Collapse a matrix to per-recording values
# select the range of values most of our species are detected in
# pick the top end of ourlow frequency where wind and engine noise show up often

summarize_mat <- function(m, samp_rate, prefix,
                          bird_hz = c(1000, 12000), low_hz = 1000){
  
  m[!is.finite(m)] <- NA
  
  freqs <- (seq_len(nrow(m)) - 0.5) * (samp_rate / 2) / nrow(m)
  bird  <- freqs >= bird_hz[1] & freqs <= bird_hz[2]
  low   <- freqs < low_hz
  
  # median acros windows within each 60s time bin
  per_min <- apply(m[bird, , drop = FALSE], 2, median, na.rm = TRUE)
  
  out <- c(
    median(per_min, na.rm = TRUE),   # typical minute
    max(per_min, na.rm = TRUE),      # worst minute
    median(m[low, ], na.rm = TRUE)   # wind/traffic band
  )
  names(out) <- paste0(prefix, c("_bird_med", "_bird_max", "_low_med"))
  out
}

# Summarize every channel; returns channel-averaged + per-channel values
summarize_channels <- function(obj, index, prefix){
  
  chans <- names(obj@values)   # "left" (mono) or c("left","right")
  
  per_ch <- lapply(chans, function(ch){
    summarize_mat(get_mat(obj, index, ch), obj@sampRate, prefix)
  })
  
  sep <- unlist(lapply(seq_along(chans), function(i){
    x <- per_ch[[i]]
    names(x) <- sub(paste0("^", prefix),
                    paste0(prefix, "_", substr(chans[i], 1, 1)),
                    names(x))
    x
  }))
  
  avg <- colMeans(do.call(rbind, per_ch))
  
  c(avg, sep)
}

################
# Main function ---------------------------------------------------------
################

get_noise_metrics <- function(files){
  
  bind_rows(
    lapply(files, function(file){
      
      tryCatch({
        
        wav <- readWave(file, from = 0, to = standardlength, units = "seconds")
        ch_arg <- if (wav@stereo) "stereo" else "left"
        
        # Mean spectrum: proportion of energy below 250 Hz
        spec <- meanspec(wav, plot = FALSE)
        freq_hz <- spec[, 1] * 1000
        lowfreq_ratio <- sum(spec[freq_hz < 250, 2]) / sum(spec[, 2])
        
        # RMS amplitude
        rms_val <- seewave::rms(wav@left)
        
        # Ruido: background noise + soundscape power
        bgn_out  <- Ruido::bgNoise(wav, channel = ch_arg,
                                   timeBin = 60, dbThreshold = -90)
        bgn_vals <- summarize_channels(bgn_out, "BGN", "bgn")
        pow_vals <- summarize_channels(bgn_out, "POW", "pow")
        
        # Ruido: spectral temporal entropy
        ent_out  <- Ruido::ENTspec(wav, channel = ch_arg, timeBin = 60)
        ent_vals <- summarize_channels(ent_out, ent_out@index[1], "ent")
        
        data.frame(
          file = file,
          folder = dirname(file),
          lowfreq_ratio = lowfreq_ratio,
          rms = rms_val,
          t(bgn_vals),
          t(pow_vals),
          t(ent_vals),
          error = NA_character_
        )
        
      }, error = function(e){
        data.frame(file = file, folder = dirname(file),
                   error = conditionMessage(e))
      })
    })
  )
}

#########
# Run -------------------------------------------------------------------
#########

sample <- read_csv("data/sample/CWS_PRA_BRNWA25_model_train_recording_sample.csv")

timing <- system.time({
  metrics <- get_noise_metrics(sample$file)
})

timing # how long did it take to run


