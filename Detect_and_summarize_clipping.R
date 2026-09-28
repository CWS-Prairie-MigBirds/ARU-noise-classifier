library(tuneR)

# ============================================================
# Detect clipping in a single channel
# ============================================================

detect_channel_clipping <- function(
    x,
    file,
    channel,
    sample_rate,
    min_samples = 2) {

  # Identify samples at the 16-bit digital limits
  clipped <- x >= 32767 | x <= -32768

  # Find consecutive runs
  r <- rle(clipped)

  end_sample <- cumsum(r$lengths)
  start_sample <- end_sample - r$lengths + 1

  # Only retain clipping runs of specified minimum length
  keep <- r$values & r$lengths >= min_samples

  # Event-level results
  if (any(keep)) {

    events <- data.frame(
      file = basename(file),
      channel = channel,
      start_sample = start_sample[keep],
      end_sample = end_sample[keep],
      n_samples = r$lengths[keep],
      start_time_s =
        (start_sample[keep] - 1) / sample_rate,
      end_time_s =
        end_sample[keep] / sample_rate,
      duration_ms =
        r$lengths[keep] / sample_rate * 1000
    )

  } else {

    events <- data.frame(
      file = character(0),
      channel = character(0),
      start_sample = integer(0),
      end_sample = integer(0),
      n_samples = integer(0),
      start_time_s = numeric(0),
      end_time_s = numeric(0),
      duration_ms = numeric(0)
    )
  }

  # File/channel-level summary
  summary <- data.frame(
    file = basename(file),
    channel = channel,
    duration_s = length(x) / sample_rate,

    # Total number of samples exactly at digital limit
    n_samples_at_limit = sum(clipped),

    # Proportion of entire recording at digital limit
    pct_samples_at_limit =
      100 * sum(clipped) / length(x),

    # Number of clipping runs meeting minimum criterion
    n_clip_events = sum(keep),

    # Number of clipped samples contained in those events
    n_samples_in_events =
      if (any(keep))
        sum(r$lengths[keep])
      else
        0,

    # Longest clipping run
    longest_event_samples =
      if (any(keep))
        max(r$lengths[keep])
      else
        0,

    longest_event_ms =
      if (any(keep))
        max(r$lengths[keep]) / sample_rate * 1000
      else
        0
  )

  list(
    events = events,
    summary = summary
  )
}


# ============================================================
# Process one WAV
# ============================================================

process_wav <- function(file, min_samples = 2) {

  message("Processing: ", basename(file))

  w <- readWave(file)

  # Verify 16-bit
  if (w@bit != 16) {
    warning(
      paste("Skipping non-16-bit file:", file)
    )
    return(NULL)
  }

  # Check sample rate
  if (w@samp.rate != 44100) {
    warning(
      paste(
        basename(file),
        "has sample rate",
        w@samp.rate,
        "Hz"
      )
    )
  }

  # Left channel
  left <- detect_channel_clipping(
    x = w@left,
    file = file,
    channel = "left",
    sample_rate = w@samp.rate,
    min_samples = min_samples
  )

  if (w@stereo) {

    # Right channel
    right <- detect_channel_clipping(
      x = w@right,
      file = file,
      channel = "right",
      sample_rate = w@samp.rate,
      min_samples = min_samples
    )

    events <- rbind(
      left$events,
      right$events
    )

    summary <- rbind(
      left$summary,
      right$summary
    )

  } else {

    events <- left$events
    summary <- left$summary
  }

  list(
    events = events,
    summary = summary
  )
}


# ============================================================
# Process all WAV files in directory
# ============================================================

folder <- "D:/ARU_recordings"

files <- list.files(
  folder,
  pattern = "\\.wav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

results <- lapply(
  files,
  process_wav,
  min_samples = 2
)


# Remove any files that failed format checks
results <- results[
  !vapply(results, is.null, logical(1))
]


# ============================================================
# Combine results
# ============================================================

clipping_events <- do.call(
  rbind,
  lapply(results, function(z) z$events)
)

clipping_summary <- do.call(
  rbind,
  lapply(results, function(z) z$summary)
)


# ============================================================
# Save output
# ============================================================

write.csv(
  clipping_events,
  "clipping_events.csv",
  row.names = FALSE
)

write.csv(
  clipping_summary,
  "clipping_summary.csv",
  row.names = FALSE
)


# Look at worst recordings
clipping_summary <- clipping_summary[
  order(
    clipping_summary$n_samples_in_events,
    decreasing = TRUE
  ),
]

print(head(clipping_summary, 20))
