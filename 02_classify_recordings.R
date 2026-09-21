etWavPlayer(shQuote("C:/Program Files/Audacity/Audacity.exe"))


#### begin recording review/scoring
noise_metrics <- score_recordings(noise_metrics)

#### backfill NA values using median
noise_metrics <- noise_metrics %>%
  mutate(
    z_aci = ifelse(
      is.na(z_aci),
      median(z_aci, na.rm = TRUE),
      z_aci
    )
  )

