ask <- function(prompt, choices = NULL){
  repeat{
    ans <- trimws(tolower(readline(prompt = prompt)))
    if(is.null(choices) || ans %in% choices) return(ans)
    cat("Please enter one of:", paste(choices, collapse = ", "), "\n")
  }
}

score_recordings <- function(noise_metrics, out_file = "scored_recordings.csv"){
  
  # Create columns only if they don't already exist (allows resuming)
  if(!"acceptable" %in% names(noise_metrics)) noise_metrics$acceptable <- NA_character_
  if(!"noise_type" %in% names(noise_metrics)) noise_metrics$noise_type <- NA_integer_
  if(!"clipping" %in% names(noise_metrics)) noise_metrics$clipping <- NA_character_
  
  for(i in seq_len(nrow(noise_metrics))){
    
    # Skip recordings that were already scored
    if(!is.na(noise_metrics$acceptable[i])) next
    
    cat("\n---------------------------------\n")
    cat("Recording", i, "of", nrow(noise_metrics), "\n")
    cat(basename(noise_metrics$file[i]), "\n")
    cat("---------------------------------\n")
    
    play(noise_metrics$file[i])
    
    # Q1: acceptable?
    answer <- ask(
      "Acceptable for processing? (y = yes, n = no, b = borderline, x = stop): ",
      c("y", "n", "b", "x")
    )
    
    if(answer == "x"){
      cat("Scoring stopped by user.\n")
      break
    }
    
    # Q2: noise type (only if no or borderline)
    if(answer %in% c("n", "b")){
      cat("\nWhat is the dominant noise type?\n")
      cat(" 1. Wind\n")
      cat(" 2. Rain\n")
      cat(" 3. Running water\n")
      cat(" 4. Engines\n")
      cat(" 5. Explosions\n")
      noise_metrics$noise_type[i] <- as.integer(
        ask("Enter 1-5: ", as.character(1:5))
      )
    }
    
    # Q3: clipping (always asked)
    noise_metrics$clipping[i] <- ask(
      "Is clipping present in the recording? (y/n): ",
      c("y", "n")
    )
    
    # Only save the answers once all questions are complete
    noise_metrics$acceptable[i] <- answer
    write.csv(noise_metrics, out_file, row.names = FALSE)
  }
  
  write.csv(noise_metrics, out_file, row.names = FALSE)
  cat("\nSaved to", out_file, "\n")
  return(invisible(noise_metrics))
}
