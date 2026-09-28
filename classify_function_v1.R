### Function to play the sampled files and have the user anwser whether or 
# not the recording is useful for processing  

score_recordings <- function(noise_metrics){
  noise_metrics$acceptable <- NA
  
  for(i in seq_len(nrow(noise_metrics))){
    cat("\n---------------------------------\n")
    cat("Recording", i, "of", nrow(noise_metrics), "\n")
    cat(basename(noise_metrics$file[i]), "\n")
    cat("---------------------------------\n")
    
    play(noise_metrics$file[i])
    
    repeat{
      answer <- tolower(
        readline(
          prompt = "Acceptable for processing? (y = yes, n = no, b = borderline x = stop): "
        )
      )
      
      if(answer %in% c("y","n", "b", "x")) break
      
      cat("Please enter y, n, b, or x.\n")
    }
    
    if(answer == "x"){
      
      cat("Scoring stopped by user.\n")
      break
      
    } else if(answer == "y"){
      
      noise_metrics$acceptable[i] <- "y"
      
    } else if(answer == "n"){
      
      noise_metrics$acceptable[i] <- "n"
      
    } else if(answer == "b"){
      
      noise_metrics$acceptable[i] <- "b"
      
    }
  }
  
  return(noise_metrics)
}


