ask <- function(prompt, choices = NULL){
  repeat{
    ans <- trimws(tolower(readline(prompt = prompt)))
    if(is.null(choices) || ans %in% choices) return(ans)
    cat("Please enter one of:", paste(choices, collapse = ", "), "\n")
  }
}

# Where Audacity keeps its settings on a Mac     #bhavya
audacity_cfg <- path.expand("~/Library/Application Support/audacity/audacity.cfg")

# Find the row of one setting inside its section (NA if it isn't there) bhavya
find_setting_row <- function(lines, section, key){
  sec_row <- which(lines == paste0("[", section, "]"))[1]
  if(is.na(sec_row)) return(NA)
  rows <- seq_along(lines)
  next_sec <- which(grepl("^\\[", lines) & rows > sec_row)[1]
  sec_end <- if(is.na(next_sec)) length(lines) else next_sec - 1
  which(startsWith(lines, paste0(key, "=")) & rows > sec_row & rows <= sec_end)[1]
}

# Split "Section/name=value" into its parts #bhavya
split_setting <- function(s){
  path <- sub("=.*", "", s)
  list(path    = path,
       value   = sub("^[^=]*=", "", s),
       section = sub("/[^/]*$", "", path),
       key     = sub(".*/", "", path))
}

# Write the project's settings into Audacity, leaving all other settings alone.
# Before changing anything, it remembers the person's own values so they can be
# put back later with restore_audacity_settings().    #bhavya
# Audacity must be CLOSED when this runs
set_audacity_settings <- function(settings){
  dir.create(dirname(audacity_cfg), recursive = TRUE, showWarnings = FALSE)
  lines <- if(file.exists(audacity_cfg)) readLines(audacity_cfg, warn = FALSE) else character(0)
  
  # remember the person's own values (only the first time, so running this
  # twice doesn't overwrite their real settings with the project ones) #bhavya
  if(!exists("audacity_backup", envir = .GlobalEnv)){
    backup <- character(0)
    for(s in settings){
      p <- split_setting(s)
      row <- find_setting_row(lines, p$section, p$key)
      backup[p$path] <- if(is.na(row)) NA else sub("^[^=]*=", "", lines[row])
    }
    assign("audacity_backup", backup, envir = .GlobalEnv)
  }
  
  for(s in settings){
    p <- split_setting(s)
    new_line <- paste0(p$key, "=", p$value)
    sec_row <- which(lines == paste0("[", p$section, "]"))[1]
    if(is.na(sec_row)){                        # section missing: add it at the end
      lines <- c(lines, paste0("[", p$section, "]"), new_line)
      next
    }
    row <- find_setting_row(lines, p$section, p$key)
    if(is.na(row)) lines <- append(lines, new_line, after = sec_row)   # add setting
    else lines[row] <- new_line                                         # replace setting
  }
  writeLines(lines, audacity_cfg)
  cat("Project Audacity settings applied.\n")
  invisible(TRUE)
}

# Put the person's own Audacity settings back the way they were. #bhavya
# Audacity must be CLOSED when this runs
restore_audacity_settings <- function(){
  if(!exists("audacity_backup", envir = .GlobalEnv)){
    cat("Nothing to restore - project settings were not applied in this R session.\n")
    return(invisible(FALSE))
  }
  backup <- get("audacity_backup", envir = .GlobalEnv)
  lines <- readLines(audacity_cfg, warn = FALSE)
  
  for(path in names(backup)){
    p <- split_setting(paste0(path, "=", backup[[path]]))
    row <- find_setting_row(lines, p$section, p$key)
    if(is.na(backup[[path]])){
      if(!is.na(row)) lines <- lines[-row]      # setting didn't exist before: remove it
    } else if(is.na(row)){
      sec_row <- which(lines == paste0("[", p$section, "]"))[1]
      lines <- append(lines, paste0(p$key, "=", backup[[path]]), after = sec_row)
    } else {
      lines[row] <- paste0(p$key, "=", backup[[path]])   # put old value back
    }
  }
  writeLines(lines, audacity_cfg)
  rm("audacity_backup", envir = .GlobalEnv)
  cat("Your own Audacity settings have been restored.\n")
  invisible(TRUE)
}
score_recordings <- function(noise_metrics, out_file = "scored_recordings.csv"){
  
  # Create columns only if they don't already exist (allows resuming)
  if(!"acceptable" %in% names(noise_metrics)) noise_metrics$acceptable <- NA_character_
  if(!"clipping" %in% names(noise_metrics)) noise_metrics$clipping <- NA_character_
  if(!"insect_amphibian" %in% names(noise_metrics)) noise_metrics$insect_amphibian <- NA_character_
  for(i in seq_len(nrow(noise_metrics))){
    
    # Skip recordings that were already scored
    if(!is.na(noise_metrics$acceptable[i])) next
    
    cat("\n---------------------------------\n")
    cat("Recording", i, "of", nrow(noise_metrics), "\n")
    cat(basename(noise_metrics$file[i]), "\n")
    cat("---------------------------------\n")
    
    system(paste(shQuote(audacity_path), shQuote(noise_metrics$file[i])), wait = FALSE)
    
    # Q1: acceptable?
    answer <- ask(
      "Acceptable for processing? (y = yes, n = no, b = borderline, m = malfunction, x = stop): ",
      c("y", "n", "b", "m", "x")
    )
    
    if(answer == "x"){
      cat("Scoring stopped by user.\n")
      break
    }
    
    # Q2: clipping (always asked)
    noise_metrics$clipping[i] <- ask(
      "Is clipping (>5%) present in the recording? (y/n): ",
      c("y", "n")
    )
    
    # Q3: amphibians/insects (always asked)
    noise_metrics$insect_amphibian[i] <- ask(
      "Is excessive (>50%) insect or amphibian noise present? (y/n): ",
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
