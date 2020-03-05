# Helper function - map drum to note
drum_to_note <- function(drum){
  if(drum == "hh"){return("F#1")}else if(drum == "kick"){return("C1")}else if(drum == "snare"){return("D1")
  }else if(drum == "openhh"){return("Bb1")}
}


# Drum Shifter
drum_shifter <- function(seq_arg = NULL, shift = NULL, prob = 1, position = "all", track_apply = NULL, ...){

  if(is.null(seq_arg) | seq_arg[1] == "rand_seq"){
    obj <- as.list(match.call())
    obj$func <- sys.function()
    return(obj)
  }

  # Handling non-list seq arguments
  if(!is.list(seq_arg)){
    seq_arg <- list(seq_arg)
  }

  track_arg = rep(TRUE, length(seq_arg))
  if(!is.null(track_apply)){
    track_arg = rep(FALSE, length(seq_arg))
    track_arg[track_apply] = TRUE
  }

  if(is.null(shift)){
    stop("Please enter an integer value in the shift parameter.")
  }

  purrr::map2(seq_arg, track_arg, function(seq, track){

    if(!track){
      return(seq)
    }else{

      mappings = template_and_builder_aux(seq = seq, position = position, prob = prob)
      pos_mappings = mappings[[1]]
      prob_mappings = mappings[[2]]
      prob_mappings = prob_mappings[!pos_mappings %in% which(seq == "rest")]
      pos_mappings = pos_mappings[!pos_mappings %in% which(seq == "rest")]

      seq[pos_mappings] <- purrr::pmap(list(pos_mappings, prob_mappings), function(x, y){

        sample(c(shift, seq[x]), 1, prob = c(y, 1-y))

      }) %>% unlist

      return(seq)
    }
  })

}

