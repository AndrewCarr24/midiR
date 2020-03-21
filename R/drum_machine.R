#' Create drum sequence.
#'
#' @description Creates drum sequence as named  list.  Emulates a 16-step drum sequencer.
#' @param hh (optional) Takes numbers from 1 to 16, which represent the hi-hat's positions in a 16-step sequence.
#' @param kick (optional) Takes numbers from 1 to 16, which represent the kick's positions in a 16-step sequence.
#' @param snare (optional) Takes numbers from 1 to 16, which represent the snare's positions in a 16-step sequence.
#' @importFrom magrittr %>%
#' @return A named list with notes in positions corresponding to the hits.  This list has 'seq' as its class attribute, which means that it can be fed into the create_midi function to be converted into a hexadecimal MIDI sequence.
#' @examples
#' drum_machine(hh = 1:16, kick = seq(1,16, by = 4),
#' snare = c(5, 13))
#'
#'
drum_machine <- function(..., note_length = NULL, instrument = "Synth Drum", rep = 1){

  args <- list(...)

  if(any(args %>% unlist > 16 | args %>% unlist < 0)){
    stop("This is a 16-step drum machine.  Only enter integers between 1 and 16.")
  }

  # Assigning each distinct instrument to its own channel
  channel <- rep("C0", length(args))
  if(length(instrument) > 1){
    channel <- paste0("C", as.character(factor(instrument) %>% as.numeric - 1))
  }

  seqs <- purrr::pmap(list(args, names(args), instrument, channel), function(x, y, z, k){

    seq <- rep("rest", 16)

    if(y == "kick"){
      seq[x] = "C1"
    }else if(y == "hh"){
      seq[x] = "F#1"
    }else if(y == "snare"){
      seq[x] = "D1"
    }else if(y == "htom"){
      seq[x] = "A1"
    }else if(y %in% paste0(c("C", "C#", "D", "D#", "E", "F",
                             "F#", "G", "G#", "A", "Bb", "B"),
                           rep(seq(-2,8), each = 12))){
      seq[x] = y
    }

    seq <- rep(seq, rep)

    class(seq) = "seq"
    attr(seq, "note_length") = note_length
    attr(seq, "meta") = paste("00", k, instrument_to_hex(z))


    seq

  })

  return(seqs)

}


