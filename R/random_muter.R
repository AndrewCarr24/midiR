#' Randomly mute MIDI sequence
#'
#' @description Takes sequence produced by drum_machine function and applies mutes to this sequence.
#' @param seq_arg The sequence to be modified.  This should be the output of the drum_machine function.
#' @param prob (default = 1) The probability a note in the sequence will be muted.
#' @param position (default = "all") The position of items in the sequence to be muted.
#' @param track_apply An integer or vector of integers indicating which tracks (hh, kick, snare) to apply the mute to.
#' @return A modified MIDI sequence to be entered into the create_midi function.
#' @examples
#'# Applies mutes to first track (hh, or hi-hat) of drum sequence with .5 probability.
#'
#' drum_machine(hh = 1:16, kick = seq(1, 16, by = 4), snare = c(5, 13)) %>%
#' random_muter(., prob = .5, track_apply = 1)
#'
#'
random_muter <- function(seq_arg = NULL, prob = 1, position = "all", track_apply = NULL){

  if(is.null(seq_arg) | seq_arg[1] == "rand_seq"){
    obj <- as.list(match.call())
    obj$func <- sys.function()
    return(obj)
  }

  # Handling non-list seq arguments
  if(!is.list(seq_arg)){
    seq_arg <- list(seq_arg)
  }

  # Selecting tracks(s) to apply mutes to
  track_arg = rep(TRUE, length(seq_arg))
  if(!is.null(track_apply)){
    track_arg = rep(FALSE, length(seq_arg))
    track_arg[track_apply] = TRUE
  }

  lst <- purrr::map2(seq_arg, track_arg, function(seq, track){

    # Dealing
    if(!track){
      return(seq)
    }else{

      mappings = template_and_builder_aux(seq = seq, position = position, prob = prob)

      pos_mappings = mappings[[1]]
      prob_mappings = mappings[[2]]

      seq[pos_mappings] <- purrr::pmap(list(pos_mappings, prob_mappings), function(x, y){
        sample(c('rest', seq[x]), 1, prob = c(y, 1-y))
      }) %>% unlist

      return(seq)
    }
  })

  #if(length(lst) == 1){
  #  return(lst[[1]])}else{
      return(lst)
  #  }

}
