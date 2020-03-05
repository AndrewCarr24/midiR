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

      seq[pos_mappings] <- pmap(list(pos_mappings, prob_mappings), function(x, y){
        sample(c('rest', seq[x]), 1, prob = c(y, 1-y))
      }) %>% unlist

      return(seq)
    }
  })

  if(length(lst) == 1){
    return(lst[[1]])}else{
      return(lst)
    }

}
