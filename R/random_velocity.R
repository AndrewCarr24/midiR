random_velocity <- function(seq_arg = NULL, position = NULL, v_map = NULL, track_apply = NULL){

  instruments <- purrr::map(seq_arg, ~attr(.x, "meta"))

  if(is.null(seq_arg) | seq_arg[1] == "rand_seq"){
    obj <- as.list(match.call())
    obj$func <- sys.function()
    return(obj)
  }

  # Selecting tracks(s) to apply modification to
  track_arg = rep(TRUE, length(seq_arg))
  if(!is.null(track_apply)){
    track_arg = rep(FALSE, length(seq_arg))
    track_arg[track_apply] = TRUE
  }


  lst <- purrr::pmap(list(seq_arg, track_arg, instruments), function(seq, track, instr){

    #
    if(!track){
      return(seq)
    }else{

      mappings = template_and_builder_aux(seq = seq, position = position, prob = 1, mapper = v_map)

      pos_mappings = mappings[[1]]
      v_mappings = mappings[[2]]
      v_mappings = v_mappings[!pos_mappings %in% which(seq == "rest")]
      pos_mappings = pos_mappings[!pos_mappings %in% which(seq == "rest")]

      v_seq <- rep(NA, length(seq))

      v_seq[pos_mappings] <- purrr::pmap(list(pos_mappings, v_mappings), function(x, y){
        y %>% as.hexmode() %>% as.character()})

      v_seq <- v_seq %>% unlist

      attr(seq,  "class") <- "seq"
      attr(seq, "meta") <- instr
      attr(seq, "v") <- v_seq

      return(seq)

      }

  })

  return(lst)

}


