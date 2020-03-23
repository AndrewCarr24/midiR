random_cc <- function(seq_arg = NULL, cc_parm = NULL, cc_map = NULL, position = NULL, track_apply = NULL){

  instruments <- purrr::map(seq_arg, ~attr(.x, "meta"))

  # Selecting tracks(s) to apply modification to
  track_arg = rep(TRUE, length(seq_arg))
  if(!is.null(track_apply)){
    track_arg = rep(FALSE, length(seq_arg))
    track_arg[track_apply] = TRUE
  }


  lst <- purrr::pmap(list(seq_arg, track_arg, instruments), function(seq, track, instr){

    if(!track){
      return(seq)
    }else{

      mappings = template_and_builder_aux(seq = seq, position = position, prob = 1, mapper = cc_map)
      pos_mappings = mappings[[1]]
      prob_mappings = mappings[[2]]
      prob_mappings = prob_mappings[!pos_mappings %in% which(seq == "rest")]
      pos_mappings = pos_mappings[!pos_mappings %in% which(seq == "rest")]


      cc_seq <- rep(NA, length(seq))

      cc_seq[pos_mappings] <- purrr::map(prob_mappings, function(y){
        paste0(y %>% as.hexmode() %>% as.character(), "-", stringr::str_pad(as.character(cc_parm), 2, pad = "0"))})

      cc_seq <- cc_seq %>% unlist

      attr(seq,  "class") <- "seq"
      attr(seq, "meta") <- instr
      attr(seq, "cc") <- cc_seq

      return(seq)

    }

  })

  return(lst)

}
