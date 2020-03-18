cc_parm = "05"
cc_map = cc_mapper(30, 70)
position = template(1, 3, every = 4)
track_apply = 1
seq_arg <- drum_machine(hh = 1:16, kick = seq(1, 16, 4), snare = c(5, 13))


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

  mappings = template_and_builder_aux(seq = seq, position = position, prob = prob, mapper = cc_map)
  pos_mappings = mappings[[1]]
  prob_mappings = mappings[[2]]
  prob_mappings = prob_mappings[!pos_mappings %in% which(seq == "rest")]
  pos_mappings = pos_mappings[!pos_mappings %in% which(seq == "rest")]


  cc_seq <- rep(NA, length(seq))

  cc_seq[pos_mappings] <- purrr::map(prob_mappings, function(y){
    paste0("CC-", y %>% as.hexmode() %>% as.character(), "-", cc_parm)})

  cc_seq <- cc_seq %>% unlist

  attr(seq,  "class") <- "seq"
  attr(seq, "meta") <- instr
  attr(seq, "cc") <- cc_seq

  return(seq)

    }

  })

  return(lst)

}



