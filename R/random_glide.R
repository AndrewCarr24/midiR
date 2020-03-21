random_glide <-  function(seq_arg = NULL, prob = 1, position = "all", track_apply = NULL){

  if(is.null(seq_arg) | seq_arg[1] == "rand_seq"){
    obj <- as.list(match.call())
    obj$func <- sys.function()
    return(obj)
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

        if(length(seq) != x){
          if(seq[x] != seq[x+1] & seq[x] != "rest" & seq[x+1] != "rest"){
            sample(c(paste0(seq[x], "g"), seq[x]), 1, prob = c(y, 1-y))
          }else{seq[x]}
        }else{seq[x]}
      }) %>% unlist

      seq_ind <- rep(2, length(seq[grepl("g", seq)]))
      seq_ind[1] <- 1

      seq[grepl("g", seq)] <- purrr::map2(seq[grepl("g", seq)], seq_ind, function(x, y){

        stringr::str_replace(x, "g$", paste0("g", y))

      }) %>% unlist

      glide_end_ind <- (grep("g2$", seq) + 1)[!(grep("g2$", seq) + 1) %in% grep("g2$", seq)]
      glide_end_ind <- (grep("g1$", seq) + 1)[!(grep("g1$", seq) + 1) %in% grep("g2$", seq)]

      seq[glide_end_ind] <- paste0(seq[glide_end_ind], "g3")


      return(seq)
    }
  })


  return(lst)


}
