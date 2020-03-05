random_modify <- function(seq_arg = NULL, modifier = NULL, prob = 1, position = "all", track_apply = NULL,
                          cc_parm = NULL, cc_map = NULL){

  mod_arg <- switch(modifier,
                    double = "d",
                    flam = "f",
                    roll = "l",
                    cc = "CC")

  if(is.null(seq_arg) | seq_arg[1] == "rand_seq"){
    obj <- as.list(match.call())
    obj$func <- sys.function()
    return(obj)
  }

  # Handling non-list seq arguments
  if(!is.list(seq_arg)){
    seq_arg <- list(seq_arg)
  }

  # Selecting tracks(s) to apply doubles to
  track_arg = rep(TRUE, length(seq_arg))
  if(!is.null(track_apply)){
    track_arg = rep(FALSE, length(seq_arg))
    track_arg[track_apply] = TRUE
  }

  lst <- purrr::map2(seq_arg, track_arg, function(seq, track){

    #
    if(!track){
      return(seq)
    }else{

      mappings = template_and_builder_aux(seq = seq, position = position, prob = prob, mapper = cc_map)

      pos_mappings = mappings[[1]]
      prob_mappings = mappings[[2]]
      prob_mappings = prob_mappings[!pos_mappings %in% which(seq == "rest")]
      pos_mappings = pos_mappings[!pos_mappings %in% which(seq == "rest")]


      if(mod_arg != "CC"){

        seq[pos_mappings] <- purrr::pmap(list(pos_mappings, prob_mappings), function(x, y){
          sample(c(paste0(seq[x], mod_arg), seq[x]), 1, prob = c(y, 1-y))}) %>% unlist

        # Fix items with two modifiers (remove second modifier)
        seq <- purrr::map(seq, function(x){
          if(x != "rest" & !"CC" %in% x) gsub("(?<=[a-z]{1})[a-z]{1}", "", x, perl = TRUE)
          else x
        }) %>% unlist

        attr(seq, "class") <- "seq"
        attr(seq, "meta") <- "00 C0 76"

        return(seq)

      }else{

        seq[pos_mappings] <- purrr::pmap(list(pos_mappings, prob_mappings), function(x, y){
          c(paste0("CC-", y, "-", cc_parm), seq[x])})

        seq <- seq %>% unlist

        attr(seq,  "class") <- "seq"
        attr(seq, "meta") <- "00 C0 76"

        return(seq)

      }}

  })

  return(lst)

}
