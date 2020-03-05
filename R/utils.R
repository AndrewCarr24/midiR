## Template Function
template <- function(..., every = 4){

  return(list(..., every, "template"))

}

## Builder Function
builder <- function(..., path = "linear"){

  return(list(..., path, "builder"))

}

## CC Function
cc_mapper <- function(...){

  return(list(..., "cc"))

}

# Turns template and builder lists into position and probability mappings (for use in randomizer functions)
template_and_builder_aux <- function(seq = seq, position = position, prob = prob, mapper = NULL){

  if(is.list(position)){bar = position[[length(position) - 1]]
  pos_nums = position[1:(length(position) - 2)]}

  if(is.list(prob)){probs = prob[1:(length(prob) - 2)] %>% unlist
  prob_nums =  purrr::map(seq(1, length(probs), 2), ~probs[c(.x, .x+1)])}

  if(!is.null(mapper)){
    stopifnot(position[length(position)] == "template" & mapper[length(mapper)] == "cc")
    cc_mappings <- rep(mapper[1:(length(mapper)-1)], bar)
  }

  if(position[length(position)] == "template"){
    pos_mappings = purrr::map(pos_nums, ~seq(.x, length(seq), bar))
  }else{
    pos_mappings = 1:length(seq)
  }

  if(prob[length(prob)] == "builder"){
    prob_mappings = purrr::map2(list(pos_mappings %>% unlist), prob_nums, ~seq(.y[[1]], .y[[2]], (.y[2] - .y[1])/(length(.x)-1)))
  }else{
    prob_mappings = rep(prob, length(pos_mappings %>% unlist))
  }

  if(exists("cc_mappings")){
    return(list(sort(pos_mappings %>% unlist), cc_mappings %>% unlist))
  }else{
    return(list(sort(pos_mappings %>% unlist), prob_mappings %>% unlist))
  }
}


