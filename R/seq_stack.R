#' Stack sequences
#'
#' @description Takes two drum sequences and stacks them to combine their parts.
#' @param seq1 A named list of seq objects.  The output of the drum_machine function.
#' @param seq2 A named list of seq objects.  The output of the drum_machine function.
#' @return A named list of seq objects.  This sequence contains the parts of both input sequences.
#' @examples
#' drum_machine(hh = 1:16) %>%
#'      seq_stack(., drum_machine(kick = seq(1, 16, by = 4), snare = c(5, 13)))
#'
seq_stack <- function(seq1, seq2){

  # Make sure arguments are lists of seq objects
  if(!is.list(seq1) | any(purrr::map(seq1, ~class(.x)) != "seq") |
     !is.list(seq2) | any(purrr::map(seq2, ~class(.x)) != "seq"))
    stop("This function only takes lists of sequences.")

  # Make sure sequences have different parts
  if(any(names(seq2) %in% names(seq1)))
    stop("Only sequences with differently named elements can be stacked.")

  # Make sure sequences are equal length
  if(length(seq1[[1]]) != length(seq2[[1]]))
    stop("Only sequences of equal length can be stacked.")

  return(c(seq1, seq2))

}
