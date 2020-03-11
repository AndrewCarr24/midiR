#' Concatenate sequences
#'
#' @description Takes two drum sequences and concatenates them to create longer musical sequences.
#' @param seq1 A named list of seq objects.  The output of the drum_machine function.
#' @param seq2 A named list of seq objects.  The output of the drum_machine function.
#' @return A named list of seq objects.  Each element is a concatenated vector of corresponding elements in seq1 and seq2.
#' @examples
#' drum_machine(hh = 1:16, kick = seq(1,16, by = 4), snare = c(5, 13)) %>%
#'      seq_cat(., drum_machine(hh = seq(1, 16, by = 2), kick = c(1, 3, 5, 9, 11, 13), snare = c(5, 13)))
#'
seq_cat <- function(seq1, seq2){

  # Make sure arguments are lists of seq objects
  if(!is.list(seq1) | any(purrr::map(seq1, ~class(.x)) != "seq") |
     !is.list(seq2) | any(purrr::map(seq2, ~class(.x)) != "seq"))
    stop("This function only takes lists of sequences.")

  # Make sure object names are the same
  if(!identical(names(seq1), names(seq2)))
    stop("The arguments of this function must be lists that have elements with the same names.")

  # Elementwise concatenation of arguments.  Output of each concatenated element takes attributes of first argument.
  purrr::map2(seq1, seq2, function(x, y){

    x_class <- class(x)
    x_instrument <- attr(x, "meta")

    output <- c(x, y)

    class(output) <- x_class
    attr(output, "meta") <- x_instrument

    output

  })

}
