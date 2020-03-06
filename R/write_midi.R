#' Write MIDI file.
#'
#' @description Takes a hexadecimal MIDI sequence and writes it to the harddrive.
#' @param midi_hex This is a hexadecimal string, output from the create_midi function.
#' @param path The name of the file.
#' @examples
#'# Saves a MIDI sequence as a .mid file called example_midi.mid
#'
#' drum_machine(hh = 1:16, kick = seq(1, 16, by = 4), snare = c(5, 13)) %>%
#' create_midi() %>% write_midi(., "example_midi.mid")
#'
#'
# Write hex vector to midi file on hard drive - first arg hex string, second arg file path
write_midi <- function(midi_hex, path){
  write.filename <- file(path, "wb")
  # Write binary midi file
  writeBin(as.raw(as.hexmode(midi_hex)), write.filename)
  close(write.filename)
}
