# Write hex vector to midi file on hard drive - first arg hex string, second arg file path
write_midi <- function(midi_hex, path){
  write.filename <- file(path, "wb")
  # Write binary midi file
  writeBin(as.raw(as.hexmode(midi_hex)), write.filename)
  close(write.filename)
}
