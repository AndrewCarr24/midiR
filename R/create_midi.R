# Create MIDI Function - can handle lists of sequences, treating each item in list as separate track #
create_midi <- function(notes_arg, tempo = 120){

  # Create midi from each track (each item in list)
  track_info <- purrr::map(notes_arg %>% unname(), function(notes){

    # Get note length and make it eighth notes if null
    note_length <- attr(notes, "note_length")

    # Strip and store instrument (if available)
    instrument <- take_instrument(notes)

    # Trim rests at end of sequence
    notes <- trim_rests(notes)

    # Deal with CCs
    notes <- cc_parse(notes)

    if(is.list(notes)){
      cc_stuff <- notes[1:2]
      notes <- notes[[3]]
    }else{cc_stuff <- NULL}

    # Create on/off note data from notes and note length
    on_off_list <- create_on_off_notes(notes, note_length = note_length)

    # Putting note, length, and rest vecs together to make midi track
    midi_track_maker(length_vec = on_off_list[[2]], note_vec = on_off_list[[1]], rest_vec = on_off_list[[3]],
                     instrument = instrument, cc = cc_stuff)
  })

  # Give header and tempo tracks to midi track
  return(give_header_and_tempo(track_info))

}





######################
## Helper Functions ##
######################

# Create multibyte hex from number
special_hexmode <- function(number){

  quotient <- number %/% 256
  remainder <- number %% 256
  remainder_hex <- as.character(as.hexmode(remainder))


  if(quotient < 256){

    quotient_hex <- as.character(as.hexmode(quotient))
    return(c("00", "00", quotient_hex, remainder_hex))

  }else{

    quotient2 <- quotient %/% 256
    remainder2 <- quotient %% 256
    quotient_hex <- as.character(as.hexmode(remainder2))
    quotient2_hex <- as.character(as.hexmode(quotient2))

    return(c("00", quotient2_hex, quotient_hex, remainder_hex))

  }

}


# Note to hex function - enter string of notes get corresponding hex
note_to_hex <- function(note){
  # Apply function to character string of notes
  lapply(note, function(x){

    # Split string into character vector of notes
    x <- stringr::str_split(x, " ") %>% unlist %>% stringr::str_split(., "-") %>% unlist
    # Return appropriate hex for note followed by 'rest'
    c(as.character(as.hexmode(0:131)), "rest")[which(c(paste0(c("C", "C#", "D", "D#", "E", "F",
                                                                "F#", "G", "G#", "A", "Bb", "B"),
                                                              rep(seq(-2,8), each = 12)), "rest") %in% x)]}) %>% unname()
}

# Converts hex to notes
hex_to_notes <- function(hex_nums){

  notes <-c(paste0(c("C", "C#", "D", "Eb", "E", "F",
                     "F#", "G", "G#", "A", "Bb", "B"),
                   rep(seq(-2,8), each = 12)), "rest")

  return(notes[which(c(as.character(as.hexmode(0:131)), "rest") %in% hex_nums)])
}

# Tempo function - convert beats per minute to tempo in pulses
set_tempo <- function(bpm){
  as.hexmode(round(1000000*(60/bpm))) %>% format(., width = 6) %>%
    gsub("(.{2})", "\\1 ", .) %>% trimws
}

# Note Length to Pulse Function - convert note length to pulses
note_length_to_pulse <- function(note_length){
  pulses <- matrix(NA, length(note_length))
  pulses[note_length == "sn"] <- "18"
  pulses[note_length == "en"] <- "30"
  pulses[note_length == "qn"] <- "60"
  pulses[note_length == "hn"] <- "81 40"
  pulses[note_length =="wn"] <- "83 00"
  pulses[note_length == "c"] <- "00"
  pulses %>% as.vector
}

# Function to convert variable-length hex to base 10 numbers
strtoi_mod <- function(chars){

  purrr::map(chars, ~strsplit(.x, " ") %>% unlist) %>%

    purrr::map(., function(x){

      if(length(x) == 1){
        strtoi(x, base = 16L)}
      else{
        128*(as.numeric(x[1]) %% 80) + strtoi(x[2], base = 16L)
      }
    }) %>% return()
}


# Modified binary to hex converter that does variable-length encodings
as.hexmode_mod <- function(arg){
  purrr::map(arg, function(x){

    num = paste(as.character(as.hexmode(x %/% 128) %>% as.numeric + 80),
                as.hexmode(x %% 128)) %>% strsplit(., " ") %>% unlist

    if(num[1] == 80){
      return(num[-1])}else{
        return(paste(num, collapse = ' '))
      }
  })
}

# Convert numbers from hex to binary, add them, convert back to hex
hex_add <- function(nums){
  strtoi_mod(nums) %>% cumsum() %>% as.hexmode_mod()
}


# Take cumulative sums between zeros
cumsum_between_zeros <- function(lst){
  dplyr::tibble(rest = lst) %>% mutate(marker = ifelse(rest == "00", 1, 0)) %>%
    mutate(marker2 = ifelse(marker == 1, 0, cumsum(marker))) %>% group_by(marker2) %>%
    mutate(rest_fin = hex_add(rest)) %>%
    .[['rest_fin']]}


# Take and store track instrument information
take_instrument <- function(seq){
  if(!is.null(attr(seq, "meta"))){
    return(strsplit(attr(seq, "meta"), " ")[[1]])
  }
}

# Trim rests at end of sequence
trim_rests <- function(seq){
  if(seq[length(seq)] == "rest"){
    seq[1:tail(which(seq != "rest"), n = 1)]
  }else{seq}
}

# Adjust length vec to account for half notes and double/triple notes
length_vec_adj <- function(length_vec, notes){

  length_vec[grepl("h", notes)] <- as.character(as.numeric(length_vec[grepl("h", notes)])/2)

  length_vec <-(strtoi_mod(length_vec)[[1]]*(stringr::str_count(notes, "_") + 1)) %>%
    as.hexmode_mod()

  return(length_vec)

}

# Create a rest vector (all '00' except where there are rests)
create_rest_vec <- function(length_vec, note_vec){
  rest_vec <- rep("00", length(note_vec))

  if(any(note_vec == "rest")){

    # Add rest lengths to rest vector - the note after the rest is delayed by the note length associated with the rest
    rest_vec[which(note_vec == "rest") + 1] <- length_vec[which(note_vec == "rest")]

    # Applying special cumsum function to rest_vec
    rest_vec <- cumsum_between_zeros(rest_vec) %>% unlist

    # Remove the rests and rest lengths from note, note length, and rest vectors
    rest_vec <- rest_vec[-which(note_vec == "rest")]
  }

  return(as.list(rest_vec))
}

# Put note, length, and rest vecs together to make midi track
midi_track_maker <- function(length_vec, note_vec, rest_vec, instrument = NULL, cc = NULL){

  hex_track <- stringr::str_split(paste(c(purrr::pmap(list(length_vec, note_vec, rest_vec), function(x, y, z){

    # Dealing with chords
    if(length(y) > 1){
      x <- c(x, rep(0, length(y)-1))
    }

    if(any(z == "flam1")){

      first_on_off <- c("90", "7F")
      second_on_off <- c("90", "7F")
      z <- z[1]
      x <- "04"

    }else if(any(z == "flam2")){

      first_on_off <- c("80", "00")
      second_on_off <- c("80", "00")
      z <- "2c"
      x <- "00"

    }else{

      first_on_off <- c("90", "7F")
      second_on_off <- c("80", "00")

    }

    # Creating body of midi track - series of Note On and Note Off commands followed by end of track (FF 2F 00)
    c(paste(z, first_on_off[1], y, first_on_off[2]), paste(x, second_on_off[1], y, second_on_off[2]))
  }) %>% unlist, "00 FF 2F 00")), " ") %>% unlist

  # Apply CC information (if there is any)
  if(!is.null(cc)){
    cc_idx <- which(cc_parse2(hex_track) == "Note On")[cc[[2]]]
    cc_dummy <- rep(NA, length(hex_track))
    cc_dummy[cc_idx] <- cc[[1]]

    hex_track <- purrr::map2(hex_track, cc_dummy, function(x, y){

      if(!is.na(y))
        return(c(cc_to_hex(y), x))
      else
        return(x)

    }) %>% unlist
  }

  # Apply instrument information to hex track (if available)
  hex_track <- c(instrument, hex_track)

  # Return full midi information - header, midi track length (in bytes), track contents, end track
  c(c("4D", "54", "72", "6B"), as.character(special_hexmode(length(hex_track))), hex_track)

}

# Parse CC - two functions
cc_parse <- function(seq){

  if(any(grepl("CC", seq))){

    seq <- seq %>% unlist %>% unname
    seq2 <- purrr::map(seq, ~substr(.x, 1,2)) %>% unlist %>% unname

    seq3 <- seq[-which(seq2 == "CC")]
    attr(seq3, "class") <- "seq"
    attr(seq3, "meta") <- "00 C0 76"

    return(list(seq[which(seq2 == "CC")], which(seq2 == "CC") - 0:(length(which(seq2 == "CC"))-1), seq3))

  }else{

    return(seq)
  }
}

cc_parse2 <- function(hex_trk){
  purrr::map2(list(hex_trk), purrr::map(seq(4, length(hex_trk), by = 4), ~(.x-3):.x), function(x,y){
    x[y]
  }) %>%
    purrr::map(., function(z){
      if(z[2] == "90") return(c("Note On", "-", "-", "-"))
      else return(c("Note Off", "-", "-", "-"))
    }) %>% unlist
}

# Turn CC into hex
cc_to_hex <- function(seq){

  level <- strsplit(seq, "-")[[1]][2] %>% as.numeric %>% as.hexmode() %>% as.character()
  parm <- strsplit(seq, "-")[[1]][3] %>% as.numeric %>% as.hexmode() %>% as.character()

  return(c("00", "B0", parm, level))

}

# Give header and tempo tracks to midi track
give_header_and_tempo <- function(track_info){

  tempo_track <- c("4D", "54", "72", "6B", "00", "00", "00", "14",
                   "00", "FF", "58", "04", "04", "02", "18", "08", "00", "FF", "51", "03", "07", "A1", "20",
                   "83", "00", "FF", "2F", "00")

  # Create main midi header
  hex_header <- c(c("4D", "54", "68", "64", "00", "00", "00", "06", "00", "01", "00"), as.character(length(track_info)+1),
                  c("00", "60"))

  return(c(hex_header, tempo_track, track_info %>% unlist))
}

# Modify the note vector to account for double/flam notes
note_vec_modify <- function(vec_arg, notes_arg){

  note_vec_mod <- as.list(rep(1, length(vec_arg) + length(grep("d|f", notes_arg)) + 2*length(grep("l", notes_arg))))
  idx <- special_notes_idx(notes_arg)
  note_vec_mod[idx] <- NA

  note_vec_mod[!is.na(note_vec_mod)] <- vec_arg
  note_vec_mod <- zoo::na.locf(note_vec_mod)

  return(note_vec_mod)
}

# Modify the length vector to account for double/flam notes
length_vec_modify <- function(vec_arg, notes_arg){

  length_vec_fin <- purrr::map2(vec_arg, seq_along(vec_arg), function(x, y){
    if(grepl("d", notes_arg[y])){
      half_length <- as.character(strtoi_mod(x)[[1]]/2 %>% as.hexmode())
      return(list(half_length, half_length))
    }else if(grepl("f", notes_arg[y])){
      full_length <- as.character(strtoi_mod(x)[[1]] %>% as.hexmode())
      return(list(full_length, full_length))
    }else if(grepl("l", notes_arg[y])){
      third_length <- as.character(strtoi_mod(x)[[1]]/3 %>% as.hexmode())
      return(list(third_length, third_length, third_length))
    }else{return(x)}
  }) %>% purrr::flatten()

  return(length_vec_fin)
}


# Get index of special notes (flams, doubles, rolls) in note vector
special_notes_idx <- function(notes){

  special_notes <- grep("d|f", notes)
  special_notes <- c(special_notes, grep("l", notes))
  names(special_notes) <- c(rep(1, length(grep("d|f", notes))), rep(2, length(grep("l", notes))))
  special_notes <- sort(special_notes)
  names_stored <- names(special_notes)

  names(special_notes) <- cumsum(names(special_notes))

  special_notes <- special_notes + as.numeric(names(special_notes))

  other_notes <- special_notes[which(names_stored == "2")] - 1

  special_notes_fin <- c(special_notes %>% unname, other_notes)

  names(special_notes_fin) <- names_stored

  names(special_notes_fin)[is.na(names(special_notes_fin))] <- "2"

  return(sort(special_notes_fin))
}

# Takes notes/note_length and returns note_vec, length_vec, and rest_vec
create_on_off_notes <- function(notes, note_length){

  # Make note_length appropriate length
  if(is.null(note_length)){
    note_length <- rep("en", length(notes))
  }else{note_length <- rep(note_length, length(notes))}

  # Create lists from note vector and note length vector.  Note lengths are converted to pulses
  note_vec <- notes %>% stringr::str_replace_all(., "(?![rest])[a-z]", "") %>% note_to_hex() %>% as.list()
  length_vec <- note_length %>% note_length_to_pulse()

  # Account for half notes
  length_vec[grepl("h", notes)] <- as.character(as.numeric(length_vec[grepl("h", notes)])/2)

  # Account for double and triple-length notes
  length_vec <-(strtoi_mod(length_vec)[[1]]*(stringr::str_count(notes, "_") + 1)) %>%
    as.hexmode_mod()

  # Modify note_vec and length_vec to account for 'special notes' (doubles, rolls, flams, ect)
  note_vec <- note_vec_modify(vec_arg = note_vec, notes_arg = notes)
  length_vec <- length_vec_modify(vec_arg = length_vec, notes_arg = notes)

  # Create a rest vector (all '00' except where there are rests)
  rest_vec <- create_rest_vec(length_vec, note_vec)

  # Remove rests from note and length vectors
  if("rest" %in% notes){
    length_vec <- length_vec[-which(note_vec == "rest")]
    note_vec <- note_vec[-which(note_vec == "rest")]
  }

  # Adjusting rest_vec to account for chords in note_vec
  rest_vec <- purrr::map2(rest_vec, note_vec, function(x, y){
    if(length(y) > 1){
      return(c(x, rep("00", length(y) - 1)))
    }else{
      return(x)
    }
  })

  # Adjusting rest_vec to account for flams
  rest_vec[(grep("f", notes[notes != "rest"])+1:length(grep("f", notes[notes != "rest"]))) - 1] <-
    purrr::map(rest_vec[(grep("f", notes[notes != "rest"])+1:length(grep("f", notes[notes != "rest"]))) - 1], ~c(.x,"flam1"))
  rest_vec[(grep("f", notes[notes != "rest"])+1:length(grep("f", notes[notes != "rest"])))] <-
    purrr::map(rest_vec[(grep("f", notes[notes != "rest"])+1:length(grep("f", notes[notes != "rest"])))], ~c(.x,"flam2"))

  return(list(note_vec, length_vec, rest_vec))
}

