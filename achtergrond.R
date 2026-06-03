woorden <- readLines("nederlandse_woorden.txt")

kies <- function(aantalletters = NULL) {
  if (is.null(aantalletters)) {
    woord <- sample(woorden, 1)
  } else {
    gefilterd <- woorden[nchar(woorden) == aantalletters]
    if (length(gefilterd) == 0) {
      stop(paste("geen woorden van lengte", aantalletters, "in de lijst"))
    }
    woord <- sample(gefilterd, 1)
  }
  return(woord)
}

check_letter <- function(letter, woord) {
  letter <- tolower(letter)
  woord  <- tolower(woord)
  letters_woord <- strsplit(woord, "")[[1]]
  which(letters_woord == letter)
}

maak_patroon <- function(woord) {
  rep(NA_character_, nchar(woord))
}

update_patroon <- function(patroon, posities, letter) {
  patroon[posities] <- letter
  patroon
}

toon_patroon <- function(patroon) {
  weergave <- ifelse(is.na(patroon), "_", patroon)
  paste(weergave, collapse = " ")
}

is_gewonnen <- function(patroon) {
  !any(is.na(patroon))
}


