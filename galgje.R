getwd()


woordenlijst <- readLines("wordlist.txt")
head(woordenlijst)
woorden <- readLines("wordlist.txt")
head(woo
head(woorden)
woorden <- readLines("woorden_galgje.txt")
head(woorden)
install.packages("shiny")
library(shiny)
woorden <- readLines("nederlandse_woorden.txt")
woord <- sample(woorden, 1)
print(woord)




kies <- function(aantalletters = NULL) {
  if (is.null(aantalletters)) {
    woord <- sample(woorden, 1)
  } else {
    gefilterd <- woorden[nchar(woorden) == aantalletters]
    if (length(gefilterd) == 0) {          # ✅ eerst checken
      stop(paste("geen woorden van lengte", aantalletters, "in de lijst"))
    }
    woord <- sample(gefilterd, 1)          # dan pas sample
  }
  print(woord)
}

