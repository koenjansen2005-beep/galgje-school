setwd("~/P12/bioinformatica/galgje/galgje_2")
library(shiny)
source("achtergrond.R")

# --- WAT JE ZIET ---
ui <- fluidPage(
  titlePanel("Galgje"),
  
  numericInput("woordlengte", "Woordlengte:", value = 5, min = 3, max = 10),
  actionButton("nieuw_spel", "Nieuw spel"),
  
  br(), br(),
  
  h3(textOutput("patroon_tekst")),
  textOutput("fouten_tekst"),
  textOutput("debug_woord"),
  
  br(),
  
  textInput("letter_input", "Raad een letter:"),
  actionButton("raad", "Raad!")
)

# --- WAT ER GEBEURT ---
server <- function(input, output, session) {
  
  # het spel onthoudt deze 4 dingen
  woord        <- reactiveVal("")
  patroon      <- reactiveVal(c())
  fout_letters <- reactiveVal(c())
  fouten       <- reactiveVal(0)
  
  # knop geklikt = nieuw spel
  observeEvent(input$nieuw_spel, {
    w <- kies(input$woordlengte)
    woord(w)
    patroon(maak_patroon(w))
    fout_letters(c())
    fouten(0)
  })
  
  # raad knop geklikt = letter controleren
  observeEvent(input$raad, {
    letter <- tolower(input$letter_input)
    
    # alleen doorgaan als er 1 letter ingetypt is
    if (nchar(letter) != 1) return()
    
    # alleen doorgaan als het spel bezig is
    if (woord() == "") return()
    
    posities <- check_letter(letter, woord())
    
    if (length(posities) > 0) {
      # goede gok: vul de letter in het patroon
      patroon(update_patroon(patroon(), posities, letter))
    } else {
      # foute gok: foutenteller omhoog
      fouten(fouten() + 1)
      fout_letters(c(fout_letters(), letter))
    }
  })
  
  # toon het patroon: _ a _ _ _
  output$patroon_tekst <- renderText({
    if (woord() == "") "Druk op Nieuw spel!"
    else toon_patroon(patroon())
  })
  
  # toon de fouten
  output$fouten_tekst <- renderText({
    if (woord() == "") return("")
    paste("Fouten:", fouten(), "/ 6  |  Fout:", 
          if (length(fout_letters()) == 0) "-"
          else paste(fout_letters(), collapse = ", "))
  })
  
  # tijdelijk: laat het woord zien om te testen
  output$debug_woord <- renderText({
    if (woord() == "") return("")
    paste("Woord (test):", woord())
  })
}

shinyApp(ui, server)
