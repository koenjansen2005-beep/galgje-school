library(shiny)


ui <- fluidPage(
  textOutput("woord_display"),
  actionButton("nieuw_spel", "Nieuw spel")
)

server <- function(input, output) {
  
  woord <- reactiveVal(sample(woorden, 1))
  
  observeEvent(input$nieuw_spel, {
    woord(sample(woorden, 1))
  })
  
  output$woord_display <- renderText({
    letters_woord <- strsplit(woord(), "")[[1]]
    display <- rep("_", length(letters_woord))
    paste(display, collapse = " ")
  })
}

shinyApp(ui, server)

ui <- fluidPage(
  textOutput("woord_display"),
  textInput("letter", "Raad een letter:"),
  actionButton("raad", "Raad!"),
  actionButton("nieuw_spel", "Nieuw spel")
)

server <- function(input, output) {
  
  woord        <- reactiveVal(sample(woorden, 1))
  geraden      <- reactiveVal(c())  # goede letters
  
  # Nieuw spel
  observeEvent(input$nieuw_spel, {
    woord(sample(woorden, 1))
    geraden(c())
  })
  
  # Letter raden
  observeEvent(input$raad, {
    letter <- tolower(input$letter)
    if (nchar(letter) == 1) {
      geraden(unique(c(geraden(), letter)))
    }
  })
  
  # Woord weergeven
  output$woord_display <- renderText({
    letters_woord <- strsplit(woord(), "")[[1]]
    display <- ifelse(letters_woord %in% geraden(), letters_woord, "_")
    paste(display, collapse = " ")
  })
}

shinyApp(ui, server)