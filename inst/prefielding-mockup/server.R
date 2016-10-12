library(shiny)


## Functions for making decision set  ----------------------------------------------------------

## draw headlines
draw_headline <- function(version = 3, neutral = FALSE) {

  headlines_hard <- headlines("hard")$headline
  headlines_soft <- headlines("soft")$headline

  if (neutral) {
    headlines_hard <- headlines_hard[headlines("hard")$valance == "neutral"]
    headlines_soft <- headlines_soft[headlines("soft")$valance == "neutral"]
  }

  out <- switch(version,
                "1" = headlines_hard[sample(1:length(headlines_hard), 4, replace = TRUE)],
                "2" = c(headlines_hard[sample(1:length(headlines_hard), 4, replace = TRUE)],
                        headlines_hard[sample(1:length(headlines_hard), 2, replace = TRUE)]),
                "3" = c(headlines_hard[sample(1:length(headlines_hard), 4, replace = TRUE)],
                        headlines_soft[sample(1:length(headlines_soft), 2, replace = TRUE)]))
  out <- out[sample(1:length(out), length(out))]
  return(out)
}

## Draw endorsements
draw_endorsement <- function(n = 4) {
  anbefalinger <- c(0L, 0L, 0L, 95L, 98L, 106L, 1133L, 1121L, 1098L)
  out <- anbefalinger[sample(1:length(anbefalinger), n, FALSE)]
  return(as.integer(out))
}

## Draw source
draw_source <- function(n = 4) {
  kilder <- c("NRK",
              "TV2",
              "Klassekampen",
              "Dagens Næringsliv",
              "VG",
              "Aftenposten",
              "Dagbladet")
  out <- kilder[sample(1:length(kilder), n, TRUE)]
  return(out)
}

## Generate Profile
generate_profiles <- function(version = sample(3, 1),
                              endorsement = sample(c(TRUE, FALSE), 1),
                              neutral = sample(c(TRUE, FALSE), 1)) {
  n <- switch(version,
              "1" = 4,
              "2" = 6,
              "3" = 6)

  out <- data.frame(valg = "  ",
                    kilde = draw_source(n),
                    anbefalinger = draw_endorsement(n),
                    overskrift = draw_headline(version, neutral))
  if (!endorsement) out <- out[, -c(grep("anbefalinger", names(out)))]
  return(out)
}

## Generate vignette
generate_vignette <- function(decisions = sample(3, 1)) {
  out <- paste0("Vi ønsker å studere folks medievaner. Under er noen tenkte saker vi har laget som minner om de man kan finne i norske nettaviser. Vennligst les alle overskriftene nøye. Hvis du var nødt til å lese ",
                c("1 sak, hvilken ville du foretrukket å bruke tiden din på?",
                  "2 saker, hvilke ville du foretrukket å bruke tiden din på?",
                  "3 saker, hvilke ville du foretrukket å bruke tiden din på?"))
  out <- out[decisions]
  return(out)
}


## Server output function ----------------------------------------------------------

function(input, output) {

  v <- reactiveValues(data = NULL)

  ## Draw based on specified options
  observeEvent(input$specific, {
    v$vignette <- generate_vignette(as.numeric(input$decisions))
    v$profiles <- generate_profiles(as.numeric(input$version),
                                    as.logical(input$endorsement),
                                    as.logical(input$neutral))
  })

  ## Draw random variation
  observeEvent(input$random, {
    v$vignette <- generate_vignette()
    v$profiles <- generate_profiles()
  })

  # Output profiles and vignettes
  output$vignette <- renderText({v$vignette})
  output$profiles <- renderTable({v$profiles})
}
