library(shiny)


## Functions for making decision set  ----------------------------------------------------------

## draw headlines
draw_headline <- function(version = 3,
                          experiment = 1,
                          valance = TRUE,
                          party = TRUE) {

  headlines_hard <- selective::headlines("hard")
  headlines_soft <- selective::headlines("soft")

  if (experiment == 2) {
    version <- "1"
    if (!valance) {
      headlines_hard <- headlines_hard[headlines_hard$valance == "neutral", ]
    }
    if (!party) {
      headlines_hard <- headlines_hard[headlines_hard$party == "none", ]
    }
  }

  if (experiment == 1) {
    ## headlines_hard <- headlines_hard[headlines_hard$valance != "neutral", ]
    ## headlines_soft <- headlines_soft[headlines_soft$valance != "neutral", ]
    headlines_hard <- headlines_hard[headlines_hard$party != "none", ]
  }

  out <- switch(version,
                "1" = headlines_hard$headline[sample(1:length(headlines_hard$headline), 4, replace = TRUE)],
                "2" = c(headlines_hard$headline[sample(1:length(headlines_hard$headline), 4, replace = TRUE)],
                        headlines_hard$headline[sample(1:length(headlines_hard$headline), 2, replace = TRUE)]),
                "3" = c(headlines_hard$headline[sample(1:length(headlines_hard$headline), 4, replace = TRUE)],
                        headlines_soft$headline[sample(1:length(headlines_soft$headline), 2, replace = TRUE)]))
  out <- out[sample(1:length(out), length(out))]

  ## Get treatment components
  where <- match(out, c(headlines_hard$headline, headlines_soft$headline))
  where_hard_only <- ifelse(out %in% headlines_soft$headline, NA, where)
  treatments <- data.frame(overskrift = out,
                           .type = ifelse(out %in% headlines_soft$headline, "soft", "hard"),
                           .party = as.character(headlines_hard$party_label)[where_hard_only],
                           .valance = c(as.character(headlines_hard$valance),
                                       as.character(headlines_soft$valance))[where],
                           .direction = paste0(as.character(headlines_hard$direction)[where_hard_only], " with:"),
                           .opinion = as.character(headlines_hard$opinion)[where_hard_only])
  return(treatments)
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
                              valance = sample(c(TRUE, FALSE), 1),
                              source = sample(c(TRUE, FALSE), 1),
                              party = sample(c(TRUE, FALSE), 1),
                              experiment = 1,
                              component_table = FALSE) {
  n <- switch(version,
              "1" = 4,
              "2" = 6,
              "3" = 6)
  heads <- draw_headline(version, experiment, valance, party)

  out <- data.frame(valg = "  ",
                    kilde = draw_source(nrow(heads)),
                    anbefalinger = paste0(draw_endorsement(nrow(heads)), " personer anbefaler"),
                    overskrift = heads$overskrift)
  if (component_table) out <- cbind(out, heads[, 2:ncol(heads)])
  if (!endorsement | experiment == 2) out <- out[, -c(grep("anbefalinger", names(out)))]
  if (!source & experiment == 2) out <- out[, -c(grep("kilde", names(out)))]
  return(out)
}


## Generate vignette
generate_vignette <- function(decisions = sample(3, 1)) {
  out <- paste0("Hvis du var nødt til å lese ",
                c("1 sak, hvilken",
                  "2 saker, hvilke",
                  "3 saker, hvilke"),
                " ville du foretrukket å bruke tiden din på?")
  out <- out[decisions]
  return(out)
}


## Server output function ----------------------------------------------------------

function(input, output) {

  v <- reactiveValues(data = NULL)


  ## Experiment 1 ----------------------------------------------

  ## Draw based on specified options
  observeEvent(input$specific, {
    v$vignette <- generate_vignette(as.numeric(input$decisions))
    v$profiles <- generate_profiles(as.numeric(input$version),
                                    as.logical(input$endorsement),
                                    as.logical(input$neutral),
                                    experiment = 1,
                                    component_table = as.logical(input$show_treatments))
  })

  ## Draw random variation
  observeEvent(input$random, {
    v$vignette <- generate_vignette()
    v$profiles <- generate_profiles(experiment = 1,
                                       component_table = as.logical(input$show_treatments))
  })

  # Output profiles and vignettes
  output$vignette <- renderText({v$vignette})
  output$profiles <- renderTable({v$profiles})


  ## Experiment 2 ----------------------------------------------

  ## Draw based on specified options
  observeEvent(input$specific_2, {
    v$vignette_2 <- generate_vignette(as.numeric(input$decisions))
    v$profiles_2 <- generate_profiles(as.numeric(input$version),
                                      as.logical(input$endorsement),
                                      as.logical(input$valance),
                                      as.logical(input$source),
                                      as.logical(input$party),
                                      experiment = 2,
                                      component_table = as.logical(input$show_treatments_2))
  })

  ## Draw random variation
  observeEvent(input$random_2, {
    v$vignette_2 <- generate_vignette()
    v$profiles_2 <- generate_profiles(experiment = 2,
                                      component_table = as.logical(input$show_treatments_2))
  })

  # Output profiles and vignettes
  output$vignette_2 <- renderText({v$vignette_2})
  output$profiles_2 <- renderTable({v$profiles_2})

}
