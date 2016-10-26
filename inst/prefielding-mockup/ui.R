library(shiny)

## Server input function ----------------------------------------------------------

navbarPage(
  title = "Experiment on selective exposure (test setup)",
  id = "prefielding_mockup",

  ## Experiment 1 -----------------------------------------------------------------
  tabPanel(
    title = "Experiment 1",
    h2("Experiment 1: Breaking dissonance", align = "center"),
    br(),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          checkboxInput("show_treatments", label = "Show integrated treatments", value = FALSE),
          br(),
          radioButtons("version", label = "Soft news",
                       choices = list("Hard news (4)" = 1,
                                      "Hard news (4) + Hard news (2)" = 2,
                                      "Hard news (4) + Soft news (2)" = 3),
                       selected = 3),
          radioButtons("endorsement", label = "Social endorsements",
                       choices = list("Vis anbefalinger" = TRUE,
                                      "Ikke vis anbefalinger" = FALSE),
                       selected = TRUE),
          radioButtons("decisions", label = "Number of choices",
                       choices = list("Må velge 1 sak" = 1,
                                      "Må velge 2 saker" = 2,
                                      "Må velge 3 saker" = 3),
                       selected = 3),
          actionButton("specific", label = "Make with this configuration"),
          hr(),
          actionButton("random", label = "Make with a random configuration")
        )
      ),
      column(
        width = 6,
        br(), br(), br(), br(),
        h4("Vi ønsker å studere folks medievaner. Under er noen tenkte saker vi har laget som minner om de man kan finne i norske nettaviser. Vennligst les alle overskriftene nøye og se for deg at sakene er ekte"),
        br(),
        h4(textOutput("vignette")),
        br(),
        h5(tableOutput("profiles"))
      )
  )),

  ## Experiment 2 -----------------------------------------------------------------
  tabPanel(
    title = "Experiment 2",
    h2("Experiment 2: Making dissonance", align = "center"),
    br(),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          checkboxInput("show_treatments_2", label = "Show integrated treatments", value = FALSE),
          br(),
          radioButtons("party", label = "Party",
                       choices = list("With party" = TRUE,
                                      "Without party" = FALSE),
                       TRUE),
          radioButtons("valance", label = "Valance",
                       choices = list("With valance" = TRUE,
                                      "Without valance (neutral)" = FALSE),
                       selected = FALSE),
          radioButtons("source", label = "Source",
                       choices = list("With source" = TRUE,
                                      "Without source" = FALSE),
                       selected = FALSE),
          radioButtons("decisions", label = "Number of choices",
                       choices = list("Må velge 1 sak" = 1,
                                      "Må velge 2 saker" = 2,
                                      "Må velge 3 saker" = 3),
                       selected = 3),
          actionButton("specific_2", label = "Make with this configuration"),
          hr(),
          actionButton("random_2", label = "Make with a random configuration")
        )
      ),
      column(
        width = 8,
        br(), br(), br(), br(),
        h4("Vi ønsker å studere folks medievaner. Under er noen tenkte saker vi har laget som minner om de man kan finne i norske nettaviser. Vennligst les alle overskriftene nøye og se for deg at sakene er ekte."),
        br(),
        h4(textOutput("vignette_2")),
        br(),
        h5(tableOutput("profiles_2"))
      )
    )
  )
)
