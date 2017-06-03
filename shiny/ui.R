library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Zaposlenost in izobrazba mladih"),
  
  tabsetPanel(
      tabPanel("Zaposlenost",
               DT::dataTableOutput("zaposlenost")),
      
      tabPanel("Izobrazba",
               sidebarPanel(
                  uiOutput("izobrazba")
                ),
               mainPanel(plotOutput("drzave")))
    )
))
