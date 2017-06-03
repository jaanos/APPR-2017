library(shiny)

shinyServer(function(input, output) {
  output$zaposlenost <- DT::renderDataTable({
    dcast(zaposlenost, drzava ~ leto, value.var = "zaposlenost") %>%
      rename(`Država` = drzava)
  })
  
  output$izobrazba <- renderUI(
    selectInput("izobrazba", label="Izberi državo",
                choices=c(izobrazba$drzava))
  )
  output$drzave <- renderPlot({
    main <- "BDP per capita"
    if (!is.null(input$drzava) && input$leto %in% levels(drzave$drzava)) {
      t <- drzave %>% filter(drzava == input$drzava)
      main <- paste(main, "v državi", input$drzava)
    } else {
      t <- drzave
    }
    ggplot(t, aes(x = leto)) + geom_histogram() +
      ggtitle(main) + xlab("leto") + ylab("BDP")
  })
})
