server = function(input, output, session) {
  
  output$plot1 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers')
    
    
  )
}
