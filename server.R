server = function(input, output, session) {
  
  output$plot1 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = "Mole X index"), yaxis = list(title = "Movment Time (s)"))
    
    
  )
  
  output$plot2 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = "Mole X index"), yaxis = list(title = "Movment Time (s)"))
    
    
  )
  
  
}
