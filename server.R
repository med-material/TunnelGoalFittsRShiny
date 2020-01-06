server = function(input, output, session) {
  
  
  
  observeEvent({input$mail},{
    var<-list()
    if (input$mail != -1){
      var = list(list(paste("Email = '", input$mail, "'", sep = "")))
    }
  
  
  output$plot1 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
    
    
  )
  
  output$plot2 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
    
    
  )
  
  
})
}