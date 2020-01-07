server = function(input, output, session) {
  
  output$Test <- renderUI({selectInput("Test",
                                       "Choose Test",
                                       choices = GenerateSelectChoices(default = "", text = "", fieldName = "DateId", extraInfo = list("SessionTime"))
  )})
  
  
  
  observeEvent({input$mail},{
    var<-list()
    if (input$mail != -1){
      var = list(list(paste("UserId = '", input$mail, "'", sep = "")))
    }
    output$Test <- renderUI({selectInput("Test",
                                         "Choose Test",
                                         choices = GenerateSelectChoices(default = "All Tests", text = "", fieldName = "DateId", extraInfo = list("SessionTime"), conditions = var)
    )})
    
    observeEvent({input$Test},{
      var2<-list()
      if (input$Test != -1){
        output$GameType <- renderText(paste("Game Type:", GetField("GameType", FetchDatas(conditionLists = list(list(paste("DateId = '", input$Test, "'", sep = ""))), option="DISTINCT GameType")), sep = " "))
        output$HitType <- renderText(paste(length(GetField("HitType", FetchDatas(conditionLists = list(list(paste("DateId = '", input$Test, "'", sep = "")), list("HitType = 'Hit'")), option="HitType"))), " Successful Hits" , sep = " "))
        output$WrongHits <- renderText(paste(length(GetField("HitType", FetchDatas(conditionLists = list(list(paste("DateId = '", input$Test, "'", sep = "")), list("HitType = 'Miss'", "HitType = 'Error'")), option="HitType"))), " Invalid Hits" , sep = " "))
        output$Average <- renderText(paste("Average time : ", format(mean(GetField("DeltaTime", FetchDatas(conditionLists = list(list(paste("DateId = '", input$Test, "'", sep = "")), list("HitType = 'Hit'")), option="DeltaTime"))),digits = 3), " seconds" , sep = " "))
      }
      else
      {
        output$GameType <- renderText("")
        output$HitType <- renderText("")
        output$WrongHits <- renderText("")
        output$Average <- renderText("")
      }
      
      
    })
    
    
    output$plot1 <- renderPlotly(
      plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
      
      
    )
    
    output$plot2 <- renderPlotly(
      plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
      
      
    )
    
    
  })
}