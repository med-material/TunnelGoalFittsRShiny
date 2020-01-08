firstGen <- TRUE

server = function(input, output, session) {
  
  
  
  
  observeEvent({input$mail},{
    var<-list()
    
    if (input$mail != -1){
      var = list(list(paste("UserId = '", input$mail, "'", sep = "")))
    }
    
    
    
    output$Test <- renderUI(
      {selectInput("Test",
                   "Choose Test",
                   choices = GenerateSelectChoices(default = "All Tests", text = "", fieldName = "DateId", extraInfo = list("SessionTime"), extracaract = " seconds", conditions = var)
                   
      )})
    
    
    
    observeEvent({input$Test},{
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
    
    observeEvent({input$Test
      input$mail},{
        
        if(firstGen)
        {
          firstGen <<- FALSE
          return()
        }
        
        var2<-list()
        i <- 1
        if (input$Test != -1){
          var2[[i]] <- list(list(paste("DateId = '", input$Test, "'", sep = "")))
          i = i+1
        }
        
        if (input$mail != -1){
          var2[[i]] <- list(list(paste("UserId = '", input$mail, "'", sep = "")))
          i = i+1
        }
        
        tempVar <- FetchDatas(conditionLists = var2, option = 'TargetsDistance, TargetDiameter, DeltaTime')
        
        DifficultyIndex <- numeric(length(tempVar["DeltaTime"]))
        
        
        DeltaTime <- tempVar["DeltaTime"]
        
        tempVar2 <- data.frame(DifficultyIndex, DeltaTime)
        
        
        for (i in 1:nrow(tempVar)) {
          
          tempVar2[i, "DifficultyIndex"] <- log2((strtoi(tempVar[i, "TargetDiameter"]) / strtoi(tempVar[i, "TargetsDistance"]))+1)
          
        }
        
        
        updateSelectInput(session, 
                          "Index",
                          choices =  tempVar2["DifficultyIndex"] )
        
        
        
        
        output$plot1 <- renderPlotly(
          plot_ly(type = 'scatter',mode='markers',
                  data = tempVar2, x=~DifficultyIndex, y=~DeltaTime) %>% 
            layout(xaxis = list(title = ""), 
                   yaxis = list(title = "Movment Time (s)"))
        )
        
      })
    
    
    
    
    output$plot2 <- renderPlotly(
      plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
      
      
    )
    
    
  })
}