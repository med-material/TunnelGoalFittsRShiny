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
    
    UpdateDisplayedIndex(TRUE)
  })
  
  
  
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
    
    UpdateDisplayedIndex(FALSE)
  })
  
  
  
  
  output$plot2 <- renderPlotly(
    plot_ly(type = 'scatter',mode='markers') %>% layout(xaxis = list(title = ""), yaxis = list(title = "Movment Time (s)"))
  )
  
  UpdateDisplayedIndex <- function(mailUpdated)
  {
    if(is.null(input$Test) | is.null(input$mail))
    {
      return()
    }
    
    
    var2<-list()
    i <- 1

    # if (input$Test != -1){
    #   if (!mailUpdated)
    #   {
    #     var2[[i]] <- list(list(paste("DateId = '", input$Test, "'", sep = "")))
    #     i = i+1
    #   }
    # }
    # 
    # 
    # if (input$mail != -1){
    #   var2[[i]] <- list(list(paste("UserId = '", input$mail, "'", sep = "")))
    #   i = i+1
    # }
    
    # tempVar <- FetchDatas(conditionLists = var2, option = 'TargetsDistance, TargetDiameter, DeltaTime, UserId, GameType')
    tempVar <- FetchDatas(option = 'TargetsDistance, TargetDiameter, DeltaTime, UserId, GameType, DateId')
    tempVar2 <- tempVar
    
    if (input$mail != -1){
      tempVar2 <- subset(tempVar2, tempVar2$UserId == input$mail)
    }
    
    if (input$Test != -1){
      if (!mailUpdated)
      {
        tempVar2 <- subset(tempVar2, tempVar2$DateId == input$Test)
      }
    }
    
    tempVar2$DifficultyIndex <- numeric(length(tempVar2["DeltaTime"]))
    tempVar$DifficultyIndex <- numeric(length(tempVar["DeltaTime"]))
    
    # DeltaTime <- tempVar["DeltaTime"]
    # 
    # UserId <- tempVar["UserId"]
    # 
    # GameType <- tempVar["GameType"]
    # 
    # tempVar2 <- data.frame(DifficultyIndex, DeltaTime, UserId, GameType)
    
    for (i in 1:nrow(tempVar2)) {
      
      tempVar2[i, "DifficultyIndex"] <- log2((strtoi(tempVar2[i, "TargetDiameter"]) / strtoi(tempVar2[i, "TargetsDistance"]))+1)
      
    }
    
    for (i in 1:nrow(tempVar)) {
      
      tempVar[i, "DifficultyIndex"] <- log2((strtoi(tempVar[i, "TargetDiameter"]) / strtoi(tempVar[i, "TargetsDistance"]))+1)
      
    }
    
    tempVar3 <- do.call(rbind, lapply(split(tempVar,as.factor(tempVar$DifficultyIndex)), function(x) {return(x[which.max(x$DeltaTime),])}))
    tempVar4 <- do.call(rbind, lapply(split(tempVar,as.factor(tempVar$DifficultyIndex)), function(x) {return(x[which.min(x$DeltaTime),])}))
    
    
    output$dropdown_index <- renderUI({
      selectInput("Index",
                  NULL,
                  choices = c("Index of Difficulty" = "", tempVar2[["DifficultyIndex"]]),
                  selectize = TRUE
      )
    })
    
    
    # pal <- c("red", "yellow", "green", "blue", "pink")
    
    var3 <- tempVar2$UserId
    
    if (input$mail != -1 & input$Test == -1)
    {
      var3 <- tempVar2$GameType
    }
    if (input$Test != -1)
    {
      var3 <- NULL
    }

    #    output$plot1 <- renderPlotly(
    # plot_ly(data = tempVar3, x = ~DifficultyIndex, y = ~DeltaTime, type = 'scatter', mode = 'lines',
    #         fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
    #         showlegend = FALSE)%>%
    # 
    #   add_trace(data = tempVar4, x = ~DifficultyIndex, y = ~DeltaTime, type = 'scatter', mode = 'lines',
    #             fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
    #             showlegend = FALSE)  %>%
    # 
    #   add_trace(type = 'scatter',
    #             mode='markers',
    #             color = var3 ,
    #             # colors = pal,
    #             data = tempVar2, x=~DifficultyIndex, y=~DeltaTime, showlegend = TRUE)
    
    
    output$plot1 <- renderPlotly(
      plot_ly(type = 'scatter',
              mode='markers',
              color = var3 ,
              # colors = pal,
              data = tempVar2, x=~DifficultyIndex, y=~DeltaTime) %>%

        add_trace(data = tempVar3, x = ~DifficultyIndex, y = ~DeltaTime, type = 'scatter', mode = 'lines', color = NULL,
 fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE)%>%

        add_trace(data = tempVar4, x = ~DifficultyIndex, y = ~DeltaTime, type = 'scatter', mode = 'lines', color = NULL,
                  fill = 'tonexty', fillcolor='rgba(1,1,1,0.1)', line = list(color = 'transparent'),
                  showlegend = FALSE)
      %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "Movment Time (s)"),
               legend = list(orientation = 'h'))
    )
  }
}