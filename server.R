library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggpubr)

server = function(input, output, session) {
  
  # VARIABLES
  df_goal <- NULL
  df_tunnel <- NULL
  df_fitts <- NULL
  
  # define colors to use in plots.
  colorPalette <- c("#c94232","#239a37")
  all_accounts = RetreiveUniqueColVals("tunnel_fit_test","Email")
  
  # a variable we use, if we filter based on pid.
  pid_index <- NULL
  pid_name <- NULL
  pid_email <- NULL
  pid_query <- NULL
  subject <- "Goal" #tunnel, fitts
  
  # a variable we use to keep track of the currently available participants
  participants <- NULL
  choices <- NULL
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[['subject']])) {
      subject <<- query[['subject']]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    if (!is.null(query[['pid']])) {
      pid = query[['pid']]
      pid_query <<- pid
      pid_name <<- pid
    }
    if (!is.null(query[['email']])) {
      sel = query[['email']]
      pid_email = query[['email']]
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent({input$subjectChooser}, {
    if (input$subjectChooser != subject) {
      subject <<- input$subjectChooser
      UpdatePIDSelection()      
      UpdateVisualizations()      
    }
  })
  
  observeEvent(ignoreNULL=FALSE, {input$pidChooser}, {
    print(paste("email: ", input$emailSelect))
    # prevent infinite loop - only update pid_name to null, if the value is not already null.
    if (is.null(pid_name) & is.null(input$pidChooser)) {
      print(paste("pidChooser: pid_index ", pid_index))
      print(paste("pidChooser: pid_name ", pid_name))
      print("ignored..")
      return()
    }
    # CheckboxInputGroup sends an initial NULL value which overrides any query values.
    # Make sure we check whether a specific PID was specified as URL param before.
    if (!is.null(pid_query)) {
      print("pid_query exists, ignoring pidChooser")
    } else if (!is.null(input$pidChooser)) {
      pid_index <<- input$pidChooser
      pid_name <<- unlist(participants[input$pidChooser,"PID"])
      pid_email <<- unlist(participants[input$pidChooser,"Email"])
    } else {
      pid_index <<- NULL
      pid_name <<- NULL
      pid_email <<- NULL
    }
    print(paste("pidChooser: pid_index ", pid_index))
    print(paste("pidChooser: pid_name ", pid_name))
    UpdateVisualizations()  
  })
  observeEvent({input$emailSelect},{
    if (input$emailSelect == "-1") {
      return()
    }
    RefreshDataSets(input$emailSelect)

    # VARIABLES
    df_goal <<- df_all %>% filter(GameType == "Goal")
    df_goal$FittsID<<-log2(2*df_goal$ObjectDistanceCm/df_goal$ObjectHeightCm)
    df_tunnel <<- df_all %>% filter(GameType == "Tunnel")
    df_tunnel$aspectRatio<<-df_tunnel$ObjectDistanceCm/df_tunnel$ObjectWidthCm
    df_fitts <<- df_all %>% filter(GameType == "Fitts") 
    df_fitts$FittsID<<-log2(2*df_fitts$ObjectDistanceCm/df_fitts$ObjectWidthCm)
    UpdatePIDSelection()
    
    UpdateVisualizations()
  })
  observeEvent(input$Param, {  
    UpdateVisualizations()
  })
 # update PID selection -------- 
  UpdatePIDSelection <- function() {
    # Update PID Choosers to show PID numbers based on the data
    if (subject == "Goal") {
      participants <<- unique(df_goal %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "Fitts") {
      participants <<- unique(df_fitts %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "Tunnel") {
      participants <<- unique(df_tunnel %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    }
    if (!is.null(pid_query)) {
      pid_name <<- pid_query
      pid_query <<- NULL
      pid_index <<- unname(choices[names(choices) == pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", pid_name))
      print(paste("PIDQuery: pid_index", pid_index))
    }
    print(choices)
    print(nrow(participants))
    if (is.null(choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data", "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    }
    else if (is.null(pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = pid_index, inline = TRUE)
    }
  }

  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", pid_name))
    print(paste("UpdateVis subject: ", subject))
    print(paste("UpdateVis subject==Fitts: ", subject == "Fitts"))
    print(paste("df_all nrow:",nrow(df_all)))
    print(paste("df_goal nrow:",nrow(df_goal)))
    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      df_all <- df_all %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      df_goal <- df_goal %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      df_tunnel <- df_tunnel %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      df_fitts <- df_fitts %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
    }
    if (subject == "Goal") {
      print(paste("df_goal filtered nrow:",nrow(df_goal)))
      output$goalHitType <- renderText(paste(length(df_goal$ID[df_goal$HitType == "Hit"]), " Successful Hits", sep=" "))
      output$goalWrongHits <- renderText(paste(length(df_goal$ID[df_goal$HitType == "Miss"]), " Errors", sep=" "))
      output$goalAverage <- renderText(paste("Average time : ", format(mean(df_goal$DeltaTime)), sep=" "))
      output$goalType <- renderText(paste("Input Type :", unique(df_goal$InputType), sep = " "))  
      output$goalRespond <- renderText(paste("Input Responder :", unique(df_goal$InputResponder), sep = " "))
      
      output$goalTestDetails <- renderPlotly(
        plot_ly(type = 'scatter',
                mode='markers',
                #color = var3 ,
                # colors = pal,
                data = df_goal, x=~TargetNumber, y=~DeltaTime) %>%

          layout(xaxis = list(title = "TargetNumber"),
                 yaxis = list(title = "Movment Time (s)"),
                 legend = list(orientation = 'h'))
      )
      
      output$goalComparison <- renderPlotly(
        plot_ly(type = 'scatter',
                mode='markers',
                color = df_goal$InputType ,
                # colors = pal,
        ) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = "Movement Time (s)"),
                 legend = list(orientation = 'h'))
      )
      output$goalLRPlot <- renderPlot({goalLRcompGG<- ggplot(df_goal, aes(FittsID,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time in seconds")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(cols = vars(InputResponders),rows = vars(InputType))
      print(goalLRcompGG)})
      output$goalLRPressurePlot <- renderPlot({goalLRcompPress<- ggplot(df_goal[df_goal$InputType=="pressuresensor",], aes(FittsID,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time in seconds")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(~InputResponders)
      print(goalLRcompPress)})
      
      output$GoalDeviceComp <- renderPlot({GoalDeviceCompPlot<- ggplot(df_goal, aes(FittsID,DeltaTime, group=InputType,colour=InputType)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(~InputResponders)
      print(GoalDeviceCompPlot)})
      
    } else if (subject == "Fitts") {
      print(paste("df_fitts filtered nrow:",nrow(df_fitts)))
      output$fittsHitType <- renderText(paste(length(df_fitts$ID[df_fitts$HitType == "Hit"]), " Successful Hits", sep=" "))
      output$fittsWrongHits <- renderText(paste(length(df_fitts$ID[df_fitts$HitType == "Miss"]), " Errors", sep=" "))
      output$fittsAverage <- renderText(paste("Average time : ", format(mean(df_fitts$DeltaTime)), sep=" "))
      output$fittsType <- renderText(paste("Input Type :", unique(df_fitts$InputType), sep = " "))  
      output$fittsRespond <- renderText(paste("Input Responder :", unique(df_fitts$InputResponder), sep = " "))
      
      output$fittsTestDetails <- renderPlotly(
        plot_ly(type = 'scatter',
                mode='markers',
                #color = var3 ,
                # colors = pal,
                data = df_fitts, x=~TargetNumber, y=~DeltaTime) %>%
          
          layout(xaxis = list(title = "TargetNumber"),
                 yaxis = list(title = "Movment Time (s)"),
                 legend = list(orientation = 'h'))
      )
      
      output$fittsComparison <- renderPlotly(
        plot_ly(type = 'scatter',
                mode='markers',
                color = df_fitts$InputType ,
                # colors = pal,
        ) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(title = "Movement Time (s)"),
                 legend = list(orientation = 'h'))
      )
      #Fitts plot with regression line and equation
      fittsRegPlotX<-ggplot(df_fitts, aes(FittsID,DeltaTime)) +xlab("ID")+theme_bw()+geom_point()
        output$fittsRegPlot <- renderPlotly(ggplotly(p = fittsRegPlotX) %>%
                                               config(scrollZoom = TRUE))
        
        my.formula <- y ~ x
        #FittsLRcomp <-
        output$fittsLRPlot <- renderPlot({FittsLRcompGG<- ggplot(df_fitts, aes(FittsID,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time + confirmation in seconds")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(cols = vars(InputResponders),rows = vars(InputType))
          print(FittsLRcompGG)})
          #renderPlotly(ggplotly(p = FittsLRcomp) %>%
          #                                   config(scrollZoom = TRUE))
        output$fittsLRPlotPressure <- renderPlot({FittsLRcompPress<- ggplot(df_fitts[df_fitts$InputType=="pressuresensor",], aes(FittsID,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time + confirmation in seconds")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(~InputResponders)
        print(FittsLRcompPress)})
        
        output$fittsDeviceComp <- renderPlot({fittsDeviceCompPlot<- ggplot(df_fitts, aes(FittsID,DeltaTime, group=InputType,colour=InputType)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time + confirmation in seconds")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black")+facet_grid(~InputResponders)
        print(fittsDeviceCompPlot)}
        
        )
        
        
        
        
  #      FittsLRLearn <-ggplot(df_fitts, aes(runTrialNo,DeltaTime/FittsID, group=InputResponders)) +xlab("ID")+theme_bw()+geom_point()
  #      output$fittsLRLearnPlot <- renderPlotly(ggplotly(p = FittsLRLearn) %>%
  #                                           config(scrollZoom = TRUE))
        
        
        
        
    } else if (subject == "Tunnel") {
      print(paste("df_tunnel filtered nrow:",nrow(df_tunnel)))
      output$tunnelHitType <- renderText(paste(length(df_tunnel$ID[df_tunnel$HitType == "Hit"]), " Successful Hits", sep=" "))
      output$tunnelWrongHits <- renderText(paste(length(df_tunnel$ID[df_tunnel$HitType == "Miss"]), " Errors", sep=" "))
      output$tunnelAverage <- renderText(paste("Average time : ", format(mean(df_tunnel$DeltaTime)), sep=" "))
      output$tunnelType <- renderText(paste("Input Type :", unique(df_tunnel$InputType), sep = " "))  
      output$tunnelRespond <- renderText(paste("Input Responder :", unique(df_tunnel$InputResponder), sep = " "))  
      
      output$tunnelTestDetails <- renderPlotly(
                                plot_ly(type = 'scatter',
                                        mode='markers',
                                        data = df_tunnel, x=~TargetNumber, y=~DeltaTime) %>%
                                        layout(xaxis = list(title = "TargetNumber"),
                                         yaxis = list(title = "Movment Time (s)"),
                                         legend = list(orientation = 'h'))
                                )
      output$tunnelLRPlot <- renderPlot({TunnelLRcompGG<- ggplot(df_tunnel, aes(aspectRatio,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ stat_regline_equation()+ ylab("total movement time")+xlab("aspect ratio ")+ theme_bw()+geom_point()+facet_grid(cols = vars(InputResponders),rows = vars(InputType))
      print(TunnelLRcompGG)})
      
      output$TunnelLRPlotPressure <- renderPlot({TunnelLRcompPress<- ggplot(df_tunnel[df_tunnel$InputType=="pressuresensor",], aes(aspectRatio,DeltaTime, colour=InputResponders)) +  geom_smooth(method = "lm", fill = NA)+ stat_regline_equation()+ ylab("total movement time")+xlab("aspect ratio ")+ theme_bw()+geom_point()+facet_grid(~InputResponders)
      print(TunnelLRcompPress)})
      
      
      
      # FittsLRcomp <-ggplot(df_fitts, aes(FittsID,DeltaTime)) +xlab("HELLO ID")+theme_bw()+geom_point()
      # output$fittsLRPlot <- renderPlotly(ggplotly(p = FittsLRcomp) %>%
      #                                       config(scrollZoom = TRUE))
      # 
    }
  }
  
}
  
