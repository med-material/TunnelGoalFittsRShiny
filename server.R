library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggpubr)


server <- function(input, output, session) {

  # VARIABLES
  df_goal <- NULL
  df_tunnel <- NULL
  df_fitts <- NULL
  df_DHR <- NULL
  
  # define colors to use in plots.
  colorPalette <- c("#c94232", "#239a37")
  all_accounts <- RetreiveUniqueColVals("tunnel_fit_test", "Email")

  # a variable we use, if we filter based on pid.
  pid_index <- NULL
  pid_name <- NULL
  pid_email <- NULL
  pid_query <- NULL
  subject <- "Goal" # tunnel, fitts

  # a variable we use to keep track of the currently available participants
  participants <- NULL
  choices <- NULL

  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[["subject"]])) {
      subject <<- query[["subject"]]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    if (!is.null(query[["pid"]])) {
      pid <- query[["pid"]]
      pid_query <<- pid
      pid_name <<- pid
    }
    if (!is.null(query[["email"]])) {
      sel <- query[["email"]]
      pid_email <- query[["email"]]
      updateSelectInput(session, "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session, "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent(
    {
      input$subjectChooser
    },
    {
      if (input$subjectChooser != subject) {
        subject <<- input$subjectChooser
        UpdatePIDSelection()
        UpdateVisualizations()
      }
    }
  )

  observeEvent(
    ignoreNULL = FALSE,
    {
      input$pidChooser
    },
    {
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
        pid_name <<- unlist(participants[input$pidChooser, "PID"])
        pid_email <<- unlist(participants[input$pidChooser, "Email"])
      } else {
        pid_index <<- NULL
        pid_name <<- NULL
        pid_email <<- NULL
      }
      print(paste("pidChooser: pid_index ", pid_index))
      print(paste("pidChooser: pid_name ", pid_name))
      UpdateVisualizations()
    }
  )
  observeEvent(
    {
      input$emailSelect
    },
    {
      if (input$emailSelect == "-1") {
        return()
      }
      RefreshDataSets(input$emailSelect)

      # VARIABLES
      df_all <<- df_all %>% mutate(TargetNumber=as.numeric(TargetNumber),hitScore=ifelse(HitType=="Hit",1,0))
      df_goal <<- df_all %>% filter(GameType == "Goal" & is.na(CDGain)) 
      df_goal$FittsID <<- log2(2 * df_goal$ObjectDistanceCm / df_goal$ObjectHeightCm)
      df_goalAgg <<- df_goal %>%
        filter(is.na(CDGain)) %>%
        group_by(Email, PID, InputType, InputResponders, FittsID) %>%
        summarize(meanMT = mean(DeltaTime, na.rm = TRUE))
      # df_goalAggEnt<<-df_goalAgg %>% group_by(Email,PID, InputType, InputResponders) %>% summarize(total.count=n())
      df_tunnel <<- df_all %>% filter(GameType == "Tunnel" & is.na(CDGain))
      df_tunnel$aspectRatio <<- df_tunnel$ObjectDistanceCm / df_tunnel$ObjectWidthCm
      df_fitts <<- df_all %>% filter(GameType == "Fitts" & is.na(CDGain))
      df_fitts <<- df_fitts[df_fitts$DeltaTime < 5, ]
      df_fitts$FittsID <<- log2(2 * (df_fitts$ObjectDistanceCm) / df_fitts$ObjectWidthCm)
      df_fittsAgg <<- df_fitts %>%
        filter(HitType=='Hit' & is.na(CDGain)) %>% 
        group_by(Email, PID, InputType, InputResponders, FittsID) %>%
        summarize(meanMT = mean(DeltaTime, na.rm = TRUE))
      df_DHR <<- df_all %>% filter(GameType == "Fitts" & !is.na(CDGain)) %>% mutate(FittsID=log2(2 * (ObjectDistanceCm+ObjectWidthCm) / ObjectWidthCm))
      # df_fittsAggEnt<<-df_fittsAgg %>% group_by(Email,PID, InputType, InputResponders) %>% summarize(total.count=n())
      df_DHRagg <<- df_DHR %>% dplyr::group_by(Email,PID, InputType, InputResponders, FittsID, CDGain,ObjectWidthCm) %>% summarize(MTmean=mean(DeltaTime),errorRate=1-mean(hitScore))
      UpdatePIDSelection()

      UpdateVisualizations()
    }
  )
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
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "Fitts") {
      participants <<- unique(df_fitts %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "Tunnel") {
      participants <<- unique(df_tunnel %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "DHR") {
      participants <<- unique(df_DHR %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
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
    print(paste("df_all nrow:", nrow(df_all)))
    print(paste("df_goal nrow:", nrow(df_goal)))
    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      df_all <- df_all %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
      df_goal <- df_goal %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
      df_tunnel <- df_tunnel %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
      df_fitts <- df_fitts %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name) 
      df_DHR<-df_DHR %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name) %>%
        filter(!is.na(CDGain)) 
      df_DHRagg<-df_DHRagg %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name) %>%
        filter(!is.na(CDGain)) 
    }
    if (subject == "Goal") {
      print(paste("df_goal filtered nrow:", nrow(df_goal)))
      output$goalHitType <- renderText(paste(length(df_goal$ID[df_goal$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$goalWrongHits <- renderText(paste(length(df_goal$ID[df_goal$HitType == "Miss"]), " Errors", sep = " "))
      output$goalAverage <- renderText(paste("Average time : ", format(mean(df_goal$DeltaTime)), sep = " "))
      output$goalType <- renderText(paste("Input Type :", unique(df_goal$InputType), sep = " "))
      output$goalRespond <- renderText(paste("Input Responder :", unique(df_goal$InputResponder), sep = " "))

      output$goalTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          # color = var3 ,
          # colors = pal,
          data = df_goal, x = ~TargetNumber, y = ~DeltaTime
        ) %>%

          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )

      output$goalComparison <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          color = df_goal$InputType
          # colors = pal,
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Movement Time (s)"),
            legend = list(orientation = "h")
          )
      )
      output$goalLRPlot <- renderPlot({
        goalLRcompGG <- ggplot(df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(goalLRcompGG)
      })
      
      output$goalLRSinglePlot <- renderPlot({
        goalLRSinglePlot <- ggplot(df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation( aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
        print(goalLRSinglePlot)
      })
      
      output$goalLRPressurePlot <- renderPlot({
        goalLRcompPress <- ggplot(df_goal[df_goal$InputType == "pressuresensor", ], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(~InputResponders)
        print(goalLRcompPress)
      })

      output$GoalDeviceComp <- renderPlot({
        GoalDeviceCompPlot <- ggplot(df_goal[df_goal$HitType=='Hit',], aes(FittsID, DeltaTime, group = InputType, colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(GoalDeviceCompPlot)
      })
      
      output$GoalDeviceCompSPlot <- renderPlot({
        GoalDeviceCompSPlot <- ggplot(df_goal[df_goal$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation( aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) 
        print(GoalDeviceCompSPlot)
      })

      # output$GoalDeviceCompAgg <- renderPlot({GoalDeviceCompAggPlot<- ggplot(df_goalAgg, aes(FittsID,DeltaTime, group=InputType,colour=InputType)) +  geom_smooth(method = "lm", fill = NA)+ ylab("movement time")+xlab("ID")+ theme_bw()+geom_point()+ stat_regline_equation(color="black",aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))+facet_grid(~InputResponders)
      # print(GoalDeviceCompAggPlot)})

      output$GoalDeviceCompAgg <- renderPlot({
        GoalDeviceCompAggPlot <- ggplot(df_goalAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(GoalDeviceCompAggPlot)
      })
    } else if (subject == "Fitts") {
      print(paste("df_fitts filtered nrow:", nrow(df_fitts)))
      output$fittsHitType <- renderText(paste(length(df_fitts$ID[df_fitts$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$fittsWrongHits <- renderText(paste(length(df_fitts$ID[df_fitts$HitType == "Miss"]), " Errors", sep = " "))
      output$fittsAverage <- renderText(paste("Average time : ", format(mean(df_fitts$DeltaTime)), sep = " "))
      output$fittsType <- renderText(paste("Input Type :", unique(df_fitts$InputType), sep = " "))
      output$fittsRespond <- renderText(paste("Input Responder :", unique(df_fitts$InputResponder), sep = " "))

      output$fittsTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          # color = var3 ,
          # colors = pal,
          data = df_fitts[df_fitts$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
        ) %>%

          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )

      output$fittsComparison <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          color = df_fitts$InputType
          # colors = pal,
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Movement Time (s)"),
            legend = list(orientation = "h")
          )
      )
      # Fitts plot with regression line and equation
      fittsRegPlotX <- ggplot(df_fitts[df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime)) +
        xlab("ID") +
        theme_bw() +
        geom_point()
      output$fittsRegPlot <- renderPlotly(ggplotly(p = fittsRegPlotX) %>%
        config(scrollZoom = TRUE))

      my.formula <- y ~ x
      # FittsLRcomp <-
      output$fittsLRPlot <- renderPlot({
        FittsLRcompGG <- ggplot(df_fitts[df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(FittsLRcompGG)
      })
      
      output$fittsLRSinglePlot <- renderPlot({
        fittsLRSinglePlot <- ggplot(df_fitts[df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, group =InputResponders, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) 
        print(fittsLRSinglePlot)
      })
      
      # renderPlotly(ggplotly(p = FittsLRcomp) %>%
      #                                   config(scrollZoom = TRUE))
      output$fittsLRPlotPressure <- renderPlot({
        FittsLRcompPress <- ggplot(df_fitts[df_fitts$InputType == "pressuresensor" & df_fitts$HitType=='Hit', ], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(~InputResponders)
        print(FittsLRcompPress)
      })

      output$fittsDeviceComp <- renderPlot({
        fittsDeviceCompPlot <- ggplot(df_fitts[df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(fittsDeviceCompPlot)
      })

      output$fittsDeviceCompSPlot <- renderPlot({
        fittsDeviceCompSPlot <- ggplot(df_fitts[df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) 
        print(fittsDeviceCompSPlot)
      })
      
      output$fittsDeviceCompAgg <- renderPlot({
        fittsDeviceCompAggPlot <- ggplot(df_fittsAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(fittsDeviceCompAggPlot)
      })

      output$fittsLearning <- renderPlot({
        fittsDeviceCompAggPlot <- ggplot(df_fittsAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(fittsDeviceCompAggPlot)
      })



      #      FittsLRLearn <-ggplot(df_fitts, aes(runTrialNo,DeltaTime/FittsID, group=InputResponders)) +xlab("ID")+theme_bw()+geom_point()
      #      output$fittsLRLearnPlot <- renderPlotly(ggplotly(p = FittsLRLearn) %>%
      #                                           config(scrollZoom = TRUE))
    } 
    else if (subject == "Tunnel") {
      print(paste("df_tunnel filtered nrow:", nrow(df_tunnel)))
      output$tunnelHitType <- renderText(paste(length(df_tunnel$ID[df_tunnel$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$tunnelWrongHits <- renderText(paste(length(df_tunnel$ID[df_tunnel$HitType == "Miss"]), " Errors", sep = " "))
      output$tunnelAverage <- renderText(paste("Average time : ", format(mean(df_tunnel$DeltaTime)), sep = " "))
      output$tunnelType <- renderText(paste("Input Type :", unique(df_tunnel$InputType), sep = " "))
      output$tunnelRespond <- renderText(paste("Input Responder :", unique(df_tunnel$InputResponder), sep = " "))

      output$tunnelTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          data = df_tunnel[df_tunnel$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
        ) %>%
          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )
      output$tunnelLRPlot <- renderPlot({
        TunnelLRcompGG <- ggplot(df_tunnel[df_tunnel$HitType=='Hit',], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          stat_regline_equation() +
          ylab("total movement time") +
          xlab("tunnel aspect ratio (length/height)") +
          theme_bw() +
          geom_point() +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(TunnelLRcompGG)
      })

      output$TunnelLRPlotPressure <- renderPlot({
        TunnelLRcompPress <- ggplot(df_tunnel[df_tunnel$InputType == "pressuresensor" & df_tunnel$HitType=='Hit', ], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          stat_regline_equation() +
          ylab("total movement time") +
          xlab("aspect ratio ") +
          theme_bw() +
          geom_point() +
          facet_grid(~InputResponders)
        print(TunnelLRcompPress)
      })
      
      }else if (subject == "DHR") {
        print(paste("df_DHR filtered nrow:", nrow(df_DHR)))
        output$DHRHitType <- renderText(paste(length(df_DHR$ID[df_DHR$HitType == "Hit"]), " Successful Hits", sep = " "))
        output$DHRWrongHits <- renderText(paste(length(df_DHR$ID[df_DHR$HitType == "Miss"]), " Errors", sep = " "))
        output$DHRAverage <- renderText(paste("Average time : ", format(mean(df_DHR$DeltaTime)), sep = " "))
        output$DHRType <- renderText(paste("Input Type :", unique(df_DHR$InputType), sep = " "))
        output$DHRRespond <- renderText(paste("Input Responder :", unique(df_DHR$InputResponder), sep = " "))
        
        output$tunnelTestDetails <- renderPlotly(
          plot_ly(
            type = "scatter",
            mode = "markers",
            data = df_tunnel[df_tunnel$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
          ) %>%
            layout(
              xaxis = list(title = "TargetNumber"),
              yaxis = list(title = "Movment Time (s)"),
              legend = list(orientation = "h")
            )
        )
        output$DHRPlot <- renderPlot({
          DHRPlot <- ggplot(df_DHRagg,mapping=aes(x=ObjectWidthCm*10/CDGain, y=MTmean),colour="Red")+
            # geom_smooth(method = "lm", fill = NA) +
            # stat_regline_equation() +
            
            geom_point(data=df_DHR[df_DHR$HitType=='Hit',],mapping=aes(x=ObjectWidthCm*10/CDGain, y=DeltaTime),colour="blue",size=3)+
              geom_line()+geom_point(colour="Red",size=5) +
          ylab("total movement time") +
            xlab("target size in mm (in motor space)") +
            theme_bw() + scale_x_reverse()
          print(DHRPlot)
        })
        
        output$DHRerrPlot <- renderPlot({
          DHRerrPlot <- ggplot(df_DHRagg,mapping=aes(x=ObjectWidthCm*10/CDGain, y=errorRate),colour="Red")+
            # geom_smooth(method = "lm", fill = NA) +
            # stat_regline_equation() +
            geom_line()+geom_point(colour="Red",size=5) +
            ylab("error rate (misses)") +
            xlab("target size in mm (in motor space)") +
            theme_bw() + scale_x_reverse()
          print(DHRerrPlot)
        })

        

      # FittsLRcomp <-ggplot(df_fitts, aes(FittsID,DeltaTime)) +xlab("HELLO ID")+theme_bw()+geom_point()
      # output$fittsLRPlot <- renderPlotly(ggplotly(p = FittsLRcomp) %>%
      #                                       config(scrollZoom = TRUE))
      #
    
  }
}
}