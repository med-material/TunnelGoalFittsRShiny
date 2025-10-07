library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggpubr)


server <- function(input, output, session) {

  #local_data = F
  #df_all <- data.frame()
  # VARIABLES
  r <- reactiveValues(df_all = NULL,
                      local_data = T,
                      df_goal = NULL,
                      df_tunnel = NULL,
                      df_fitts = NULL,
                      df_DHR = NULL,
                      pid_index = NULL,
                      pid_name = NULL,
                      pid_email = NULL,
                      pid_query = NULL,
                      subject = "Goal", # tunnel, fitts
                      participants = NULL,
                      choices = NULL
                      )


  
  # define colors to use in plots.
  colorPalette <- c("#c94232", "#239a37")
  all_accounts <- RetreiveUniqueColVals("tunnel_fit_test", "Email")
  csv_data <- callModule(csv_upload, "uploadData")
  
  # a variable we use, if we filter based on pid.
  
  


  # a variable we use to keep track of the currently available participants


  observeEvent(input$CsvButton, {
    insertUI(selector = "#CsvButton", where = "afterEnd",
             ui = showModal(modalDialog(csv_upload_UI("uploadData"), easyClose = TRUE)))
  })
  
  observeEvent(csv_data$trigger, {
    req(csv_data$trigger > 0)
    if (!is.null(csv_data$df)) {
      r$df_all = RefreshDataLocal(csv_data$df)
      r$local_data = T
      user_id = r$df_all$Email
      new_pid = r$df_all$PID
      updateSelectInput(session, "emailSelect", choices = user_id, selected = user_id[1])
      updateTabsetPanel(session, "subjectChooser", selected = new_pid[1])
    }
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[["subject"]])) {
      r$subject <- query[["subject"]]
      updateTabsetPanel(session, "subjectChooser", selected = r$subject)
    }
    if (!is.null(query[["pid"]])) {
      pid <- query[["pid"]]
      r$pid_query <- pid
      r$pid_name <- pid
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
      if (input$subjectChooser != r$subject) {
        r$subject <- input$subjectChooser
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
      if (is.null(r$pid_name) & is.null(input$pidChooser)) {
        print(paste("pidChooser: pid_index ", r$pid_index))
        print(paste("pidChooser: pid_name ", r$pid_name))
        print("ignored..")
        return()
      }
      # CheckboxInputGroup sends an initial NULL value which overrides any query values.
      # Make sure we check whether a specific PID was specified as URL param before.
      if (!is.null(r$pid_query)) {
        print("pid_query exists, ignoring pidChooser")
      } else if (!is.null(input$pidChooser)) {
        r$pid_index <- input$pidChooser
        r$pid_name <- unlist(r$participants[input$pidChooser, "PID"])
        r$pid_email <- unlist(r$participants[input$pidChooser, "Email"])
      } else {
        r$pid_index <- NULL
        r$pid_name <- NULL
        r$pid_email <- NULL
      }
      print(paste("pidChooser: pid_index ", r$pid_index))
      print(paste("pidChooser: pid_name ", r$pid_name))
      UpdateVisualizations()
    }
  )
  observeEvent(
    {
      input$emailSelect
    },
    {
      if (input$emailSelect %in% c("-1","NA")) {
        return()
      }
      RefreshDataSets(input$emailSelect, r$local_data)
      # VARIABLES
      r$df_all <- r$df_all %>% mutate(TargetNumber=as.numeric(TargetNumber),hitScore=ifelse(HitType=="Hit",1,0))
      r$df_goal <- r$df_all %>% filter(GameType == "Goal") 
      r$df_goal$FittsID <- log2(2 * r$df_goal$ObjectDistanceCm / r$df_goal$ObjectHeightCm)
      r$df_goalAgg <- r$df_goal %>%
        filter(is.na(CDGain)) %>%
        group_by(Email, PID, InputType, InputResponders, FittsID) %>%
        summarize(meanMT = mean(DeltaTime, na.rm = TRUE))
      # df_goalAggEnt<<-df_goalAgg %>% group_by(Email,PID, InputType, InputResponders) %>% summarize(total.count=n())
      r$df_tunnel <- r$df_all %>% filter(GameType == "Tunnel")
      r$df_tunnel$aspectRatio <- r$df_tunnel$ObjectDistanceCm / r$df_tunnel$ObjectWidthCm
      r$df_fitts <- r$df_all %>% filter(GameType == "Fitts")
      r$df_fitts <- r$df_fitts[r$df_fitts$DeltaTime < 5, ]
      r$df_fitts$FittsID <- log2(2 * (r$df_fitts$ObjectDistanceCm) / r$df_fitts$ObjectWidthCm)
      r$df_fittsAgg <- r$df_fitts %>%
        filter(HitType=='Hit' & is.na(CDGain)) %>% 
        group_by(Email, PID, InputType, InputResponders, FittsID) %>%
        summarize(meanMT = mean(DeltaTime, na.rm = TRUE))
      r$df_DHR <- r$df_all %>% filter(GameType == "Fitts" & !is.na(CDGain)) %>% mutate(FittsID=log2(2 * (ObjectDistanceCm+ObjectWidthCm) / ObjectWidthCm))
      # df_fittsAggEnt<<-df_fittsAgg %>% group_by(Email,PID, InputType, InputResponders) %>% summarize(total.count=n())
      r$df_DHRagg <- r$df_DHR %>% dplyr::group_by(Email,PID, InputType, InputResponders, FittsID, CDGain,ObjectWidthCm) %>% summarize(MTmean=mean(DeltaTime),errorRate=1-mean(hitScore))
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
    if (r$subject == "Goal") {
      r$participants <- unique(r$df_goal %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    } else if (r$subject == "Fitts") {
      r$participants <- unique(r$df_fitts %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    } else if (r$subject == "Tunnel") {
      r$participants <- unique(r$df_tunnel %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    } else if (r$subject == "DHR") {
      r$participants <- unique(r$df_DHR %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    }
    
    if (!is.null(r$pid_query)) {
      r$pid_name <- r$pid_query
      r$pid_query <- NULL
      r$pid_index <- unname(r$choices[names(r$choices) == r$pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", r$pid_name))
      print(paste("PIDQuery: pid_index", r$pid_index))
    }
    print(r$choices)
    print(nrow(r$participants))
    if (is.null(r$choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data", "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    }
    else if (is.null(r$pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = r$choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", r$pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = r$choices, selected = r$pid_index, inline = TRUE)
    }
  }

  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", r$pid_name))
    print(paste("UpdateVis subject: ", r$subject))
    print(paste("UpdateVis subject==Fitts: ", r$subject == "Fitts"))
    print(paste("df_all nrow:", nrow(r$df_all)))
    print(paste("df_goal nrow:", nrow(r$df_goal)))
    # Filter visualization data based on pid_name
    if (!is.null(r$pid_name)) {
      r$df_all <- r$df_all %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name)
      r$df_goal <- r$df_goal %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name)
      r$df_tunnel <- r$df_tunnel %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name)
      r$df_fitts <- r$df_fitts %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name) 
      r$df_DHR<-r$df_DHR %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name) %>%
        filter(!is.na(CDGain)) 
      df_DHRagg<-df_DHRagg %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% r$pid_name) %>%
        filter(!is.na(CDGain)) 
    }
    if (r$subject == "Goal") {
      print(paste("df_goal filtered nrow:", nrow(r$df_goal)))
      output$goalHitType <- renderText(paste(length(r$df_goal$ID[r$df_goal$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$goalWrongHits <- renderText(paste(length(r$df_goal$ID[r$df_goal$HitType == "Miss"]), " Errors", sep = " "))
      output$goalAverage <- renderText(paste("Average time : ", format(mean(r$df_goal$DeltaTime)), sep = " "))
      output$goalType <- renderText(paste("Input Type :", unique(r$df_goal$InputType), sep = " "))
      output$goalRespond <- renderText(paste("Input Responder :", unique(r$df_goal$InputResponder), sep = " "))

      output$goalTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          # color = var3 ,
          # colors = pal,
          data = r$df_goal, x = ~TargetNumber, y = ~DeltaTime
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
          color = r$df_goal$InputType
          # colors = pal,
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Movement Time (s)"),
            legend = list(orientation = "h")
          )
      )
      output$goalLRPlot <- renderPlot({
        req(!is.na(r$df_goal))
        req(!is.null(r$df_goal))
        graphreq = nrow(r$df_goal %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders. Only",nrow(r$df_goal %>% distinct(InputResponders)),"Available.")), errorClass = "vis")
        goalLRcompGG <- ggplot(r$df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
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
        req(!is.na(r$df_goal))
        req(!is.null(r$df_goal))
        # graphreq: checks that the minimum data is needed to produce the 
        graphreq = nrow(r$df_goal %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")
        goalLRSinglePlot <- ggplot(r$df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation( aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
        print(goalLRSinglePlot)
      })
      
      output$goalLRPressurePlot <- renderPlot({
        req(!is.na(r$df_goal))
        req(!is.null(r$df_goal))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_goal %>% filter(InputType=="pressuresensor") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders, both using the pressure sensor input type. Only",graphreq,"Available.")), errorClass = "vis")
        goalLRcompPress <- ggplot(r$df_goal[r$df_goal$InputType == "pressuresensor", ], aes(FittsID, DeltaTime, colour = InputResponders)) +
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
        req(!is.na(r$df_goal))
        req(!is.null(r$df_goal))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_goal %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input types (devices). Only",graphreq,"Available.")), errorClass = "vis")        
        GoalDeviceCompPlot <- ggplot(r$df_goal[r$df_goal$HitType=='Hit',], aes(FittsID, DeltaTime, group = InputType, colour = InputType)) +
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
        req(!is.na(r$df_goal))
        req(!is.null(r$df_goal))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_goal %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input types (devices). Only",graphreq,"Available.")), errorClass = "vis")        
        GoalDeviceCompSPlot <- ggplot(r$df_goal[r$df_goal$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
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
        GoalDeviceCompAggPlot <- ggplot(r$df_goalAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
        print(GoalDeviceCompAggPlot)
      })
    } else if (r$subject == "Fitts") {
      print(paste("df_fitts filtered nrow:", nrow(r$df_fitts)))
      output$fittsHitType <- renderText(paste(length(r$df_fitts$ID[r$df_fitts$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$fittsWrongHits <- renderText(paste(length(r$df_fitts$ID[r$df_fitts$HitType == "Miss"]), " Errors", sep = " "))
      output$fittsAverage <- renderText(paste("Average time : ", format(mean(r$df_fitts$DeltaTime)), sep = " "))
      output$fittsType <- renderText(paste("Input Type :", unique(r$df_fitts$InputType), sep = " "))
      output$fittsRespond <- renderText(paste("Input Responder :", unique(r$df_fitts$InputResponder), sep = " "))

      output$fittsTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          # color = var3 ,
          # colors = pal,
          data = r$df_fitts[r$df_fitts$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
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
          color = r$df_fitts$InputType
          # colors = pal,
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Movement Time (s)"),
            legend = list(orientation = "h")
          )
      )
      # Fitts plot with regression line and equation
      fittsRegPlotX <- ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime)) +
        xlab("ID") +
        theme_bw() +
        geom_point()
      output$fittsRegPlot <- renderPlotly(ggplotly(p = fittsRegPlotX) %>%
        config(scrollZoom = TRUE))

      my.formula <- y ~ x
      # FittsLRcomp <-
      output$fittsLRPlot <- renderPlot({
        req(!is.na(r$df_fitts))
        req(!is.null(r$df_fitts))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")        
        FittsLRcompGG <- ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputResponders)) +
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
        req(!is.na(r$df_fitts))
        req(!is.null(r$df_fitts))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")        
        fittsLRSinglePlot <- ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, group =InputResponders, colour = InputResponders)) +
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
        req(!is.na(r$df_fitts))
        req(!is.null(r$df_fitts))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_fitts  %>% filter(InputType=="pressuresensor", HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from the pressure sensor with at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")        
        FittsLRcompPress <- ggplot(r$df_fitts[r$df_fitts$InputType == "pressuresensor" & r$df_fitts$HitType=='Hit', ], aes(FittsID, DeltaTime, colour = InputResponders)) +
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
        req(!is.na(r$df_fitts))
        req(!is.null(r$df_fitts))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input types (devices). Only",graphreq,"Available.")), errorClass = "vis")        
        fittsDeviceCompPlot <- ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputType)) +
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
        req(!is.na(r$df_fitts))
        req(!is.null(r$df_fitts))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input types (devices). Only",graphreq,"Available.")), errorClass = "vis")        
        fittsDeviceCompSPlot <- ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) 
        print(fittsDeviceCompSPlot)
      })
      
      output$fittsDeviceCompAgg <- renderPlot({
        fittsDeviceCompAggPlot <- ggplot(r$df_fittsAgg, aes(FittsID, meanMT, colour = InputResponders)) +
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
        fittsDeviceCompAggPlot <- ggplot(r$df_fittsAgg, aes(FittsID, meanMT, colour = InputResponders)) +
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
    else if (r$subject == "Tunnel") {
      print(paste("df_tunnel filtered nrow:", nrow(r$df_tunnel)))
      output$tunnelHitType <- renderText(paste(length(r$df_tunnel$ID[r$df_tunnel$HitType == "Hit"]), " Successful Hits", sep = " "))
      output$tunnelWrongHits <- renderText(paste(length(r$df_tunnel$ID[r$df_tunnel$HitType == "Miss"]), " Errors", sep = " "))
      output$tunnelAverage <- renderText(paste("Average time : ", format(mean(r$df_tunnel$DeltaTime)), sep = " "))
      output$tunnelType <- renderText(paste("Input Type :", unique(r$df_tunnel$InputType), sep = " "))
      output$tunnelRespond <- renderText(paste("Input Responder :", unique(r$df_tunnel$InputResponder), sep = " "))

      output$tunnelTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          data = r$df_tunnel[r$df_tunnel$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
        ) %>%
          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )
      output$tunnelLRPlot <- renderPlot({
        req(!is.na(r$df_tunnel))
        req(!is.null(r$df_tunnel))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_tunnel %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This tunnel graph needs data from at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")        
        TunnelLRcompGG <- ggplot(r$df_tunnel[r$df_tunnel$HitType=='Hit',], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
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
        req(!is.na(r$df_tunnel))
        req(!is.null(r$df_tunnel))
        # graphreq: checks that the minimum data is needed to produce the graph.
        graphreq = nrow(r$df_tunnel %>% filter(InputType=="pressuresensor",HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This tunnel graph needs data from the pressure sensor with at least two input responders. Only",graphreq,"Available.")), errorClass = "vis")        
        TunnelLRcompPress <- ggplot(r$df_tunnel[r$df_tunnel$InputType == "pressuresensor" & r$df_tunnel$HitType=='Hit', ], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          stat_regline_equation() +
          ylab("total movement time") +
          xlab("aspect ratio ") +
          theme_bw() +
          geom_point() +
          facet_grid(~InputResponders)
        print(TunnelLRcompPress)
      })
      
      }else if (r$subject == "DHR") {
        print(paste("df_DHR filtered nrow:", nrow(r$df_DHR)))
        output$DHRHitType <- renderText(paste(length(r$df_DHR$ID[r$df_DHR$HitType == "Hit"]), " Successful Hits", sep = " "))
        output$DHRWrongHits <- renderText(paste(length(r$df_DHR$ID[r$df_DHR$HitType == "Miss"]), " Errors", sep = " "))
        output$DHRAverage <- renderText(paste("Average time : ", format(mean(r$df_DHR$DeltaTime)), sep = " "))
        output$DHRType <- renderText(paste("Input Type :", unique(r$df_DHR$InputType), sep = " "))
        output$DHRRespond <- renderText(paste("Input Responder :", unique(r$df_DHR$InputResponder), sep = " "))
        
        output$tunnelTestDetails <- renderPlotly(
          plot_ly(
            type = "scatter",
            mode = "markers",
            data = r$df_tunnel[r$df_tunnel$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
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
            
            geom_point(data=r$df_DHR[r$df_DHR$HitType=='Hit',],mapping=aes(x=ObjectWidthCm*10/CDGain, y=DeltaTime),colour="blue",size=3)+
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