library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(ggpmisc)  # for stat_regline_equation()

# CSV upload module (provided in your repo)
# global.R already sources this, but sourcing here is harmless if you prefer:
# source("modules/csv_upload_module.R", local = TRUE)

server <- function(input, output, session) {
  
  # VARIABLES
  r <- reactiveValues(
    df_all = NULL,
    local_data = TRUE,
    df_goal = NULL,
    df_tunnel = NULL,
    df_fitts = NULL,
    df_DHR = NULL,
    df_goalAgg = NULL,
    df_fittsAgg = NULL,
    df_DHRagg = NULL,
    pid_index = NULL,
    pid_name = NULL,
    pid_email = NULL,
    pid_query = NULL,
    subject = "Goal", # "Tunnel", "Fitts", "DHR"
    participants = NULL,
    choices = NULL
  )
  
  # module instance
  csv_data <- callModule(csv_upload, "uploadData")
  
  # Open the modal for CSV upload
  observeEvent(input$CsvButton, {
    insertUI(selector = "#CsvButton", where = "afterEnd",
             ui = showModal(modalDialog(csv_upload_UI("uploadData"), easyClose = TRUE)))
  })
  
  # Handle CSV upload
  observeEvent(csv_data$trigger, {
    req(csv_data$trigger > 0)
    req(!is.null(csv_data$df))
    
    r$df_all     <- RefreshDataLocal(csv_data$df)  # from global.R
    r$local_data <- TRUE
    
    # Build email dropdown from uploaded data + "Everyone's Data"
    emails  <- sort(unique(r$df_all$Email))
    choices <- c(setNames("NA", "Everyone's Data"), emails)
    default_sel <- if (length(choices) > 0) names(choices)[1] else "-1"
    
    updateSelectInput(session, "emailSelect",
                      choices = choices,
                      selected = default_sel)
    
    updateTabsetPanel(session, "subjectChooser", selected = "Goal")
  })
  
  # Read URL parameters (subject, pid, email)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[["subject"]])) {
      r$subject <- query[["subject"]]
      updateTabsetPanel(session, "subjectChooser", selected = r$subject)
    }
    if (!is.null(query[["pid"]])) {
      r$pid_query <- query[["pid"]]
      r$pid_name  <- query[["pid"]]
    }
    
    # Only update email dropdown if data is loaded
    if (!is.null(r$df_all)) {
      emails  <- sort(unique(r$df_all$Email))
      choices <- c(setNames("NA", "Everyone's Data"), emails)
      if (!is.null(query[["email"]]) && query[["email"]] %in% c("NA", emails)) {
        updateSelectInput(session, "emailSelect", choices = choices, selected = query[["email"]])
      } else {
        updateSelectInput(session, "emailSelect", choices = choices)
      }
    } else {
      updateSelectInput(session, "emailSelect",
                        choices = c("Press Upload CSV to Upload Data." = "-1"))
    }
  })
  
  # Change of subject tab
  observeEvent(input$subjectChooser, {
    if (input$subjectChooser != r$subject) {
      r$subject <- input$subjectChooser
      UpdatePIDSelection()
      UpdateVisualizations()
    }
  })
  
  # PID filter changes
  observeEvent(input$pidChooser, {
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
      r$pid_name  <- unlist(r$participants[input$pidChooser, "PID"])
      r$pid_email <- unlist(r$participants[input$pidChooser, "Email"])
    } else {
      r$pid_index <- NULL
      r$pid_name  <- NULL
      r$pid_email <- NULL
    }
    print(paste("pidChooser: pid_index ", r$pid_index))
    print(paste("pidChooser: pid_name ", r$pid_name))
    UpdateVisualizations()
  }, ignoreInit = TRUE)
  
  # Email selection -> compute datasets from the uploaded CSV only
  observeEvent(input$emailSelect, {
    if (input$emailSelect == "-1") return()
    req(!is.null(r$df_all))
    
    # Subset by selected email (keep all if "Everyone's Data")
    local_df <- r$df_all
    if (input$emailSelect != "NA") {
      local_df <- local_df %>% filter(Email == input$emailSelect)
    }
    
    # Prepare columns used downstream
    local_df <- local_df %>%
      mutate(TargetNumber = as.numeric(TargetNumber),
             hitScore     = ifelse(HitType == "Hit", 1, 0))
    
    # Split by game type
    r$df_goal   <- local_df %>% filter(GameType == "Goal")
    r$df_goal$FittsID <- with(r$df_goal,
                              log2(2 * ObjectDistanceCm / ObjectHeightCm))
    r$df_goalAgg <- r$df_goal %>%
      filter(is.na(CDGain)) %>%
      group_by(Email, PID, InputType, InputResponders, FittsID) %>%
      summarize(meanMT = mean(DeltaTime, na.rm = TRUE), .groups = "drop")
    
    r$df_tunnel <- local_df %>% filter(GameType == "Tunnel")
    r$df_tunnel$aspectRatio <- with(r$df_tunnel,
                                    ObjectDistanceCm / ObjectWidthCm)
    
    r$df_fitts <- local_df %>% filter(GameType == "Fitts") %>% filter(DeltaTime < 5)
    r$df_fitts$FittsID <- with(r$df_fitts,
                               log2(2 * ObjectDistanceCm / ObjectWidthCm))
    r$df_fittsAgg <- r$df_fitts %>%
      filter(HitType == "Hit", is.na(CDGain)) %>%
      group_by(Email, PID, InputType, InputResponders, FittsID) %>%
      summarize(meanMT = mean(DeltaTime, na.rm = TRUE), .groups = "drop")
    
    r$df_DHR <- local_df %>%
      filter(GameType == "Fitts", !is.na(CDGain)) %>%
      mutate(FittsID = log2(2 * (ObjectDistanceCm + ObjectWidthCm) / ObjectWidthCm))
    r$df_DHRagg <- r$df_DHR %>%
      group_by(Email, PID, InputType, InputResponders, FittsID, CDGain, ObjectWidthCm) %>%
      summarize(MTmean = mean(DeltaTime),
                errorRate = 1 - mean(hitScore),
                .groups = "drop")
    
    UpdatePIDSelection()
    UpdateVisualizations()
  })
  
  observeEvent(input$Param, {
    UpdateVisualizations()
  })
  
  # --------- Helpers ----------------------------------------------------------
  
  UpdatePIDSelection <- function() {
    # Update PID Choosers to show PID numbers based on the data
    if (r$subject == "Goal") {
      r$participants <- unique(r$df_goal %>% group_by(Email) %>% distinct(PID))
    } else if (r$subject == "Fitts") {
      r$participants <- unique(r$df_fitts %>% group_by(Email) %>% distinct(PID))
    } else if (r$subject == "Tunnel") {
      r$participants <- unique(r$df_tunnel %>% group_by(Email) %>% distinct(PID))
    } else if (r$subject == "DHR") {
      r$participants <- unique(r$df_DHR %>% group_by(Email) %>% distinct(PID))
    } else {
      r$participants <- NULL
    }
    
    if (!is.null(r$participants)) {
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(seq_len(nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    } else {
      r$choices <- NULL
    }
    
    if (!is.null(r$pid_query)) {
      r$pid_name  <- r$pid_query
      r$pid_query <- NULL
      r$pid_index <- unname(r$choices[names(r$choices) == r$pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", r$pid_name))
      print(paste("PIDQuery: pid_index", r$pid_index))
    }
    print(r$choices)
    print(ifelse(is.null(r$participants), 0, nrow(r$participants)))
    
    if (is.null(r$choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data",
                               "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    } else if (is.null(r$pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:",
                               "pidChooser", choices = r$choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", r$pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:",
                               "pidChooser", choices = r$choices, selected = r$pid_index, inline = TRUE)
    }
  }
  
  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", r$pid_name))
    print(paste("UpdateVis subject: ", r$subject))
    print(paste("UpdateVis subject==Fitts: ", r$subject == "Fitts"))
    print(paste("df_all nrow:", ifelse(is.null(r$df_all), 0, nrow(r$df_all))))
    print(paste("df_goal nrow:", ifelse(is.null(r$df_goal), 0, nrow(r$df_goal))))
    
    # Filter visualization data based on pid_name (in-place filters)
    if (!is.null(r$pid_name)) {
      r$df_all <- r$df_all %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name)
      r$df_goal <- r$df_goal %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name)
      r$df_tunnel <- r$df_tunnel %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name)
      r$df_fitts <- r$df_fitts %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name)
      r$df_DHR <- r$df_DHR %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name) %>%
        filter(!is.na(CDGain))
      r$df_DHRagg <- r$df_DHRagg %>%
        filter(Email %in% r$pid_email) %>% filter(PID %in% r$pid_name) %>%
        filter(!is.na(CDGain))
    }
    
    if (r$subject == "Goal") {
      print(paste("df_goal filtered nrow:", nrow(r$df_goal)))
      
      output$goalHitType   <- renderText(paste(length(r$df_goal$ID[r$df_goal$HitType == "Hit"]), " Successful Hits"))
      output$goalWrongHits <- renderText(paste(length(r$df_goal$ID[r$df_goal$HitType == "Miss"]), " Errors"))
      output$goalAverage   <- renderText(paste("Average time : ", format(mean(r$df_goal$DeltaTime)), sep = " "))
      output$goalType      <- renderText(paste("Input Type :", paste(unique(r$df_goal$InputType), collapse=", "), sep = " "))
      output$goalRespond   <- renderText(paste("Input Responder :", paste(unique(r$df_goal$InputResponders), collapse=", "), sep = " "))
      
      output$goalTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          data = r$df_goal, x = ~TargetNumber, y = ~DeltaTime
        ) %>%
          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )
      
      output$goalLRPlot <- renderPlot({
        req(!is.null(r$df_goal))
        graphreq <- nrow(r$df_goal %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      output$goalLRSinglePlot <- renderPlot({
        req(!is.null(r$df_goal))
        graphreq <- nrow(r$df_goal %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_goal, aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
      })
      
      output$goalLRPressurePlot <- renderPlot({
        req(!is.null(r$df_goal))
        graphreq <- nrow(r$df_goal %>% filter(InputType=="pressuresensor") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input responders, both using the pressure sensor input type. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_goal[r$df_goal$InputType == "pressuresensor", ], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(~InputResponders)
      })
      
      output$GoalDeviceComp <- renderPlot({
        req(!is.null(r$df_goal))
        graphreq <- nrow(r$df_goal %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input types (devices). Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_goal[r$df_goal$HitType=='Hit',], aes(FittsID, DeltaTime, group = InputType, colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      output$GoalDeviceCompSPlot <- renderPlot({
        req(!is.null(r$df_goal))
        graphreq <- nrow(r$df_goal %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This goal graph needs data from at least two input types (devices). Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_goal[r$df_goal$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
      })
      
      output$GoalDeviceCompAgg <- renderPlot({
        ggplot(r$df_goalAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
    } else if (r$subject == "Fitts") {
      print(paste("df_fitts filtered nrow:", nrow(r$df_fitts)))
      
      output$fittsHitType   <- renderText(paste(length(r$df_fitts$ID[r$df_fitts$HitType == "Hit"]), " Successful Hits"))
      output$fittsWrongHits <- renderText(paste(length(r$df_fitts$ID[r$df_fitts$HitType == "Miss"]), " Errors"))
      output$fittsAverage   <- renderText(paste("Average time : ", format(mean(r$df_fitts$DeltaTime)), sep = " "))
      output$fittsType      <- renderText(paste("Input Type :", paste(unique(r$df_fitts$InputType), collapse=", "), sep = " "))
      output$fittsRespond   <- renderText(paste("Input Responder :", paste(unique(r$df_fitts$InputResponder), collapse=", "), sep = " "))
      
      output$fittsTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          data = r$df_fitts[r$df_fitts$HitType=='Hit',], x = ~TargetNumber, y = ~DeltaTime
        ) %>%
          layout(
            xaxis = list(title = "TargetNumber"),
            yaxis = list(title = "Movment Time (s)"),
            legend = list(orientation = "h")
          )
      )
      
      output$fittsLRPlot <- renderPlot({
        req(!is.null(r$df_fitts))
        graphreq <- nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      output$fittsLRSinglePlot <- renderPlot({
        req(!is.null(r$df_fitts))
        graphreq <- nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, group =InputResponders, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
      })
      
      output$fittsLRPlotPressure <- renderPlot({
        req(!is.null(r$df_fitts))
        graphreq <- nrow(r$df_fitts  %>% filter(InputType=="pressuresensor", HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This fitts graph needs data from the pressure sensor with at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_fitts[r$df_fitts$InputType == "pressuresensor" & r$df_fitts$HitType=='Hit', ], aes(FittsID, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(~InputResponders)
      })
      
      output$fittsDeviceComp <- renderPlot({
        req(!is.null(r$df_fitts))
        graphreq <- nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input types (devices). Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime, colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      output$fittsDeviceCompSPlot <- renderPlot({
        req(!is.null(r$df_fitts))
        graphreq <- nrow(r$df_fitts %>% filter(HitType=="Hit") %>% distinct(InputType))
        validate(need(graphreq > 1, paste("This fitts graph needs data from at least two input types (devices). Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_fitts[r$df_fitts$HitType=='Hit',], aes(FittsID, DeltaTime,  colour = InputType)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
      })
      
      output$fittsDeviceCompAgg <- renderPlot({
        ggplot(r$df_fittsAgg, aes(FittsID, meanMT, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          ylab("movement time + confirmation in seconds") +
          xlab("ID") +
          theme_bw() +
          geom_point() +
          stat_regline_equation(color = "black", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      # Simple scatter used in "Test Details" tab
      output$fittsTestDetails <- renderPlotly(
        plot_ly(
          type = "scatter",
          mode = "markers",
          data = r$df_fitts[r$df_fitts$HitType=='Hit',], x = ~FittsID, y = ~DeltaTime
        ) %>%
          layout(
            xaxis = list(title = "Fitts ID"),
            yaxis = list(title = "Movement Time (s)"),
            legend = list(orientation = "h")
          )
      )
      
    } else if (r$subject == "Tunnel") {
      print(paste("df_tunnel filtered nrow:", nrow(r$df_tunnel)))
      
      output$tunnelHitType   <- renderText(paste(length(r$df_tunnel$ID[r$df_tunnel$HitType == "Hit"]), " Successful Hits"))
      output$tunnelWrongHits <- renderText(paste(length(r$df_tunnel$ID[r$df_tunnel$HitType == "Miss"]), " Errors"))
      output$tunnelAverage   <- renderText(paste("Average time : ", format(mean(r$df_tunnel$DeltaTime)), sep = " "))
      output$tunnelType      <- renderText(paste("Input Type :", paste(unique(r$df_tunnel$InputType), collapse=", "), sep = " "))
      output$tunnelRespond   <- renderText(paste("Input Responder :", paste(unique(r$df_tunnel$InputResponders), collapse=", "), sep = " "))
      
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
        req(!is.null(r$df_tunnel))
        graphreq <- nrow(r$df_tunnel %>% filter(HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This tunnel graph needs data from at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_tunnel[r$df_tunnel$HitType=='Hit',], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          stat_regline_equation() +
          ylab("total movement time") +
          xlab("tunnel aspect ratio (length/height)") +
          theme_bw() +
          geom_point() +
          facet_grid(cols = vars(InputResponders), rows = vars(InputType))
      })
      
      output$TunnelLRPlotPressure <- renderPlot({
        req(!is.null(r$df_tunnel))
        graphreq <- nrow(r$df_tunnel %>% filter(InputType=="pressuresensor", HitType=="Hit") %>% distinct(InputResponders))
        validate(need(graphreq > 1, paste("This tunnel graph needs data from the pressure sensor with at least two input responders. Only", graphreq, "Available.")), errorClass = "vis")
        ggplot(r$df_tunnel[r$df_tunnel$InputType == "pressuresensor" & r$df_tunnel$HitType=='Hit', ], aes(aspectRatio, DeltaTime, colour = InputResponders)) +
          geom_smooth(method = "lm", fill = NA) +
          stat_regline_equation() +
          ylab("total movement time") +
          xlab("aspect ratio ") +
          theme_bw() +
          geom_point() +
          facet_grid(~InputResponders)
      })
      
    } else if (r$subject == "DHR") {
      print(paste("df_DHR filtered nrow:", nrow(r$df_DHR)))
      
      output$DHRPlot <- renderPlot({
        ggplot(r$df_DHRagg, aes(x = ObjectWidthCm*10/CDGain, y = MTmean)) +
          geom_point(data = r$df_DHR[r$df_DHR$HitType=='Hit',],
                     mapping = aes(x = ObjectWidthCm*10/CDGain, y = DeltaTime),
                     colour = "blue", size = 3) +
          geom_line() + geom_point(colour = "Red", size = 5) +
          ylab("total movement time") +
          xlab("target size in mm (in motor space)") +
          theme_bw() + scale_x_reverse()
      })
      
      output$DHRerrPlot <- renderPlot({
        ggplot(r$df_DHRagg, aes(x = ObjectWidthCm*10/CDGain, y = errorRate)) +
          geom_line() + geom_point(colour = "Red", size = 5) +
          ylab("error rate (misses)") +
          xlab("target size in mm (in motor space)") +
          theme_bw() + scale_x_reverse()
      })
    }
  }
}
