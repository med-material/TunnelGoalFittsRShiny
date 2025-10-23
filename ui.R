library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  includeCSS("custom.css"),
  useShinyjs(),
  fluidRow(
    column(8, titlePanel("Tunnel Goal Fitts Tests (Cohort 2021)")),
    column(
      4,
      column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib = "font-awesome")),
      column(11,
             style = "margin-top : 20px; text-align: center;",
             actionButton("CsvButton","Upload CSV"),
             selectInput("emailSelect", NULL, choices = c("Press Upload CSV to Upload Data." = "-1"))
      )
    )
  ),
  fluidRow(
    column(12, checkboxGroupInput("pidChooser", label = "Loading...", choices = NULL, inline = TRUE))
  ),
  #  subjectChooser ----------------
  tabsetPanel(
    id = "subjectChooser", type = "tabs",
    
    tabPanel(
      value = "Goal", id = "Goal", strong("Goal Test"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Test Details",
          plotlyOutput("goalTestDetails"),
          tags$div(class = "vizcontrols-explainer"),
          textOutput("goalHitType"),
          textOutput("goalAverage"),
          textOutput("goalWrongHits"),
          textOutput("goalType"),
          textOutput("goalRespond")
        ),
        tabPanel(
          "Input Responder Comparison",
          plotOutput("goalLRPlot"),
          plotOutput("goalLRSinglePlot")
        ),
        tabPanel(
          "Pressure Input Comparison",
          plotOutput("goalLRPressurePlot")
        ),
        tabPanel(
          "Input Device Comparison",
          plotOutput("GoalDeviceComp"),
          plotOutput("GoalDeviceCompSPlot")
        ),
        tabPanel(
          "Input Device Comparison Agg by PID",
          plotOutput("GoalDeviceCompAgg")
        )
      )
    ),
    
    tabPanel(
      value = "Fitts", id = "Fitts", strong("Fitts Law Test"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Test Details",
          plotlyOutput("fittsTestDetails"),
          tags$div(class = "vizcontrols-explainer"),
          textOutput("fittsHitType"),
          textOutput("fittsAverage"),
          textOutput("fittsWrongHits"),
          textOutput("fittsType"),
          textOutput("fittsRespond")
        ),
        tabPanel(
          "Input Responder Comparison",
          plotOutput("fittsLRPlot"),
          plotOutput("fittsLRSinglePlot")
        ),
        tabPanel(
          "Pressure Input Responder comparison",
          plotOutput("fittsLRPlotPressure")
        ),
        tabPanel(
          "Input Device Comparison",
          plotOutput("fittsDeviceComp"),
          plotOutput("fittsDeviceCompSPlot")
        ),
        tabPanel(
          "Input Device Comparison Agg by PID",
          plotOutput("fittsDeviceCompAgg")
        )
      )
    ),
    
    tabPanel(
      value = "Tunnel", id = "Tunnel", strong("Tunnel Test"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Test Details",
          plotlyOutput("tunnelTestDetails"),
          tags$div(class = "vizcontrols-explainer"),
          textOutput("tunnelHitType"),
          textOutput("tunnelAverage"),
          textOutput("tunnelWrongHits"),
          textOutput("tunnelType"),
          textOutput("tunnelRespond")
        ),
        tabPanel(
          "Input Responder Comparison",
          plotOutput("tunnelLRPlot")
        ),
        tabPanel(
          "Pressure Input Responder comparison",
          plotOutput("TunnelLRPlotPressure")
        )
      )
    ),
    
    tabPanel(
      value = "DHR", id = "DHR", strong("DHR test"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Test Details",
          plotOutput("DHRPlot"),
          plotOutput("DHRerrPlot"),
          tags$div(class = "vizcontrols-explainer")
        )
      )
    ),
    
    # Rest of Page ---------------------------------------------------------------
    tags$footer()
  )
)
