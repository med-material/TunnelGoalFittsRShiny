library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  
  includeCSS("custom.css"),
  useShinyjs(),
  fluidRow(
    column(8, titlePanel("Tunnel Goal Fitts Tests (Cohort 2020)")),
    column(4,
           column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib="font-awesome")),
           column(11,style = "margin-top : 20px; text-align: center;",
            selectInput("emailSelect", NULL, choices=c("Loading.." = -1))
           )
    )
  ),
  fluidRow(
    column(12, checkboxGroupInput("pidChooser", label = "Loading...", choices = NULL, inline = TRUE))
  ),
#  subjectChooser ----------------
  tabsetPanel(id = "subjectChooser", type = "tabs",
          
    tabPanel(value  = "Goal", id = "Goal", strong("Goal Test"),
        navlistPanel(
          widths = c(4, 8),
          "Choose Visualization:",
          tabPanel("Test Details",
              plotlyOutput("goalTestDetails"),
              tags$div(class = "vizcontrols-explainer"),
              textOutput("goalHitType"),
              textOutput("goalAverage"),
              textOutput("goalWrongHits"),
              textOutput("goalType"),
              textOutput("goalRespond")
          ),
          tabPanel("Input Responder Comparison",
              plotlyOutput("goalComparison"),
              tags$div(class = "vizcontrols-explainer"),
              
          tabPanel("Input Responder Comparison",
                       plotOutput('TunnelLRPlot')
                       #plotlyOutput("fittsRegPlot"),
                       #tags$div(class = "vizcontrols-explainer")
              )
          )
        )
    ),
    # PanelsFitts ---------------------------------------------------------------
        tabPanel(value  = "Fitts", id = "Fitts", strong("Fitts Law Test"),
             navlistPanel(
               widths = c(4, 8),
               "Choose Visualization:",
  
                            tabPanel("Test Details",
                        plotlyOutput("fittsTestDetails"),
                        tags$div(class = "vizcontrols-explainer"),
                        textOutput("fittsHitType"),
                        textOutput("fittsAverage"),
                        textOutput("fittsWrongHits"),
                        textOutput("fittsType"),
                        textOutput("fittsRespond")
               ),
               tabPanel("Input Responder Comparison",
                        plotOutput('fittsLRPlot')
                        #plotlyOutput("fittsRegPlot"),
                        #tags$div(class = "vizcontrols-explainer")
             ),
             tabPanel("Input Responder Comparison",
                      plotlyOutput("fittsLRLearnPlot"),
                      tags$div(class = "vizcontrols-explainer")
             )
             # ,
             # tabPanel("Fitts LR progress",
             #          plotlyOutput("fittsLRLearnPlot"),
             #          tags$div(class = "vizcontrols-explainer")
             # )
  )),
    # PanelsTunnel ---------------------------------------------------------------



    tabPanel(value = "Tunnel", id = "Tunnel", strong("Tunnel Test"),
             navlistPanel(
               widths = c(4, 8),
               "Choose Visualization:",
               tabPanel("Test Details",
                        plotlyOutput("tunnelTestDetails"),
                        tags$div(class = "vizcontrols-explainer"),
                        textOutput("tunnelHitType"),
                        textOutput("tunnelAverage"),
                        textOutput("tunnelWrongHits"),
                        textOutput("tunnelType"),
                        textOutput("tunnelRespond")
               ),
               tabPanel("Input Responder Comparison",
                        plotlyOutput("tunnelComparison"),
                        tags$div(class = "vizcontrols-explainer")
               )
             )
    )
  ),
  # Rest of Page ---------------------------------------------------------------
  
  tags$footer()
)