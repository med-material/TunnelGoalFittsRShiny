library(shiny)
library(plotly)


ui <- fluidPage(
  
  
  titlePanel(
    
    fluidRow(
      
      column(9,"TunnelGoalFitts Visualizer (Cohort 2020)"),                           #Title
      
      column(3,                                                                    #DropDown
             
             
             verbatimTextOutput('outtt'),
             selectInput('in1', 
                         '',
                         c(Choose='', state.name), selectize=FALSE)
      )
    )
  ),
  
  
  
  tabsetPanel(type = "tab",
              
              tabPanel(strong("Test Details"), 
                       
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       column(4, h4("Choose Test")
                       ),
                       
                       
                       fluidRow(
                         
                         column(9,
                                verbatimTextOutput('out'),
                                selectInput('in1', 'Options', c(Choose='', state.name), selectize=FALSE)
                         )
                       )
              ),
              
              tabPanel(strong("Compare Tests"), verbatimTextOutput("summary"))
  ),
  
  mainPanel(
    plotlyOutput("plot1")
    
  )
)

