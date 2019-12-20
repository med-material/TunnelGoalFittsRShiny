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
                       
                       sidebarLayout(
                         # Inputs
                         sidebarPanel(
                           
                           selectInput('in1', 'Choose Test', c(Choose='', state.name), selectize=FALSE),
                           
                           p("Test Details")
                           #RebderText To generate the text
                           
                         ),
                         
                         
                         
                         
                       
                         
                         
                      
                         mainPanel(
                           
                           plotlyOutput("plot1"),
                           selectInput('in1', '', c(Choose='', state.name), selected = 'Index of Difficulty', selectize=FALSE)
                         )
                       ),
                       
                       
                       
                       
              ),
              
              tabPanel(
                strong("Compare Test"),
                
                sidebarLayout(
                  # Inputs
                  sidebarPanel(
                    
                    selectInput('in1', 'Test Type', c(Choose='',state.name), selectize=FALSE),
                    
                    p("Input To Compare : "),
                    
                    radioButtons("button",
                                 "Compare Human Performance to..",
                                 c("Eyes and Eyes tracker", "Nose and Mouse", "Right Hand and Mouse"))
                    
                  ),
                  
                  mainPanel(
                    
                    plotlyOutput("plot2"),
                    selectInput('in1', '', c(Choose='', state.name), selectize=FALSE)
                  )
                ),
                
                )
  )
)