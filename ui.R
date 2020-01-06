library(shiny)
library(plotly)


ui <- fluidPage(
  
  
  # titlePanel(
  
  fluidRow(
    #Title
    column(9,
           titlePanel(title = "TunnelGoalFitts Visualizer (Cohort 2020)")
    ),                            
    #Icon
    column(1,
           style = "margin-top: 20px; text-align: right;",
           icon("user", class = "fa-2x")                                                  
    ),
    #DropDown
    column(2,
           style = "margin-top: 20px;",
           selectInput('in1', 
                       NULL,
                       c(Choose='', state.name), selectize=FALSE
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
                       )
                       
                       
                       
                       
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
                )
                
              )
  )
)