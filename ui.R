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
           
           selectInput("mail",
                       NULL,
                       choices = GenerateSelectChoices(default = "Everyone's Data", text = "", fieldName = "UserID")
           ),          
    )
    
  ),
  
  
  
  
  tabsetPanel(type = "tab",
              
              
              tabPanel(strong("Test Details"), 
                       
                       sidebarLayout(
                         # Inputs
                         sidebarPanel(
                           
                           uiOutput("Test"),
                           
                           
                           
                           p("Test Details"),
                           #RebderText To generate the text
                           textOutput("GameType"),
                           textOutput("HitType"),
                           textOutput("Average"),
                           textOutput("WrongHits"),
                           textOutput("Type"),
                           textOutput("Respond")
                           
                           
                         ),
                         
                         
                         
                         mainPanel(
                           
                           plotlyOutput("plot1"),
                           
                           # selectInput("Index",
                           #             "Index of Difficulty",
                           #             choices = GenerateSelectChoices(default = "Index of Difficulty", 
                           #                                             fieldName = "GameType"),
                           #             selected = "Index of Difficulty")
                           uiOutput("dropdown_index")
                           
                         )
                       )
              ),
              
              tabPanel(
                strong("Compare Test"),
                
                sidebarLayout(
                  # Inputs
                  sidebarPanel(
                    
                    selectInput("Type",
                                "Test Type",
                                choices = GenerateSelectChoices(default = "Fitts", text = "", fieldName = "GameType")),
                    
                    
                    
                    checkboxGroupInput("comparaison", "Input To Compare:",
                                       c("Cylinders" = "cyl",
                                         "Transmission" = "am",
                                         "Gears" = "gear")),            
                    
                    
                  ),
                  
                  mainPanel(
                    
                    plotlyOutput("plot2"),
                    
                    selectInput("Index2",
                                NULL,
                                choices = GenerateSelectChoices(default = "Index of Difficulty", text = "", fieldName = "GameType"),
                    ),
                  )
                )
                
              )
  )
  
)
