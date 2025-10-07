#csv_upload_df = NULL
csv_upload_UI <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>CSV File Upload</h3>",
         "<p>Upload files in here, which represent the
         <strong>TunnelGoalFitts</strong>
         data.</p>"),
    
    fileInput(
      ns("file"),
      "Choose TunnelGoalFitts CSV Files", placeholder = "Select CSV file", multiple=T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    actionButton(ns("actionSubmit"), "Submit"),
    textOutput(ns("statusText"))
  )
}

csv_upload <- function(input, output, session) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    df = NULL,
    trigger = 0
  )
  
  observeEvent(input$actionSubmit, {
    if (!is.null(input$file)) {
      
      toReturn$df = csv_upload_combine_data(input$file$datapath)
      toReturn$trigger <- toReturn$trigger + 1
    }
  })
  
  observeEvent(toReturn$df, {
    req(!is.null(toReturn$df))
    output$statusText <- renderText({ " Data Received Successfully!" })
    insertUI(selector = paste0("#", ns("statusText")), where="afterBegin",
            ui = icon("check", class = "fa-1x", lib="font-awesome"))
  })
  
  return(toReturn)
}

csv_upload_combine_data <- function(files) {
  data_list = lapply(files, function(file) {
    read.csv(file, na.strings="NULL", sep=";", row.names=NULL)
  })
  #browser()
  if (length(data_list) > 1) {
    data = Reduce(function(x, y) bind_rows(x, y), data_list)
  } else {
    data <- data_list[[1]]
  }
  return(data)
}