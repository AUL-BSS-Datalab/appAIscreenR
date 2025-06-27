
uploadDataUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Upload",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("included_file"), "Upload the file with the included ..."),
               fileInput(ns("excluded_file"), "Upload the file with the excluded")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Included Data", tableOutput(ns("included_table"))),
                 tabPanel("Excluded Data", tableOutput(ns("excluded_table"))),
                 tabPanel("Test Data", tableOutput(ns("merged_table")))
                 #tabPanel("Sample Data", tableOutput(ns("sample_table")))               
                )
             )
           )
  )
}

uploadDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    included_data <- reactive({
      req(input$included_file)
      read_and_process_references_safe(input$included_file$datapath, input$included_file$name, human_code_value = 1) # Comes from helpers_data_process.R
    })
    
    excluded_data <- reactive({
      req(input$excluded_file)
      read_and_process_references_safe(input$excluded_file$datapath, input$excluded_file$name, human_code_value = 0) # Comes from helpers_data_process.R
    })
    
    merged_data <- reactive({
      req(included_data(), excluded_data())
      bind_rows(excluded_data(), included_data()) |> mutate(test_id = row_number())
    })
    
    sample_data1 <- reactive({
      req(included_data(), excluded_data())
      sample_refs(included_data(), excluded_data())
    })
    
    sample_data <- reactive({
      req(sample_data1())
      sample_data1()[1:10, ]
    })
    
    
    # Outputs
    output$included_table <- renderTable({ included_data() })
    output$excluded_table <- renderTable({ excluded_data() })
    output$merged_table   <- renderTable({ merged_data() })
    #output$sample_table   <- renderTable({ sample_data() })  # Full sample
    
    # Returns the reactive expressions - not their values
    return(list(
      included = included_data,
      excluded = excluded_data,
      sample = merged_data,
      merged = merged_data
    ))
  })
}