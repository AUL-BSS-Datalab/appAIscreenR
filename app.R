library(AIscreenR)
library(dplyr)
library(DT)
library(future)
library(htmltools)   # for html tags if needed
library(readxl)
library(shiny)
library(shinyjs)
library(synthesisr)
library(tibble)
library(zip)

# Source helpers
helper_files <- list.files("R/helpers", full.names = TRUE)
invisible(lapply(helper_files, source))

# Source modules
module_files <- list.files("R/modules", full.names = TRUE)
invisible(lapply(module_files, source))

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "font-awesome.min.css")
  ), 
  
  navbarPage("AIscreenR app",
             
             tabPanel("Upload", uploadDataUI("upload_tab")),
             
             tabPanel("Run the model",
                      sidebarLayout(
                        sidebarPanel(model_input_UI("model_ui")),
                        mainPanel(model_run_UI("model_run"))
                      )
             ),
             
             overview_UI("result"),
             download_UI("download") 
  )
)

server <- function(input, output, session) {
  saved_data <- reactiveValues(data = list())  
  uploaded_data <- uploadDataServer("upload_tab")  
  
  input_values <- model_input_server("model_ui", input_data = uploaded_data)
  result_obj <- model_run_server("model_run",
                                 api_key = input_values$api_key,
                                 user_prompt = input_values$user_prompt,
                                 run_clicked = input_values$run_clicked,
                                 input_data = uploaded_data)
  
  overview_server("result", result_obj = result_obj, saved_data = saved_data)
  download_server("download", saved_data = saved_data, data = uploaded_data$sample, result_obj = result_obj)  
}

shinyApp(ui, server)