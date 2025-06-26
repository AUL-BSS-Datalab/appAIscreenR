
model_input_UI <- function(id) {
  ns <- NS(id)
  tagList(
    passwordInput(ns("api_key"), "Enter your OpenAI API key:"),
    textAreaInput(ns("user_prompt"), "Enter prompt",
                  placeholder = "Add your prompt here",
                  width = "100%", rows = 15),
    actionButton(ns("confirm_run"), "Run the AIscreenR")
  )
}

model_input_server <- function(id, input_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    continue_run_trigger <- reactiveVal(NULL)
    
    observeEvent(input$api_key, {
      req(input$api_key)
      AIscreenR::set_api_key(input$api_key)
      showNotification("API key set successfully!", type = "message")
    })
    
    
    observeEvent(input$confirm_run, {
      if (!check_prompt_and_api(input$user_prompt, input$api_key)) # Comes from helpers_model.R
        return(NULL)
      
      
      run_gpt_price_estimation(input_data$sample(), input$user_prompt, ns) # Comes from model_run_module.R
    })
    
    
    observeEvent(input$run_screening, {
      continue_run_trigger(runif(1))  # signal the run
    })
    
    return(list(
      api_key = reactive(input$api_key),
      user_prompt = reactive(input$user_prompt),
      run_clicked = continue_run_trigger
    ))
  })
}