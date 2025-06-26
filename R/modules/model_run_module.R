
model_run_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Performance:"),
    DTOutput(ns("gpt_result")),
    h4("Confusion Matrix"),
    DTOutput(ns("crosstab_dt")),
    h4("Agreements vs disagreements"),
    verbatimTextOutput(ns("difference_summary")),
    h4("Summary"),
    DTOutput(ns("summary_table"))
  )
}

model_run_server <- function(id, api_key, user_prompt, run_clicked, input_data) {
  moduleServer(id, function(input, output, session) {
    
    result_obj <- reactiveVal(NULL)
    run_time <- reactiveVal(NULL)
    
    
    observeEvent(run_clicked(), {
      if (!check_prompt_and_api(user_prompt(), api_key())) # Comes from helper_modal.R
        return()
      
      
      showModal(show_loading_modal("Running the model...")) # Comes from helpers_modal.R
      
      start_time <- Sys.time()
      
      result_object <- run_gpt_model(
        input_data = input_data$sample(),
        prompt = user_prompt(),
        title = "title",      
        abstract = "abstract" 
      ) # run_gpt_model comes from the helpers_model.R where it uses the tabscreen_gpt from AIscreenR 
      
      end_time <- Sys.time()
      run_time(end_time - start_time)
      
      removeModal()
      
      # Update the reactiveVal
      result_obj(result_object)
    })
    
    output$gpt_result <- renderDataTable({
      req(result_obj())
      
      df <- screen_analyzer(result_obj()$answer_data)
      render_screen_analyzer_table(df) # Shows the screen_analyzer output in a datatable defined in helpers_datatable.R
    })
    
    output$crosstab_dt <- renderDT({
      req(result_obj())
      
      # Create confusion matrix with fixed factor levels (for the order) 
      tbl <- table(
        factor(result_obj()$answer_data$human_code, levels = c(0, 1)),
        factor(result_obj()$answer_data$decision_binary, levels = c(0, 1))
      )
      
      
      rownames(tbl) <- c("Excluded", "Included")   # rows = Human
      colnames(tbl) <- c("Excluded", "Included")   # columns = AI
      
      # Convert to data.frame
      df <- as.data.frame.matrix(tbl)
      df <- tibble::rownames_to_column(df, var = "Human \\ AI")  
      
      datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = "t",           
          ordering = FALSE
        )
      )
    })
    
    output$summary_table <- renderDT({
      req(result_obj())
      data <- result_obj()$answer_data
      
      human_counts <- table(factor(data$human_code, levels = c(1, 0)))
      ai_counts <- table(factor(data$decision_binary, levels = c(1, 0)))
      
      summary_df <- data.frame(
        Decision = c("Included (1)", "Excluded (0)"),
        Human = c(human_counts["1"], human_counts["0"]),
        AI = c(ai_counts["1"], ai_counts["0"])
      )
      
      datatable(
        summary_df,
        rownames = FALSE,
        options = list(
          dom = "t",
          ordering = FALSE
        )
      )
    })
    
    differences <- reactive({
      req(result_obj())
      subset_results <- result_obj()$answer_data %>%
        dplyr::select(human_code, decision_binary, test_id, title, abstract)
      
      total_rows <- nrow(subset_results)
      disagreements <- sum(subset_results$human_code != subset_results$decision_binary, na.rm = TRUE)
      agreements <- total_rows - disagreements
      
      time_info <- if (!is.null(run_time())) paste0("Run time: ", round(as.numeric(run_time(), units = "secs"), 2), " seconds\n") else ""
      
      paste0(
        time_info, "\n\n",
        "Out of ", total_rows, " cases:\n",
        "- ", disagreements, " disagreement(s)\n",
        "- ", agreements, " agreement(s)"
      )
    })
    
    output$difference_summary <- renderText({
      differences()
    })
    
    
    return(result_obj)
  })
}