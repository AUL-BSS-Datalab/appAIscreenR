overview_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Results",
           sidebarLayout(
             sidebarPanel(
               selectInput(ns("dropdown_menu"), "Choose a view",
                           choices = c("Dataframe of differences", "Answer Data", "Price data", "Result object", "Performance Data", "Confusion Matrix")),
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Answer Data"),
                 checkboxGroupInput(ns("col_name"), "Choose column name(s)", choices = NULL)
               ),
               textInput(ns("df_name"), "Name your dataframe", value = "Dataframe"),
               actionButton(ns("save_result"), "Save dataframe for download")
             ),
             # cond_input_equals --> helpers_overview.R
             mainPanel(
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Dataframe of differences"),
                 tableOutput(ns("diff_df"))
               ),
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Answer Data"),
                 tableOutput(ns("df_answerdata"))
               ),
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Price data"),
                 tableOutput(ns("price_table"))
               ),
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Result object"),
                 tableOutput(ns("result_object"))
               ),
               conditionalPanel(cond_input_equals(ns, "dropdown_menu", "Performance Data"),
                 tableOutput(ns("performance_table"))
               ),
               conditionalPanel(
                 condition = cond_input_equals(ns, "dropdown_menu", "Confusion Matrix"),
                 tableOutput(ns("confusion_matrix"))
               )
             )
           )
  )
}

overview_server <- function(id, result_obj, saved_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive difference dataframe ---
    df_diff <- reactive({
      req(result_obj())
      req(input$dropdown_menu == "Dataframe of differences")
      
      result_obj()$answer_data %>%
        dplyr::select(human_code, decision_binary, test_id, title, abstract) %>%
        dplyr::filter(human_code != decision_binary)
    })
    
    # --- Reactive selected columns from answer_data ---
    selected_df <- reactive({
      df <- result_obj()$answer_data
      req(input$col_name)
      df[, input$col_name, drop = FALSE]
    })
    
    # --- Performance data ---
    performance_data <- reactive({
      req(result_obj())
      screen_analyzer(result_obj()$answer_data)
    })
    
    # --- Price data ---
    price_data <- reactive({
      req(result_obj())
      result_obj()$price_data
    })
    
    # --- Reactive confusion matrix ---
    
    confusion_data <- reactive({
      req(result_obj())
      tbl <- table(
        factor(result_obj()$answer_data$human_code, levels = c(0, 1)),
        factor(result_obj()$answer_data$decision_binary, levels = c(0, 1))
      )
      rownames(tbl) <- c("Excluded", "Included")   # rows = Human
      colnames(tbl) <- c("Excluded", "Included")   # columns = AI
      
      df <- as.data.frame.matrix(tbl)
      
      df <- tibble::rownames_to_column(df, var = "Human \\ AI") 
    })
    
    # --- Outputs ---
    output$diff_df <- renderTable({ df_diff() })
    
    output$df_answerdata <- renderTable({ selected_df() })
    
    output$price_table <- renderTable({ price_data() })
    
    output$result_object <- renderTable({
      req(result_obj())
      result_obj()
    })
    
    output$performance_table <- renderTable({ performance_data() })
    
    output$confusion_matrix <- renderTable({ confusion_data() })
    
    # --- Dynamic checkbox update ---
    observeEvent(result_obj(), {
      df <- result_obj()$answer_data
      req(df)
      
      ordered_cols <- get_ordered_columns(df)
      
      updateCheckboxGroupInput(session, "col_name",
                               choices = setNames(ordered_cols, ordered_cols),
                               selected = ordered_cols[1:5]
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$dropdown_menu, {
      updateTextInput(session, "df_name", value = input$dropdown_menu)
    })
    
    # --- Saving dataframes to reactive list ---
    observeEvent(input$save_result, {
      req(input$df_name)
      
      # get_df_to_save --> helpers/helpers_overview.R
      df_to_save <- get_df_to_save(input$dropdown_menu, result_obj(), selected_df(), df_diff(), price_data(), performance_data(), confusion_data()) 
      
      if (!is.null(df_to_save)) {
        saved_data$data[[input$df_name]] <- df_to_save
      }
    })
  })
}

