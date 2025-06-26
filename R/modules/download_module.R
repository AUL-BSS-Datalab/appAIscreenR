download_UI <- function(id) {
  ns <- NS(id)
  tabPanel("Download",
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("saved_list_ui")),
               div(style = "margin-top: 15px; margin-bottom: 15px;"),
               downloadButton(ns("download_all"), "Download all"),
               checkboxInput(ns("save_rdata"), "Also save as .RData", value = FALSE),
               br(), br(),
               downloadButton(ns("download_all_ris"), "Download AI RIS files results")
             ),
             mainPanel(
               uiOutput(ns("saved_tables_ui"))
             )
           )
  )
}

download_server <- function(id, saved_data, data, result_obj) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_preview <- reactiveVal(NULL)
    
    # List of saved dataframes with preview, download, delete
    output$saved_list_ui <- renderUI({
      render_saved_data_list_ui(ns, saved_data)
    })
    
    # Preview logic
    observe({
      lapply(names(saved_data$data), function(name) {
        observeEvent(input[[paste0("select_", name)]], {
          current_preview(name)
        }, ignoreInit = TRUE)
      })
    })
    
    output$saved_tables_ui <- renderUI({
      req(current_preview())
      tagList(
        h4(paste("Preview of", current_preview())),
        tableOutput(ns("preview_table"))
      )
    })
    
    output$preview_table <- renderTable({
      req(current_preview())
      head(saved_data$data[[current_preview()]], 10)
    })
    
    # Delete logic
    observe({
      lapply(names(saved_data$data), function(name) {
        observeEvent(input[[paste0("delete_", name)]], {
          tmp <- saved_data$data
          tmp[[name]] <- NULL
          saved_data$data <- tmp
          if (identical(current_preview(), name)) current_preview(NULL)
        }, ignoreInit = TRUE)
      })
    })
    
    # Per-dataset download buttons
    observe({
      lapply(names(saved_data$data), function(name) {
        local({
          nm <- name
          output[[paste0("download_btn_", nm)]] <-
            create_download_handler(saved_data$data, nm, reactive(input$save_rdata))
        })
      })
    })
    
    # "Download all" button
    output$download_all <- create_all_data_download_handler(saved_data, reactive(input$save_rdata))
    
    # --- RIS download ---
    joined_data <- reactive({
      req(data(), result_obj())
      
      data() |>
        left_join(
          result_obj()$answer_data |> select(test_id, decision_binary),
          by = "test_id"
        )
    })
    
    included_result <- reactive({
      req(joined_data())
      
      joined_data() |>
        filter(decision_binary == 1) |>
        select(-test_id, -decision_binary, -human_code) |>
        rename_to_ris_tags()
    })
    
    excluded_result <- reactive({
      req(joined_data())
      
      joined_data() |>
        filter(decision_binary == 0) |>
        select(-test_id, -decision_binary, -human_code) |>
        rename_to_ris_tags()
    })
    
    output$download_all_ris <- downloadHandler(
      filename = function() {
        paste0("ai_ris_", Sys.Date(), ".zip")
      },
      content = function(file) {
        tmpdir <- tempdir()
        incl_file <- file.path(tmpdir, "included.ris")
        excl_file <- file.path(tmpdir, "excluded.ris")
        
        write_ris_to_file(rename_to_ris_tags(included_result()), incl_file)
        write_ris_to_file(rename_to_ris_tags(excluded_result()), excl_file)
        
        old_wd <- setwd(tmpdir)
        on.exit(setwd(old_wd))  # Ensure we return after
        
        zip(zipfile = file, files = c("included.ris", "excluded.ris"))
      },
      contentType = "application/zip"
    )
  })
}
