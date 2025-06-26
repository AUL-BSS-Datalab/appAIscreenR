check_prompt_and_api <- function(user_prompt, api_key) {
  if ((is.null(user_prompt) || user_prompt == "") && (is.null(api_key) || api_key == "")) {
    showModal(modalDialog(
      title = "Missing prompt and API key",
      "You must enter a prompt and your API key before running the model.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }
  if (is.null(api_key) || api_key == "") {
    showModal(modalDialog(
      title = "Missing API key",
      "You must enter your OpenAI API key before running the model.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }
  if (is.null(user_prompt) || user_prompt == "") {
    showModal(modalDialog(
      title = "Missing prompt",
      "You must enter your prompt before running the model.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }
  return(TRUE)
}

show_missing_data_modal <- function() {
  modalDialog(
    title = "No Data",
    "No data available to process. Please upload a file first.",
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

show_loading_modal <- function(message = "Loading...") {
  modalDialog(
    title = "Please wait",
    message,
    footer = NULL,
    easyClose = FALSE
  )
}

show_rate_limit_error <- function() {
  modalDialog(
    title = "Request per minute is NA",
    "An error occurred while fetching rate limits. Your API key might be invalid, or there could be a connection issue.",
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

show_run_confirmation_modal <- function(prompt, cost_estimate, req_pr_min, ns = identity) {
  modalDialog(
    title = "Confirm Run",
    tagList(
      paste("The cost for this run will be approx.", round(cost_estimate, 5), "USD"),
      tags$br(), tags$br(),
      "The prompt you entered is:",
      tags$br(), tags$em(prompt),
      tags$br(), tags$br(),
      paste("You are requesting:", req_pr_min, "per minute"),
      tags$br(),
      "Are you sure you want to continue?"
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("run_screening"), "Yes, continue")
    )
  )
}