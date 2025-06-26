run_gpt_price_estimation <- function(sample_data, prompt, ns) {
  showModal(show_loading_modal("The price is being calculated..."))
  
  app_obj <- approximate_price_gpt(
    data = sample_data,
    prompt = prompt,
    title = "title",
    abstract = "abstract",
    model = "gpt-4o-mini",
    rep = 1
  )
  
  removeModal()
  
  cost_estimate <- app_obj$price_dollar
  models_rpm <- rate_limits_per_minute("gpt-4o-mini")
  req_pr_min <- models_rpm$requests_per_minute
  
  #Optionally handle NA request rate limit here
  if (is.na(req_pr_min)) {
    showModal(show_rate_limit_error())
    return(NULL)
  }
  
  showModal(show_run_confirmation_modal(prompt, cost_estimate, req_pr_min, ns))
}


run_gpt_model <- function(input_data, prompt, title = "title", abstract = "abstract",
                          model = "gpt-4o-mini", reps = 1) {
  tryCatch({
    req_pr_min <- rate_limits_per_minute(model)$requests_per_minute
    
    tabscreen_gpt(
      data = input_data,
      prompt = prompt,
      title = title,
      abstract = abstract,
      model = model,
      reps = reps,
      rpm = req_pr_min
    )
  }, error = function(e) {
    message("Error running GPT model: ", e$message)
    NULL
  })
}
