

render_screen_analyzer_table <- function(df) {
  datatable(
    df,
    escape = FALSE,
    colnames = c(
      "promptid",
      "model",
      "reps",
      "top_p",
      'p_agreement <i class="fa fa-question-circle" data-toggle="tooltip" title="Indicating the overall percent agreement between human and gpt."></i>',
      'recall <i class="fa fa-question-circle" data-toggle="tooltip" title="Measures the ability to include all articles that should be included."></i>',
      'specificity <i class="fa fa-question-circle" data-toggle="tooltip" title="Measures the ability to exclude all articles that should be excluded."></i>'
    ),
    options = list(
      dom = 't',  # Only show the table (hide filter, pagination controls, length menu)
      ordering = FALSE,
      initComplete = JS(
        "function(settings, json) {",
        "$('[data-toggle=\"tooltip\"]').tooltip();",
        "}"
      )
    )
  )
}