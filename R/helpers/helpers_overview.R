# --- Helpers used in overview_module ---

# --- Determines when a specific panel should be shown based on the value of a dropdown input ---
cond_input_equals <- function(ns, input_id, value) {
  sprintf("input['%s'] == '%s'", ns(input_id), value)
}

# --- Setting the default columns to be the first --- 
get_ordered_columns <- function(df, default_columns = c("human_code", "decision_binary", "test_id", "title", "abstract")) {
  unique(c(default_columns, names(df)))
}

# --- Selects the correct dataset to save, display or download based on the label
get_df_to_save <- function(choice, result_obj, selected_df, df_diff, price_data, performance_data, confusion_data) {
  switch(choice,
         "Dataframe of differences" = df_diff,
         "Price data" = price_data,
         "Result object" = result_obj,
         "Answer Data" = selected_df,
         "Performance Data" = performance_data,
         "Confusion Matrix" = confusion_data,
         NULL
  )
}
