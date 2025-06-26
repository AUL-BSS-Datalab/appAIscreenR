
read_and_process_references <- function(file_path, file_name, human_code_value = 1) {
  ext <- tools::file_ext(file_name) |> tolower()
  
  if (!(ext %in% c("csv", "ris"))) {
    stop(paste("Unsupported file format:", ext))
  }
  
  if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
  } else if (ext == "ris") {
    df <- read_refs(file_path)
  }
  
  # Rename columns to standard names if they exist
  if ("T1" %in% names(df)) {
    df <- df |> rename(title = T1)
  } else if ("TI" %in% names(df)) {
    df <- df |> rename(title = TI)
  }
  
  if ("AB" %in% names(df)) {
    df <- df |> rename(abstract = AB)
  } else if ("N2" %in% names(df)) {
    df <- df |> rename(abstract = N2)
  }
  
  if (!("title" %in% names(df))) {
    stop("Couldn't find a title (T1 or TI)")
  }
  if (!("abstract" %in% names(df))) {
    stop("Couldn't find an abstract (AB or N2)")
  }
  
  df |> select(title, abstract) |> mutate(human_code = human_code_value) |> as_tibble()
}

sample_refs <- function(included, excluded) {
  excl_sample <- excluded |> sample_references(75)
  incl_sample <- included |> sample_references(25)
  bind_rows(excl_sample, incl_sample) |> mutate(test_id = row_number())
}


read_and_process_references_safe <- function(filepath, filename, human_code_value = 1) {
  tryCatch({
    df <- read_and_process_references(filepath, filename, human_code_value)  
    return(df)
  }, error = function(e) {
    showModal(modalDialog(
      title = paste("Error reading file:", filename),
      paste("Problem with file:", e$message),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  })
}