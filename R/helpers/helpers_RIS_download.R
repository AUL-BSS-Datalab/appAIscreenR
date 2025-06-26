# helper functions for 
rename_to_ris_tags <- function(df) {
  df_renamed <- df
  if ("title" %in% names(df_renamed)) df_renamed <- rename(df_renamed, TI = title)
  if ("abstract" %in% names(df_renamed)) df_renamed <- rename(df_renamed, N2 = abstract)
  df_renamed
}

write_ris_to_file <- function(df, file) {
  ris_lines <- unlist(lapply(seq_len(nrow(df)), function(i) {
    ref <- df[i, ]
    lines <- c()
    
    if ("TY" %in% names(ref) && !is.na(ref$TY) && nzchar(trimws(ref$TY))) {
      lines <- c(lines, paste0("TY  - ", ref$TY))
    } else {
      lines <- c(lines, "TY  - JOUR")
    }
    
    for (col in names(ref)) {
      if (col %in% c("TY", "ER")) next  
      
      val <- as.character(ref[[col]])
      if (!is.na(val) && nzchar(trimws(val))) {
        lines <- c(lines, paste0(col, "  - ", val))
      }
    }
    
    c(lines, "ER  -", "")
  }))
  writeLines(ris_lines, con = file)
}