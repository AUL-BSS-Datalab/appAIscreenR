create_download_handler <- function(data, name, save_rdata_input) {
  downloadHandler(
    filename = function() {
      if (isTRUE(save_rdata_input())) {
        paste0(name, "_data.zip")
      } else {
        paste0(name, ".csv")
      }
    },
    content = function(file) {
      df <- data[[name]]
      
      if (isTRUE(save_rdata_input())) {
        tmp_dir <- tempfile()
        dir.create(tmp_dir)
        
        csv_path <- file.path(tmp_dir, paste0(name, ".csv"))
        write.csv(df, csv_path, row.names = FALSE)
        
        rdata_path <- file.path(tmp_dir, paste0(name, ".RData"))
        save(list = name, file = rdata_path, envir = list2env(data))
        
        zip::zip(zipfile = file, files = c(csv_path, rdata_path), mode = "cherry-pick")
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

create_all_data_download_handler <- function(saved_data, save_rdata_input) {
  downloadHandler(
    filename = function() {
      paste0("saved_data_", Sys.Date(), ".zip")
    },
    content = function(zipfile) {
      temp_dir <- tempdir()
      file_paths <- c()
      
      for (name in names(saved_data$data)) {
        df <- saved_data$data[[name]]
        
        csv_path <- file.path(temp_dir, paste0(name, ".csv"))
        write.csv(df, csv_path, row.names = FALSE)
        file_paths <- c(file_paths, csv_path)
        
        if (isTRUE(save_rdata_input())) {
          rdata_path <- file.path(temp_dir, paste0(name, ".RData"))
          save(list = name, file = rdata_path, envir = list2env(saved_data$data))
          file_paths <- c(file_paths, rdata_path)
        }
      }
      
      zip::zip(zipfile, files = file_paths, mode = "cherry-pick")
    }
  )
}

render_saved_data_list_ui <- function(ns, saved_data) {
  if (length(saved_data$data) == 0) {
    return(tags$p("No saved dataframes yet"))
  }
  
  tagList(
    lapply(names(saved_data$data), function(name) {
      fluidRow(
        column(6,
               actionLink(ns(paste0("select_", name)),
                          label = name,
                          style = "font-weight: bold; text-align:left;")
        ),
        column(6,
               div(style = "display: flex; justify-content: flex-end; align-items: center;",
                   downloadLink(ns(paste0("download_btn_", name)),
                                label = icon("download"),
                                style = "color: black; font-size: 14px; text-decoration: none; margin-right: 10px;"),
                   actionLink(ns(paste0("delete_", name)),
                              label = icon("times"),
                              style = "color: black; font-size: 14px; background: none; border: none; text-decoration: none;")
               )
        )
      )
    })
  )
}
