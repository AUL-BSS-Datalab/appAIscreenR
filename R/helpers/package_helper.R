required_packages <- c(
  "AIscreenR",
  "dplyr",
  "DT",
  "future",
  "htmltools",
  "readxl",
  "shiny",
  "shinyjs",
  "synthesisr",
  "tibble",
  "zip"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))
