# No Remotes ----
# Attachments ----
to_install <- c("data.table", "dplyr", "DT", "geosphere", "glue", "googleway", "magrittr", "purrr", "rvest", "shiny", "stats", "stringr", "tidyr", "tm", "xml2")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }

  }
