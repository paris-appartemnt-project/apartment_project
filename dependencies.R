# This script have to be regularly update to contains all
# dependences needed to launch the app ()

# this instruction create the vector you need
# shinytemplate:::get_dependencies()


to_install <- c("rvest",
                "magrittr",
                "data.table",
                "tidyr",
                "purrr",
                "dplyr",
                "xml2",
                "stringr",
                "googleway",
                "geosphere",
                "stats",
                "tm",
                "shiny",
                "DT",
                "glue")
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }

}
