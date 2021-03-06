usethis::use_build_ignore("devstuffs.R")
usethis::use_git_ignore(".Rprofile")
usethis::use_build_ignore(".Rprofile")
usethis::use_mit_license("benjamin")
usethis::use_pipe()
usethis::use_package("magrittr")
usethis::use_package("dplyr")
usethis::use_package("rvest")
usethis::use_package("data.table")
usethis::use_package("xml2")
usethis::use_package("purrr")
usethis::use_package("tidyr")
usethis::use_package("stringr")
usethis::use_package("googleway")
usethis::use_package("geosphere")
usethis::use_package("stats")
usethis::use_package("tm")
usethis::use_package("shiny")
usethis::use_package("glue")
castorus_data <- read.csv("final_table_castorus.csv")
schools <- read.csv2("schools.csv")
squares <- read.csv2("squares.csv")
cinemas <- read.csv2("cinemas.csv")
stores <- read.csv2("stores.csv")
stations <- read.csv2("stations.csv")
commerces <- read.csv2("commerces.csv")
restaurants <- read.csv2("restaurants.csv")
score_table <- read.csv("score_table.csv")
roadnames <- read.csv("cleanroad-2.csv")
metros <- read.csv("cleanstation-2.csv")
usethis::use_vignette("data_scraping_castorus")
usethis::use_vignette("apartments_location")
usethis::use_vignette("scoring_algorithm")
usethis::use_vignette("location_information")
usethis::use_vignette("colum_index")
usethis::use_vignette("coordinates")
usethis::use_vignette("shiny_app")
usethis::use_data(castorus_data, schools, cinemas, stores, commerces, restaurants,  stations, score_table, roadnames, metros, overwrite = TRUE, internal = TRUE)
devtools::install_github("ThinkR-open/attachment")
devtools::build_vignettes()
devtools::check()


