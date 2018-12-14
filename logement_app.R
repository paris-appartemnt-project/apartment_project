# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/


library(shiny)
# install.packages("googleway")
library(googleway)
library(tidyverse)
library(stringr)
library(knitr)
library(quantmod)

castorus_table <- as.data.frame(read_csv("final_table_castorus2.csv"))
tri_categories <- read.csv("score_table.csv")

ui <- fluidPage(
  
  titlePanel("Real Estate listings in Paris, taylor made for you"),
  
  sidebarLayout(
    sidebarPanel(h3("Flat characteristics"),
                 selectizeInput("name",label = "Arrondissement",  selected = castorus_table$arrondissement, choices = castorus_table %>% distinct(arrondissement)  %>% pull(arrondissement), options = list(maxItems = 20)),
                 sliderInput(inputId = "range",label = "Price range",value = summary(castorus_table$prix)[c(1,6)],summary(castorus_table$prix)[1],max= summary(castorus_table$prix)[6]),
                 sliderInput(inputId = "surface",label = "Surface area",value = summary(castorus_table$m2)[c(1,6)], min = summary(castorus_table$m2)[1],max= summary(castorus_table$m2)[6]),
                 sliderInput(inputId = "rooms",label = "Number of rooms",value = summary(castorus_table$piec.)[c(1,6)],summary(castorus_table$piec.)[1],max= summary(castorus_table$piec.)[6]),
                 
                 helpText("Time for the fun part. Enjoy a night out on the town, having the best options for your kids future or living in a quiet environment, then customise your search to find the flat that is made for you. In the following section, check the boxes if you are interested in a specific criteria, if you not by default the algorithm will not take them into account."),
                 
                 h3("Transport links"),
                 sliderInput(inputId = "metro",label = "What is the level of public transport you require, 1 being the highest level of public transport within 500m from your flat in Paris, and 10 being the lowest.",value = summary(tri_categories$stations_qt)[c(1,6)],summary(tri_categories$stations_qt)[1],max= summary(tri_categories$stations_qt)[6]),
                 
                 h3("Daily Life"),
                 sliderInput(inputId = "school",label = "Picky chooser? Select the level of school density you want near your flat, 1 being the highest density of schools within 500m from your flat in Paris, and 10 being the lowest.",value = summary(tri_categories$schools_qt)[c(1,6)],summary(tri_categories$schools_qt)[1],max= summary(tri_categories$schools_qt)[6]),
                 sliderInput(inputId = "commerce",label = "Bit of a shopaholic? Select the level of shop density (bakery, butcher, supermarket, etc..) you want near your flat, 1 being the highest density of shops within 500m from your flat in Paris, and 10 being the lowest.",value = summary(tri_categories$commerce_qt)[c(1,6)],summary(tri_categories$commerce_qt)[1],max= summary(tri_categories$commerce_qt)[6]),
                 
                 h3("Entertainment"),
                 sliderInput(inputId = "resto",label = "Fancy going out a lot? Select the level of activity you want, 1 being a very busy neighbourhood and 10 being a quiet one",value = summary(tri_categories$anime_resto_cine_qt)[c(1,6)],summary(tri_categories$anime_resto_cine_qt)[1],max= summary(tri_categories$anime_resto_cine_qt)[6])
    ),
    
    mainPanel(
      fluidPage(google_mapOutput("map")), 
      fluidPage(DT:::dataTableOutput('table1', width = "50%", height = "10%"))
    )
    
  )
)


server <- function(input, output) {
  castorus_table <- castorus_table %>% dplyr::select(-X1) %>% rename("X" = X1_1, "Change_price" = c(11)) %>%  dplyr::select(-depuis,-Change_price,lextrait.n)
  tri_categories <- tri_categories %>% dplyr::select(-X) %>%  rename("X" = apartment_index) 
  castorus_table <- castorus_table %>% mutate(list_url_description = glue::glue("<a href='{list_url_description}'>{list_url_description} </a>"))
  # castorus_table <- as.data.frame(castorus_table)
  # # 
  step_re <- function(min_price,max_price,min_surface,max_surface,N,min_room,max_room, castorus_table){
    castorus_table <- castorus_table %>% 
      filter(prix %in% c(min_price:max_price)) %>%
      filter(m2 %in% c(min_surface:max_surface)) %>% 
      filter(piec.%in% c(min_room:max_room)) %>% 
      filter(arrondissement %in% N) %>% 
      dplyr::select(-titre,-vue.le,-Type)
    return(castorus_table)
  }
  
  
  # # Filter on neighborhood
  price1 <- reactive(min(input$range))
  price2 <- reactive(max(input$range))
  surface1 <- reactive(min(input$surface))
  surface2 <- reactive(max(input$surface))
  rooms1 <- reactive(min(input$rooms))
  rooms2 <- reactive(max(input$rooms))
  N1 <- reactive(input$name)
  castorus_table_fixed <- reactive(castorus_table)
  
  #
  castorus_table1 <- reactive(step_re(price1(),price2(),surface1(),surface2(),N1(),rooms1(),rooms2(), castorus_table))
  #
  iconUrl <- paste0("https://developers.google.com/maps/documentation/",
                    "javascript/examples/full/images/beachflag.png")
  
  #' The Function inputs a dataframe and filters out the quantiles for a specific category that the user wants or do not want.
  #'
  #' @param castorus_table a data frame we are going to modify
  #' @param tri The dataframe with the scores
  #' @param min The miminum quantile we want.
  #' @param max The highest quantile we want.
  #' @param value Select the column in tri which is going to be filtered.
  #'
  #' @return a data frame
  #' @export
  #'
  filter_slider <- function(castorus_table, tri, min, max, value){
    temp<- tri %>% dplyr::filter(tri_categories[,value] %in% c(min:max)) %>% select(X) 
    a <- castorus_table %>% dplyr::filter(X %in% temp$X)
    return(a)
  }
  
  metro1 <- reactive(min(input$metro))
  metro2 <- reactive(max(input$metro))
  castorus_table2 <- reactive(filter_slider(castorus_table1(),tri_categories, metro1(), metro2(), 8))
  
  school1 <- reactive(min(input$school))
  school2 <- reactive(max(input$school))
  castorus_table3<- reactive(filter_slider(castorus_table2(),tri_categories, school1(), school2(), 7))
  
  commerce1 <- reactive(min(input$commerce))
  commerce2 <- reactive(max(input$commerce))
  castorus_table4 <- reactive(filter_slider(castorus_table3(),tri_categories, commerce1(), commerce2(), 6))
  
  resto1 <- reactive(min(input$resto))
  resto2 <- reactive(max(input$resto))
  castorus_table5 <- reactive(filter_slider(castorus_table4(),tri_categories, resto1(), resto2(), 9))
  
  df2 <- reactive(castorus_table5())
  df1 <- reactive(coordinates(df2()))
  
  output$table1 <- DT::renderDataTable({df2()}, escape = FALSE)
  
#' The function takes a dataframe and inputs a new column for the marker image, and also selects the longitude and lattitude column.
#'
#' @param castorus_table work on this data frame to obtain the longitude, the lattitude and icon shape colum. 
#'
#' @return a data frame with longitude lattitude and the icon shape column.
#' @export
#'
#' @examples
  coordinates <- function(castorus_table){
    a <- castorus_table %>% dplyr::select("latitude","longitude") %>% dplyr::mutate(icon = iconUrl)
    return(a)
  }
  
  api_key <- reactive("AIzaSyDBIYQwxvUVTn5GumtMtMZhXZtbyw7KPrM")
  
#' This function is used to be able to obtain the indexes of the selected rows in the datatable.
#'
#' @param c the indexes of the selected rows
#' @param df1 the dataframe which is going to be used.
#'
#' @return a data frame.
#' @export
#'
#' @examples
  column_index <- function(c, df1){
    df1 <- df1[c,]
  }
  
  output$map <- renderGoogle_map({
    google_map(key = api_key(),  data = column_index(input$table1_rows_selected,coordinates(df2())), location = c(48.87515, 2.339354), zoom = 11) %>% add_markers(lat = "latitude", lon = "longitude", marker_icon = "icon") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

