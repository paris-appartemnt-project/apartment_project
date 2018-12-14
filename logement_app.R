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
                 radioButtons(inputId="metromaybe", label="Do you need to be within a 500 m walk to a tube station?", choices=c("Yes"=1,"Not interested"=0), selected = 0, inline = T),
                 conditionalPanel(condition = "input.metromaybe == 1",
                                  wellPanel(
                                    sliderInput(inputId = "metro",label = "Select the level of access to the tube that you want, 1 being the lowest service in Paris and 10 the higest",value = summary(tri_categories$stations_qt)[c(1,6)],summary(tri_categories$stations_qt)[1],max= summary(tri_categories$stations_qt)[6])
                                  ) ),
                 
                 h3("Daily Life"),
                 radioButtons(inputId = "schoolmaybe",label = "Do you need to be within a 500 m walk to a tube station?", choices=c("Yes"=1,"Not interested"=0), selected = 0, inline = T),
                 conditionalPanel(condition = "input.schoolmaybe == 1",
                                  wellPanel(
                                    sliderInput(inputId = "school",label = "Select the level of school density that you want, 1 being the lowest density in Paris and 10 being the highest",value = summary(tri_categories$schools_qt)[c(1,6)],summary(tri_categories$schools_qt)[1],max= summary(tri_categories$schools_qt)[6])
                                  ) ),
                 radioButtons(inputId = "commercemaybe",label = "Enjoy having everything close to you? Select yes if you want to be near bakeries, supermarkets, butchers, etc...", choices=c("Yes"=1,"Not interested"=0), selected = 0, inline = T),
                 conditionalPanel(condition = "input.commercemaybe == 1",
                                  wellPanel(
                                    sliderInput(inputId = "commerce",label = "Select the density of shops you need, 1 being a neighbourhood with few options and 10 with a lot",value = summary(tri_categories$commerce_qt)[c(1,6)],summary(tri_categories$commerce_qt)[1],max= summary(tri_categories$commerce_qt)[6])
                                  ) ),
                 
                 h3("Entertainment"),
                 radioButtons(inputId = "restomaybe",label = "Fancy going out a lot?", choices=c("Yes"=1,"Not interested"=0), selected = 0, inline = T),
                 conditionalPanel(condition = "input.restomaybe == 1",
                                  wellPanel(
                                    sliderInput(inputId = "resto",label = "Select the level of activity you want, 1 being a very quite neighbourhood and 10 being a lively one",value = summary(tri_categories$anime_resto_cine_qt)[c(1,6)],summary(tri_categories$anime_resto_cine_qt)[1],max= summary(tri_categories$anime_resto_cine_qt)[6])
                                  ) )
                 
    ),
    
    mainPanel(
      fluidPage(google_mapOutput("map"), DT::dataTableOutput('table1'))
    )
    
  )
)


server <- function(input, output) {
  castorus_table <- castorus_table %>% dplyr::select(-X1) %>% rename("X" = X1_1, "Change_price" = c(11)) 
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
  metroyes <- reactive(input$metromaybe)
  schoolyes <- reactive(input$schoolmaybe)
  commerceyes <- reactive(input$commercemaybe)
  restoyes <- reactive(input$restomaybe)
  N1 <- reactive(input$name)
  castorus_table_fixed <- reactive(castorus_table)
  
  #
  castorus_table1 <- reactive(step_re(price1(),price2(),surface1(),surface2(),N1(),rooms1(),rooms2(), castorus_table))
  #
  iconUrl <- paste0("https://developers.google.com/maps/documentation/",
                    "javascript/examples/full/images/beachflag.png")
  
  #' Title
  #'
  #' @param castorus_table k
  #' @param tri 
  #' @param min 
  #' @param max 
  #' @param value 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  filter_slider <- function(castorus_table, tri, min, max, value){
    temp<- tri %>% dplyr::filter(tri_categories[,value] %in% c(min:max)) %>% select(X) 
    a <- castorus_table %>% dplyr::filter(X %in% temp$X)
    return(a)
  }
  
  a_metro <- reactive({
    a1 <- if (metroyes() == 1){
      metro1 <- reactive(min(input$metro))
      metro2 <- reactive(max(input$metro))
      castorus_table2 <- filter_slider(castorus_table1(),tri_categories, metro1(), metro2(), 8)
    } else {
      castorus_table2 <- castorus_table1()
    }
    a_metro <- left_join(castorus_table2,castorus_table1())
  })
  
  a_school <- reactive({
    a2 <- if (schoolyes() == 1){
      school1 <- reactive(min(input$school))
      school2 <- reactive(max(input$school))
      castorus_table3 <- filter_slider(castorus_table1(),tri_categories, school1(), school2(), 7)
    } else {
      castorus_table3 <- a_metro()
    }
    a2 <-left_join(castorus_table3,a_metro())
  })
  
  a_commerce <- reactive({
    a3 <- if (commerceyes() == 1){
      commerce1 <- reactive(min(input$commerce))
      commerce2 <- reactive(max(input$commerce))
      castorus_table4 <- filter_slider(castorus_table1(),tri_categories, commerce1(), commerce2(), 6)
    } else {
      castorus_table4 <- a_school()
    }
    a3 <- left_join(castorus_table4,a_school())
  })  
  
  a_resto <- reactive({
    a4 <- if (restoyes() == 1){
      resto1 <- reactive(min(input$resto))
      resto2 <- reactive(max(input$resto))
      castorus_table5 <- filter_slider(a_commerce(),tri_categories, resto1(), resto2(), 9)
    } else {
      castorus_table5 <- a_commerce()
    }
    a4 <- left_join(castorus_table5,a_metro())
  })  
  
  df2 <- reactive(a_resto())
  df1 <- reactive(coordinates(df2()))
  
  output$table1 <- DT::renderDataTable({DT::datatable(df2())}, escape = FALSE,options=list(stateSave = TRUE))
  
  coordinates <- function(castorus_table){
    a <- castorus_table %>% dplyr::select("latitude","longitude") %>% dplyr::mutate(icon = iconUrl)
    return(a)
  }
  
  api_key <- reactive("AIzaSyDBIYQwxvUVTn5GumtMtMZhXZtbyw7KPrM")
  
  column_index <- function(c, df1){
    df1 <- df1[c,]
  }
  
  bb <- reactive({input$table1_rows_selected})
  a <-  reactive(column_index(input$table1_rows_selected,coordinates(df2())))
  output$map <- renderGoogle_map({
    google_map(key = api_key(),  data = column_index(input$table1_rows_selected,coordinates(df2())), location = c(48.87515, 2.339354), zoom =  11) %>% add_markers(lat = "latitude", lon = "longitude", marker_icon = "icon")
  })
  
  
  
  output$x4 = renderPrint({
    s = input$table1_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

