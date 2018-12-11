# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/


library(shiny)
install.packages("googleway")
library(googleway)
library(tidyverse)
library(stringr)
library(knitr)
castorus_table <- read.csv("castorus_data_final.csv")
df <- read.csv("biens_localises.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Real Estate listings in Paris, Taylor made for you"),
  
  sidebarLayout(
    sidebarPanel(h4("Flat characteristics"),
      # textInput("name",label = "Type a firstname",value = "Vincent")
      selectizeInput("name",label = "Arrondissement",  selected = castorus_table$arrondissement, choices = castorus_table %>% distinct(arrondissement)  %>% pull(arrondissement), options = list(maxItems = 20)),
      sliderInput(inputId = "range",label = "Price range",value = summary(castorus_table$prix)[c(1,6)],summary(castorus_table$prix)[1],max= summary(castorus_table$prix)[6]),
      sliderInput(inputId = "surface",label = "Surface area",value = summary(castorus_table$m2)[c(1,6)], min = summary(castorus_table$m2)[1],max= summary(castorus_table$m2)[6]),
      sliderInput(inputId = "rooms",label = "Number of rooms",value = summary(castorus_table$piec.)[c(1,6)],summary(castorus_table$piec.)[1],max= summary(castorus_table$piec.)[6]),
      
      h4("Transport links"),
      helpText("Check box if the following criteria is important for your search, by default the algorithm will not take them into account."),
      checkboxInput(inputId = "metro",label = "5 minute walk to nearest tube station", FALSE),
      checkboxInput(inputId = "velib",label = "5 minute walk to nearest velib station",FALSE),

      h4("Daily Life"),
      helpText("Check box if the following criteria is important for your search, by default the algorithm will not take them into account."),
      checkboxInput(inputId = "school",label = "5 minute walk to nearest school", FALSE),
      checkboxInput(inputId = "velib",label = "5 minute walk to nearest velib station",FALSE),
      checkboxInput(inputId = "parking",label = "5 minute walk to nearest car park",FALSE),
      
      h4("Environment"),
      helpText("Check box if the following criteria is important for your search, by default the algorithm will not take them into account."),
      checkboxInput(inputId = "parc",label = "Do you wish to be near a park",FALSE),
      checkboxInput(inputId = "recycling",label = "Do you wish to be near a recycling center",FALSE),
      actionButton(inputId = "go",label = "go",icon = icon("refresh"))
          ),
       
    mainPanel(
      fluidPage(google_mapOutput("map")),
      dataTableOutput('table')
      # # textOutput("nb_of_birth"),
      # plotOutput("plot_popularity"),
      # DT::DTOutput("dataset")
    )
  )
)


server <- function(input, output) {
api_key <- "AIzaSyDBIYQwxvUVTn5GumtMtMZhXZtbyw7KPrM"
iconUrl <- paste0("https://developers.google.com/maps/documentation/",
                  "javascript/examples/full/images/beachflag.png")
df1 <- df[,c(6,7)]
df1$icon <- iconUrl
output$map <- renderGoogle_map({
  google_map(key = api_key, data = df1) %>% add_markers(lat = "lat", lon = "lng", marker_icon = "icon")
    })



castorus_table <- castorus_table %>% mutate(list_url_description = glue::glue("<a href='{list_url_description}'>{list_url_description} </a>"))
# castorus_table <- as.data.frame(castorus_table)
# # 
step_re <- function(min_price,max_price,min_surface,max_surface,N,min_room,max_room, castorus_table){
  castorus_table <- castorus_table %>% 
    filter(prix %in% c(min_price:max_price)) %>%
    filter(m2 %in% c(min_surface:max_surface)) %>% 
    filter(piec.%in% c(min_room:max_room)) %>% 
    filter(arrondissement %in% N)
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

#
castorus_table1 <-  reactive(step_re(price1(),price2(),surface1(),surface2(),N1(),rooms1(),rooms2(), castorus_table))
#
output$table <- renderDataTable(castorus_table1(),escape =FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

