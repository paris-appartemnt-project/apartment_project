#' Run the shiny app
#'
#' @export
#' @import googleway stringr
#' @importFrom shiny h3 fluidPage titlePanel sidebarLayout sidebarPanel selectizeInput sliderInput helpText mainPanel reactive shinyApp
#' @importFrom glue glue
#' @importFrom DT dataTableOutput


shiny_app <- function() {
  castorus_data <- castorus_data
  score_table <- score_table
  ui <- fluidPage(
    titlePanel("Real Estate listings in Paris, taylor made for you"),

    sidebarLayout(
      sidebarPanel(h3("Flat characteristics"),
                   selectizeInput("name",label = "Arrondissement",  selected = castorus_data$arrondissement, choices = castorus_data %>% distinct(arrondissement)  %>% pull(arrondissement), options = list(maxItems = 20)),
                   sliderInput(inputId = "range",label = "Price range",value = summary(castorus_data$prix)[c(1,6)],summary(castorus_data$prix)[1],max= summary(castorus_data$prix)[6]),
                   sliderInput(inputId = "surface",label = "Surface area",value = summary(castorus_data$m2)[c(1,6)], min = summary(castorus_data$m2)[1],max= summary(castorus_data$m2)[6]),
                   sliderInput(inputId = "rooms",label = "Number of rooms",value = summary(castorus_data$piec.)[c(1,6)],summary(castorus_data$piec.)[1],max= summary(castorus_data$piec.)[6]),

                   helpText("Time for the fun part. Enjoy a night out on the town, having the best options for your kids future or living in a quiet environment, then customise your search to find the flat that is made for you. In the following section, check the boxes if you are interested in a specific criteria, if you not by default the algorithm will not take them into account."),

                   h3("Transport links"),
                   sliderInput(inputId = "metro",label = "What is the level of public transport you require, 1 being the highest level of public transport within 500m from your flat in Paris, and 10 being the lowest.",value = summary(score_table$stations_qt)[c(1,6)],summary(score_table$stations_qt)[1],max= summary(score_table$stations_qt)[6]),

                   h3("Daily Life"),
                   sliderInput(inputId = "school",label = "Picky chooser? Select the level of school density you want near your flat, 1 being the highest density of schools within 500m from your flat in Paris, and 10 being the lowest.",value = summary(score_table$schools_qt)[c(1,6)],summary(score_table$schools_qt)[1],max= summary(score_table$schools_qt)[6]),
                   sliderInput(inputId = "commerce",label = "Bit of a shopaholic? Select the level of shop density (bakery, butcher, supermarket, etc..) you want near your flat, 1 being the highest density of shops within 500m from your flat in Paris, and 10 being the lowest.",value = summary(score_table$commerce_qt)[c(1,6)],summary(score_table$commerce_qt)[1],max= summary(score_table$commerce_qt)[6]),

                   h3("Entertainment"),
                   sliderInput(inputId = "resto",label = "Fancy going out a lot? Select the level of activity you want, 1 being a very busy neighbourhood and 10 being a quiet one",value = summary(score_table$anime_resto_cine_qt)[c(1,6)],summary(score_table$anime_resto_cine_qt)[1],max= summary(score_table$anime_resto_cine_qt)[6])
      ),

      mainPanel(
        fluidPage(google_mapOutput("map")),
        fluidPage(DT::dataTableOutput('table1', width = "50%", height = "10%"))
      )

    )
  )


  server <- function(input, output) {
    castorus_data <- castorus_data %>% rename("X" = X1, "Change_price" = c(11)) %>%  dplyr::select(-depuis,-Change_price,lextrait.n)
    score_table <- score_table %>% dplyr::select(-X) %>%  rename("X" = apartment_index)
    castorus_data <- castorus_data %>% mutate(list_url_description = glue::glue("<a href='{list_url_description}'>{list_url_description} </a>"))
    # castorus_data <- as.data.frame(castorus_data)
    # #


    # # Filter on neighborhood
    price1 <- reactive(min(input$range))
    price2 <- reactive(max(input$range))
    surface1 <- reactive(min(input$surface))
    surface2 <- reactive(max(input$surface))
    rooms1 <- reactive(min(input$rooms))
    rooms2 <- reactive(max(input$rooms))
    N1 <- reactive(input$name)
    castorus_data_fixed <- reactive(castorus_data)

    #
    castorus_data1 <- reactive(get_minimalfeatures(price1(),price2(),surface1(),surface2(),N1(),rooms1(),rooms2(), castorus_data))

    #
    iconUrl <- paste0("https://developers.google.com/maps/documentation/",
                      "javascript/examples/full/images/beachflag.png")

    metro1 <- reactive(min(input$metro))
    metro2 <- reactive(max(input$metro))
    castorus_data2 <- reactive(filter_slider(castorus_data1(), score_table, metro1(), metro2(), 8))
    school1 <- reactive(min(input$school))
    school2 <- reactive(max(input$school))
    castorus_data3<- reactive(filter_slider(castorus_data2(), score_table, school1(), school2(), 7))
    commerce1 <- reactive(min(input$commerce))
    commerce2 <- reactive(max(input$commerce))
    castorus_data4 <- reactive(filter_slider(castorus_data3(), score_table, commerce1(), commerce2(), 6))
    resto1 <- reactive(min(input$resto))
    resto2 <- reactive(max(input$resto))
    castorus_data5 <- reactive(filter_slider(castorus_data4(),score_table, resto1(), resto2(), 9))
    df2 <- reactive(castorus_data5())
    df1 <- reactive(coordinates(df2()))

    output$table1 <- DT::renderDataTable({df2()}, escape = FALSE)

    api_key <- reactive(my.env$google_key)

    output$map <- renderGoogle_map({
      google_map(key = api_key(),  data = column_index(input$table1_rows_selected,coordinates(df2(), iconUrl)), location = c(48.87515, 2.339354), zoom = 11) %>% add_markers(lat = "latitude", lon = "longitude", marker_icon = "icon")
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
