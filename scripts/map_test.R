library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(tidyverse)
library(raster)
library(RSQLite)

### Aquí ira un código que limpiará todos los datos y los guardará como Rds o algo similiar en la app. 
### Ahora trabajaremos con gps 

con <- dbConnect(RSQLite::SQLite(), dbname = here::here("db/db_gps.db"))

# Get dicc Dispositivos 
dicc_gps <- dbGetQuery(con, "SELECT * FROM dicc_dispositivos")

gps <- dbGetQuery(con, "SELECT * FROM datos_gps") |>  
  dplyr::mutate(date_time = lubridate::as_datetime(time_stamp))

gps <- inner_join(gps, dicc_gps, by="codigo_gps")


# Filter by zone 
gps <- gps |> 
  filter(lat != 0) |> 
  filter(lat < 40 & lat > 36) |>  
  filter(lng != 0) |> 
  filter(lng > -7 & lng < 0)

### Hasta aquí llegara el limpiado de datos. Se cargarán los datos y se transformarán en spatial data 

gps_points <- st_as_sf(gps, coords = c("lng", "lat"), crs = 4326)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafglOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                
                selectInput(inputId = "ganadero", 
                            label = "Ganadero", 
                            choices = c("Todos", unique(gps_points$user_name))),
                # selectInput("gps", "GPS device", unique(gps_points$id_gps), multiple = TRUE),
                dateRangeInput("dateRange",
                               "Filtrar por fecha",
                               min = as.Date('2021-12-31'),
                               start = as.Date('2021-12-31'), 
                               end = as.Date(Sys.Date()),
                               max = as.Date(Sys.Date())
                )
  )
)


server <- function(input, output, session) {
  
  
  # filtrar por ganadero 
  ganadero_data <- reactive({ 
    if (input$ganadero == "Todos") { 
      gps_points
    } else {
      gps_points |> dplyr::filter(user_name == input$ganadero)
      }
    })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData  <- reactive({
    ganadero_data()  |>
      dplyr::filter(date_time >= input$dateRange[1] & date_time <= input$dateRange[2])
  })
  
  observeEvent(input$ganadero, {
    updateDateRangeInput(session,
                         inputId = "dateRange",
                         min = as.Date('2021-12-31'),
                         max = as.Date(Sys.Date()),
                         start = min(ganadero_data()$date_time),
                         end = max(ganadero_data()$date_time))
    updateDateRangeInput(session,
                         inputId = "dateRange",
                         min = min(ganadero_data()$date_time),
                         max = max(ganadero_data()$date_time))
  }
  )
  
  
  # Center the map to the selected features 
  centro <- reactive({
    coordinates(as(extent(filteredData()), "SpatialPolygons"))
  })
  
  
  # Crear el mapa 
  output$mymap <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      # addProviderTiles(providers$OpenStreetMap) %>% 
      addWMSTiles('http://www.ideandalucia.es/wms/mdt_2005?',
                  layers = 'Sombreado_10',
                  options = WMSTileOptions(format = "image/png", transparent = TRUE),
                  attribution = '<a href="http://www.juntadeandalucia.es/institutodeestadisticaycartografia" target="_blank">Instituto de Estadística y Cartografía de Andalucía</a>', 
                  group = 'Hillshade') %>% 
      addWMSTiles('http://www.ideandalucia.es/wms/mta10v_2007?',
                  layers = 'mta10v_2007',
                  options = WMSTileOptions(format = "image/png", transparent = FALSE),
                  attribution = '<a href="http://www.juntadeandalucia.es/institutodeestadisticaycartografia" target="_blank">Instituto de Estadística y Cartografía de Andalucía</a>',
                  group = 'topo2007') %>% 
      addLayersControl(overlayGroups = "pts", 
                       baseGroups = c("Hillshade","topo2007")) 
  })
  
  

  observe({
    leafletProxy('mymap') %>%
      setView(
        lng = as.numeric(centro()[1]),
        lat = as.numeric(centro()[2]),
        zoom = 12
      ) %>% 
      addGlPoints(data = filteredData(), group = "pts") 
  })
  # Reactive que devuelve los puntos de los gps seleccionados


  
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  
  # output$map <- renderLeaflet({
  #   # Use leaflet() here, and only include aspects of the map that
  #   # won't need to change dynamically (at least, not unless the
  #   # entire map is being torn down and recreated).
  #   # leaflet() %>% addTiles() %>%
  #   #   fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  #   # 
  #   
  #     addGlPoints(data = filteredData()) 
  #   
  # })
  # 
  
  # observe({
  #   # Un observer para establcer el mapbox de los datos 
  #   coordinates(filteredData()) <- ~long+lat
  #   proj4string(filteredData()) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  #   mapcenter <- coordinates(as(extent(filteredData()), "SpatialPolygons"))
  #   
  # })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # observe({
  #   # pal <- colorpal()
  #   
  #   leafletProxy("map", data = gps) %>%
  #     addGlPoints(data = filteredData(), 
  #               popup = ~paste0("ID GPS: ", filteredData()$id_gps)) %>%
  #     setView(lng = mapcenter()[1] , lat = mapcenter()[2], zoom = 12)
  #   
  # })
  
  
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
}

shinyApp(ui, server)





