library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(tidyverse)
library(raster)





gps <- read_csv("/Users/ajpelu/Nextcloud/sumhal/sumhal_vreGI/rawdata/GPS_MC/all.csv")

dicc_gps <- readxl::read_excel("/Users/ajpelu/SERPAM Dropbox/01_LIFE_WATCH/06_Tareas/02_T2_Fase_experimental_campo/A2_5_Dispositivos_GPS/base_datos_gps/dicc_gps.xlsx", 
                               sheet = "dicc_dispositivos") %>% 
  rename(id_gps = codigo_gps) %>% 
  dplyr::select(-id)

gps <- inner_join(gps, dicc_gps)

# gps <- read_csv("/Users/ajpelu/Desktop/Serrato/todos.csv")
gps_points <- st_as_sf(gps, coords = c("long", "lat"), crs = 4326)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafglOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                selectInput("ganadero", "Ganadero", unique(gps_points$user_name)),
                # selectInput("gps", "GPS device", unique(gps_points$id_gps), multiple = TRUE),
                dateRangeInput("dateRange",
                               "Filtrar por fecha",
                               start = as.Date('2021-12-31'), 
                               end = as.Date(Sys.Date())
                )
  )
)




server <- function(input, output, session) {
  
  
  # Reactive expression for the data subsetted to what the user selected
  selected_gps <- reactive({
    gps_points %>% 
      filter(user_name == input$ganadero) %>% 
      # filter(id_gps == input$gps) %>% 
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  })
  
  # Crear el mapa 
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = -3.5, lat = 37.5, zoom = 7) %>% 
      addGlPoints(data = selected_gps(), group = "pts") %>%
      addLayersControl(overlayGroups = "pts")
    
    
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





