library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(tidyverse)
library(raster)
library(RSQLite)
library(RColorBrewer)

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






### Shiny App
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
  
  paleta <- reactive({
    if (input$ganadero == "Todos") {
      pal <- colorFactor(palette = "viridis", gps_points$codigo_gps)
    } else {
      custom_palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
      pal <- colorFactor(palette = custom_palette, ganadero_data()$codigo_gps)
    }

  })

  
  # colorstyle <- reactive({
  #   if (input$ganadero == "Todos") { 
  #     "blue" 
  #   } else { 
  #     custom_palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33')
  #     pal <- colorFactor(palette = custom_palette, ganadero_data()$codigo_gps )
  #       }
  # })
  
  
  
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
    popup_point <- paste0("<strong>Ganadero:</strong> ", filteredData()$user_name,
                          "<br><strong>GPS:</strong> ", filteredData()$codigo_gps,
                          "<br><strong>Tipo:</strong> ", filteredData()$type,
                          "<br><strong>Fecha:</strong> ", filteredData()$date_time)
    pal <- paleta()
    
    leafletProxy('mymap') %>%
      setView(
        lng = as.numeric(centro()[1]),
        lat = as.numeric(centro()[2]),
        zoom = 12
      ) %>% 
      addGlPoints(data = filteredData(), 
                  group = "pts", 
                  popup = popup_point, 
                  fillColor = ~pal(codigo_gps)) 
  })

}
  
  

shinyApp(ui, server)





