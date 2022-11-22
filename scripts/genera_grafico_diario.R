
library(tidyverse)
library(purrr)

# Leer todas los archivos csv 

input_folder <- "/Users/ajpelu/Desktop/test/"

a <- list.files(path = input_folder, full.names = TRUE, pattern = ".csv")

# Generar un dataframe con todos los datos 
data <- a %>%
  map(read_csv) %>% 
  reduce(rbind)

# Calcula el numero de registros diario y genera un gráfico
data %>% 
  mutate(date = lubridate::make_date(paste0("20", year), month, day)) %>% 
  group_by(date, id_gps) %>% 
  summarise(n = length(id_gps)) %>% 
  ggplot(aes(y=n, x=date)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept = (60/5)*23) +
  facet_wrap(~id_gps, ncol = 1) +
  theme_bw()















# ui object
ui <- fluidPage(
  
  # App title ----
  titlePanel(
    title =
      div("Convertidor de datos de GPS de medida continua al formato estándar",
          p(),
          img(src = "/logo_serpam.jpg", height = "50", " SERPAM-EEZ")),
    windowTitle = "GPS Converter"
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Selecciona archivo",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Encabezamiento?", TRUE),
      
      numericInput("skip", "Saltar líneas?", 0, min=0, max=10), 
      
      # Input: Select separator ----
      radioButtons("sep", "Separador",
                   choices = c("coma (,)" = ",",
                               "punto y coma (;)"  = ";",
                               "tabulador" = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      #radioButtons("disp", "Display",
      #             choices = c(Head = "head",
      #                         All = "all"),
      #            selected = "head"),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
  ))

# Define server logic to read selected file ----
server <- function(input, output) {
  
  file_name <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    return (stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
  })
  
  
  ##### Selected data and typologies 
  datasetInput <- reactive({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        dfraw <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote, 
                          skip = input$skip)
        
        df <- dfraw %>% 
          mutate(id_gps = file_name()) %>% 
          dplyr::select_if(~ !all(is.na(.))) %>% 
          rename(
            id = V1,
            year = V11,
            month = V12,
            day = V13,
            hour = V14,
            minute = V15,
            second = V16,
            lat = V17,
            long = V18
          ) %>%
          filter(lat != -1 | long != -1) 
        #%>%
        # mutate(
        #   date = as.POSIXct(paste(year, month, day, sep = "-"), format = "%y-%m-%d"),
        #   time = hms::as_hms(paste(hour, minute, second, sep = ":"))
        # ) %>%
        # mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S")) %>%
        #dplyr::select(id, id_gps, lat, long, datetime, date, time)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # return(df)
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
    # 
  })
  
  
  
  output$contents <- renderTable({
    datasetInput()
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    
  })
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(file_name(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}


# shinyApp()
shinyApp(ui = ui, server = server)