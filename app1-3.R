#Exercici 1.3

library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(data.table)
library(leaflet)
library(geojsonio)
library(dygraphs)
library(xts)
library(rgeos)

## Acceso a los datos de COVID-19
raw_data <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv')
## Acceso a la capa de países en formato GeoJSON
paises <- geojson_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson", what = "sp")
# Sección ui
# Usamos fluidPage() para definir la interfaz. La interfaz contendrá:
# 1. El título
# 2. Un diseño con panel lateral que contiene:
# 2.1 El panel lateral (sidebarpanel), que contiene una tabla llamada "paises"
# 2.2 El panel principal (mainPanel), que contiene un mapa ("map") generado con Leaflet y un gráfico generado con dygraph
ui <- fluidPage(
  titlePanel("Indicadores Covid-19 en el mundo"),
  sidebarLayout(
    sidebarPanel(div(DT::dataTableOutput("paises"))),
    mainPanel(leafletOutput(outputId = "map", height = "500px", width="100%"),
              dygraphOutput(outputId = "serie"))
  )
)
# Sección server
server <- function(input, output) {
  # Creamos un dataframe agrupando las variables new_cases, new_deaths y new_text por país - iso_code y location -
  paises_agrupados <- reactive({
    raw_data %>%
      group_by(iso_code, location) %>%
      summarise(total_casos = sum(new_cases, na.rm=TRUE), total_muertes = sum(new_deaths,na.rm=TRUE), total_tests = sum(new_tests,na.rm=TRUE))
  })
  # Creamos un dataframe agrupando por país - iso_code, mes y año. Solo recogemos la variable new_cases
  
  paises_agrupados_mes <- reactive({
    raw_data %>%
      mutate(month = format(as.Date(date, "%Y-%m-%d"), "%m"), year = format(as.Date(date, "%Y-%m-%d"), "%Y")) %>%
      group_by(month, year, iso_code, location) %>%
      summarise(total_casos = sum(new_cases), total_muertes = sum(new_deaths), total_tests = sum(new_tests))
  })
  # DataTable con los países agrupados para mostrar en el control DT
  paises_tabla <- reactive({setDT(paises_agrupados())})
  # Dataframe que une - merge - SpatiaDataframe de países con el de datos agrupados en paises_agrupados usando el atributo común.
  paises_total_casos <- reactive({merge(paises, paises_agrupados(), by.x="ISO_A3", by.y = "iso_code" )})
  # paleta para el temático, basado en los valores de casos totales (total_casos)
  paleta <- reactive({colorQuantile("Reds", paises_total_casos()$total_casos, n = 9)})
  #Renderizado de la tabla "paises"
  output$paises= DT::renderDataTable({
    DT::datatable(paises_tabla(), selection = "single")
  })
  #Calculamos los centroides de cada polígono
  cent <- gCentroid(paises, byid = T)
  cent$ISO <- paises@data$ISO_A3
  
  #Renderizado del control de mapa
  output$map <- renderLeaflet({
    p <- leaflet(paises_total_casos()) %>%
      addTiles() %>%
      setView( lat=10, lng=0 , zoom=2) %>%
      addCircles(lng = ~cent$x, lat = ~cent$y, weight = 1, radius = ~sqrt(total_casos) * 40, color = "blue", group = "total_casos",
                 label = ~htmlEscape(paste(location, ":", format(as.numeric(total_casos), big.mark=","), sep = " "))) %>%
      addCircles(lng = ~cent$x, lat = ~cent$y, weight = 1, radius = ~sqrt(total_muertes) * 40, color = "red", group = "total_muertes",
                 label = ~htmlEscape(paste(location, ":", format(as.numeric(total_muertes), big.mark=","), sep = " "))) %>%
      addCircles(lng = ~cent$x, lat = ~cent$y, weight = 1, radius = ~sqrt(total_tests) * 40, color = "green", group = "total_tests",
                 label = ~htmlEscape(paste(location, ":", format(as.numeric(total_tests), big.mark=","), sep = " "))) %>%
      
      #Afegim el Layers Control
      
      addLayersControl(baseGroups = c("total_casos", "total_muertes", "total_tests"),
                       options = layersControlOptions(collapsed = FALSE))
    
  })
  
  # Función que captura la opción en la leyenda del mapa
  selectedOption <- eventReactive({input$map_groups}, {as.character(input$map_groups)})
  
  # Función que captura el código ISO - iso_code - cuando se selecciona un registro en la tabla.
  selectedRow <- eventReactive(input$paises_rows_selected,{
    as.character( paises_tabla()[c(input$paises_rows_selected)]$iso_code[1])
  })
  # Función que se ejecuta cuando se actualiza "selectedRow()"
  # La función leafletProxy permite actualizar el mapa sin tener que renderizarlo de nuevo
  observeEvent(selectedRow(),{
    proxy <- leafletProxy("map")
    iso_selected <- selectedRow()
    poligono <- paises_total_casos()[paises_total_casos()@data$ISO_A3 ==iso_selected,]
    extension <- bbox(poligono)
    proxy %>% addPolygons(data = poligono,
                          fillOpacity = 0,
                          color = "black",
                          opacity = 10,
                          weight = 2,
                          stroke = T,
                          layerId = "poligono")
    proxy %>% fitBounds(extension[1],extension[2], extension[3], extension[4])
    
  })
  # Función que renderiza el gráfico
  output$serie <- renderDygraph({
    dataxts <- NULL
    paises <- unique(paises_agrupados()$iso_code)
    for (l in 1:length(paises)) {
      datos_pais <- paises_agrupados_mes()[paises_agrupados_mes()$iso_code == paises[l], ]
      
      # Creamos un dataset para cada una de las posibles opciones de la leyenda
      dd <- xts(
        datos_pais[, "total_casos"],
        as.Date(paste0(datos_pais$year,"-",datos_pais$month ,"-01"))
      )
      dataxts <- cbind(dataxts, dd)
    }
    
    colnames(dataxts) <- paises
    
    #Dygraph per casos
    dygraph(dataxts) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
    d1$x$css <- 
      ".dygraph-legend > span {display:none;}
.dygraph-legend > span.highlight { display: inline; }"
    
    d1
    
  })
  
  # Función que se ejecuta cuando se actualiza "selectedRow()"
  # Actualiza el gráfico dejando solo la serie del país seleccionado
  observeEvent(selectedOption(), {
    option_selected <- selectedOption()
    paises <- unique(paises_agrupados()$iso_code)
    dataxts <- NULL
    
    output$serie <- renderDygraph({
      for (l in 1:length(paises)) {
        datos_pais <- paises_agrupados_mes()[paises_agrupados_mes()$iso_code == paises[l], ]
        dd <- xts(
          datos_pais[, option_selected],
          as.Date(paste0(datos_pais$year,"-",datos_pais$month ,"-01"))
        )
        dataxts <- cbind(dataxts, dd)}
      colnames(dataxts) <- paises
      dygraph(dataxts, main =  toupper(paste(gsub("_", " ", option_selected), "en el mundo"))) %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
      d1$x$css <- "
.dygraph-legend > span {display:none;}
.dygraph-legend > span.highlight { display: inline; }
"
      d1
    })
  })
  
  
  
  
  # Función que se ejecuta cuando se actualiza "selectedRow()"
  # Actualiza el gráfico dejando solo la serie del país seleccionado
  observeEvent(selectedRow(), {
    iso_selected <- selectedRow()
    option_selected <- selectedOption()
    datos_pais <- paises_agrupados_mes()[paises_agrupados_mes()$iso_code == iso_selected, ]
    pais_completo <- paises_agrupados_mes()$location[paises_agrupados_mes()$iso_code == iso_selected][1]
    dataxts <- NULL
    output$serie <- renderDygraph({
      dd <- xts(
        datos_pais[, option_selected],
        as.Date(paste0(datos_pais$year,"-",datos_pais$month ,"-01"))
      )
      dataxts <- cbind(dataxts, dd)
      dygraph(dataxts, main =  toupper(paste(gsub("_", " ", option_selected), "en", pais_completo))) %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
      d1$x$css <- "
.dygraph-legend > span {display:none;}
.dygraph-legend > span.highlight { display: inline; }
"
      d1
    })
  })
}
# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
