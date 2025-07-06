# Dashboard de Homicidios Cali - Versión con Mapa y Datos Reales
# Lectura directa de Excel + Mapa interactivo

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(leaflet)
library(sf)
library(DT)
library(RColorBrewer)
library(htmltools)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

# Función para leer todos los sheets de Excel
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Función para crear grupos de edad
gr_edad <- function(data) {
  data$g_edad <- ifelse(data$EDAD < 10, "0 a 9 años",
                        ifelse(data$EDAD > 9 & data$EDAD < 20, "10 a 19 años",
                               ifelse(data$EDAD > 19 & data$EDAD < 30, "20 a 29 años",
                                      ifelse(data$EDAD > 29 & data$EDAD < 40, "30 a 39 años",
                                             ifelse(data$EDAD > 39 & data$EDAD < 50, "40 a 49 años",
                                                    ifelse(data$EDAD > 49 & data$EDAD < 60, "50 a 59 años",
                                                           ifelse(data$EDAD > 59 & data$EDAD < 70, "60 a 69 años",
                                                                  ifelse(data$EDAD > 69 & data$EDAD < 80, "70 a 79 años",
                                                                         ifelse(data$EDAD > 79 & data$EDAD < 90, "80 a 89 años",
                                                                                ifelse(data$EDAD > 89 & data$EDAD < 100, "90 a 99 años",
                                                                                       ifelse(data$EDAD > 99, "100+ años", "Sin grupo")))))))))))
  return(data)
}

# ============================================================================
# CARGA DE DATOS REALES
# ============================================================================

# Cargar datos desde Excel
tryCatch({
  # Intentar cargar el archivo Excel
  homicidios_raw <- read_excel_allsheets("DATOS_1993-2019.xlsx")
  homicidios <- homicidios_raw[[2]]  # Segundo sheet como en tu código original
  
  # Procesamiento según tu código original
  homicidios$EDAD[is.na(homicidios$EDAD)] <- 200
  homicidios <- gr_edad(homicidios)
  homicidios$com[homicidios$com > 22] <- 23
  
  cat("✅ Datos cargados exitosamente desde Excel\n")
  cat("📊 Total de registros:", nrow(homicidios), "\n")
  cat("📅 Años disponibles:", min(homicidios$fechao, na.rm = TRUE), "-", max(homicidios$fechao, na.rm = TRUE), "\n")
  
}, error = function(e) {
  cat("❌ Error al cargar Excel. Usando datos de muestra.\n")
  cat("Error:", e$message, "\n")
  
  # Datos de muestra como respaldo
  set.seed(123)
  n_records <- 1000
  
  homicidios <- data.frame(
    fechao = sample(1993:2019, n_records, replace = TRUE),
    com = sample(1:22, n_records, replace = TRUE),
    barrio = sample(c("LAS CEIBAS", "ALFONSO LOPEZ", "SAN JUAN BOSCO", "LOS COMUNEROS"), 
                    n_records, replace = TRUE),
    modalidad = sample(c("ARMA DE FUEGO", "CORTOPUNZANTE", "OTRAS ARMAS"), 
                       n_records, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    SEXO = sample(c("M", "F"), n_records, replace = TRUE, prob = c(0.8, 0.2)),
    EDAD = sample(15:70, n_records, replace = TRUE),
    tipo_violencia = sample(c("CONVIVENCIA", "DELINCUENCIA", "OTRO"), 
                            n_records, replace = TRUE, prob = c(0.3, 0.6, 0.1)),
    categoria_movil = sample(c("RIÑAS", "TRÁFICO", "VENGANZA", "PATRIMONIO"), 
                             n_records, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  homicidios <- gr_edad(homicidios)
})

# ============================================================================
# COORDENADAS DE COMUNAS DE CALI (aproximadas)
# ============================================================================

# Coordenadas aproximadas del centro de cada comuna
coordenadas_comunas <- data.frame(
  com = 1:22,
  lat = c(3.4516, 3.4372, 3.4209, 3.4118, 3.4028, 3.3937, 3.3847, 3.3756,
          3.3666, 3.3575, 3.3485, 3.3394, 3.3304, 3.3213, 3.3123, 3.3032,
          3.2942, 3.2851, 3.2761, 3.2670, 3.2580, 3.2489),
  lon = c(-76.5319, -76.5228, -76.5137, -76.5046, -76.4955, -76.4864, -76.4773, -76.4682,
          -76.4591, -76.4500, -76.4409, -76.4318, -76.4227, -76.4136, -76.4045, -76.3954,
          -76.3863, -76.3772, -76.3681, -76.3590, -76.3499, -76.3408),
  nombre_comuna = paste("Comuna", 1:22)
)

# ============================================================================
# UI MEJORADA CON MAPA
# ============================================================================

ui <- fluidPage(
  titlePanel("🗺️ Dashboard Interactivo de Homicidios - Cali (1993-2019)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("🔍 Filtros Principales"),
      
      selectInput("filter_año", "📅 Año:",
                  choices = c("Todos", sort(unique(homicidios$fechao))),
                  selected = "Todos"),
      
      selectInput("filter_sexo", "👥 Sexo:",
                  choices = c("Todos", "M", "F"),
                  selected = "Todos"),
      
      selectInput("filter_modalidad", "⚔️ Modalidad:",
                  choices = c("Todos", unique(homicidios$modalidad)),
                  selected = "Todos"),
      
      selectInput("filter_violencia", "🎯 Tipo de Violencia:",
                  choices = c("Todos", unique(homicidios$tipo_violencia)),
                  selected = "Todos"),
      
      selectInput("filter_comuna", "🏘️ Comuna:",
                  choices = c("Todas", sort(unique(homicidios$com))),
                  selected = "Todas"),
      
      hr(),
      h4("📈 Estadísticas Filtradas"),
      verbatimTextOutput("stats"),
      
      hr(),
      h4("🎨 Opciones del Mapa"),
      checkboxInput("mostrar_etiquetas", "Mostrar etiquetas en mapa", value = TRUE),
      sliderInput("tamaño_puntos", "Tamaño de puntos:", 
                  min = 1, max = 20, value = 10, step = 1)
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        # TAB 1: Mapa Principal
        tabPanel("🗺️ Mapa Interactivo",
                 fluidRow(
                   column(12,
                          h4("Distribución Geográfica de Homicidios por Comuna"),
                          leafletOutput("mapa_principal", height = "600px")
                   )
                 ),
                 fluidRow(
                   column(6, plotlyOutput("plot_comunas_mapa")),
                   column(6, plotlyOutput("plot_modalidad_mapa"))
                 )
        ),
        
        # TAB 2: Análisis Temporal
        tabPanel("📈 Análisis Temporal",
                 fluidRow(
                   column(6, plotlyOutput("plot_temporal")),
                   column(6, plotlyOutput("plot_sexo"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("plot_edad")),
                   column(6, plotlyOutput("plot_violencia_tiempo"))
                 )
        ),
        
        # TAB 3: Análisis por Modalidad
        tabPanel("⚔️ Modalidades",
                 fluidRow(
                   column(8, plotlyOutput("plot_modalidad_detalle")),
                   column(4, plotlyOutput("plot_modalidad_pie"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_modalidad_heatmap"))
                 )
        ),
        
        # TAB 4: Datos y Descarga
        tabPanel("📋 Datos",
                 fluidRow(
                   column(12,
                          h4("📥 Descargar Datos Filtrados"),
                          downloadButton("download", "Descargar CSV", class = "btn-primary btn-lg"),
                          br(), br(),
                          DT::dataTableOutput("tabla")
                   )
                 )
        )
      )
    )
  )
)

# ============================================================================
# SERVER MEJORADO
# ============================================================================

server <- function(input, output, session) {
  
  # Datos reactivos filtrados
  datos_filtrados <- reactive({
    datos <- homicidios
    
    if (input$filter_año != "Todos") {
      datos <- datos[datos$fechao == as.numeric(input$filter_año), ]
    }
    if (input$filter_sexo != "Todos") {
      datos <- datos[datos$SEXO == input$filter_sexo, ]
    }
    if (input$filter_modalidad != "Todos") {
      datos <- datos[datos$modalidad == input$filter_modalidad, ]
    }
    if (input$filter_violencia != "Todos") {
      datos <- datos[datos$tipo_violencia == input$filter_violencia, ]
    }
    if (input$filter_comuna != "Todas") {
      datos <- datos[datos$com == as.numeric(input$filter_comuna), ]
    }
    
    return(datos)
  })
  
  # Datos agregados por comuna para el mapa
  datos_mapa <- reactive({
    datos <- datos_filtrados()
    
    # Agregar por comuna
    datos_agregados <- datos %>%
      group_by(com) %>%
      summarise(
        total_casos = n(),
        casos_arma_fuego = sum(modalidad == "ARMA DE FUEGO", na.rm = TRUE),
        casos_cortopunzante = sum(modalidad == "CORTOPUNZANTE", na.rm = TRUE),
        casos_otras_armas = sum(modalidad == "OTRAS ARMAS", na.rm = TRUE),
        hombres = sum(SEXO == "M", na.rm = TRUE),
        mujeres = sum(SEXO == "F", na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Unir con coordenadas
    datos_con_coords <- merge(datos_agregados, coordenadas_comunas, by = "com", all.y = TRUE)
    datos_con_coords[is.na(datos_con_coords)] <- 0
    
    return(datos_con_coords)
  })
  
  # Estadísticas básicas
  output$stats <- renderText({
    datos <- datos_filtrados()
    if(nrow(datos) > 0) {
      modalidad_principal <- names(sort(table(datos$modalidad), decreasing = TRUE))[1]
      comuna_principal <- names(sort(table(datos$com), decreasing = TRUE))[1]
      
      paste0(
        "📊 Total casos: ", nrow(datos), "\n",
        "📅 Años analizados: ", length(unique(datos$fechao)), "\n",
        "🏘️ Comunas afectadas: ", length(unique(datos$com)), "\n",
        "⚔️ Modalidad principal: ", modalidad_principal, "\n",
        "🎯 Comuna más afectada: ", comuna_principal, "\n",
        "👨 Hombres: ", sum(datos$SEXO == "M", na.rm = TRUE), "\n",
        "👩 Mujeres: ", sum(datos$SEXO == "F", na.rm = TRUE)
      )
    } else {
      "Sin datos para los filtros seleccionados"
    }
  })
  
  # ================================================================
  # MAPA PRINCIPAL MEJORADO Y CORREGIDO
  # ================================================================
  
  output$mapa_principal <- renderLeaflet({
    datos_map <- datos_mapa()
    
    # Crear paleta de colores basada en número de casos
    if(max(datos_map$total_casos) > 0) {
      # Paleta más estética con gradiente suave
      pal <- colorNumeric(
        palette = c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"),
        domain = datos_map$total_casos,
        na.color = "#E8E8E8"
      )
      
      # Crear etiquetas para el popup más elegantes
      labels <- sprintf(
        "<div style='font-family: Arial, sans-serif; line-height: 1.4;'>
        <h4 style='margin: 0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>
        🏘️ %s</h4>
        <table style='width: 100%%; font-size: 13px;'>
        <tr><td style='padding: 2px 8px 2px 0;'><strong>📊 Total casos:</strong></td><td style='color: #e74c3c; font-weight: bold;'>%d</td></tr>
        <tr><td style='padding: 2px 8px 2px 0;'>🔫 Arma de fuego:</td><td>%d</td></tr>
        <tr><td style='padding: 2px 8px 2px 0;'>🔪 Cortopunzante:</td><td>%d</td></tr>
        <tr><td style='padding: 2px 8px 2px 0;'>⚔️ Otras armas:</td><td>%d</td></tr>
        <tr><td style='padding: 2px 8px 2px 0;'>👨 Hombres:</td><td style='color: #3498db;'>%d</td></tr>
        <tr><td style='padding: 2px 8px 2px 0;'>👩 Mujeres:</td><td style='color: #e91e63;'>%d</td></tr>
        </table>
        </div>",
        datos_map$nombre_comuna,
        datos_map$total_casos,
        datos_map$casos_arma_fuego,
        datos_map$casos_cortopunzante,
        datos_map$casos_otras_armas,
        datos_map$hombres,
        datos_map$mujeres
      ) %>% lapply(htmltools::HTML)
      
      # Crear mapa con estilo mejorado y SIN highlightOptions
      mapa <- leaflet(datos_map) %>%
        # Usar tiles más estéticos
        addProviderTiles(providers$CartoDB.Positron, 
                         options = providerTileOptions(opacity = 0.8)) %>%
        setView(lng = -76.5225, lat = 3.4516, zoom = 11) %>%
        addCircleMarkers(
          lng = ~lon, lat = ~lat,
          radius = ~pmax(8, pmin(25, sqrt(total_casos) * input$tamaño_puntos / 3)),
          color = "white",
          fillColor = ~pal(total_casos),
          fillOpacity = 0.8,
          weight = 2,
          opacity = 1,
          popup = labels,
          popupOptions = popupOptions(maxWidth = 300)
          # Removido highlightOptions que causaba el error
        ) %>%
        addLegend(
          pal = pal,
          values = ~total_casos,
          opacity = 0.8,
          title = HTML("<strong>Número de casos</strong>"),
          position = "bottomright",
          labFormat = labelFormat(
            suffix = " casos",
            between = " - ",
            transform = function(x) round(x)
          )
        )
      
      # Añadir etiquetas mejoradas si está activado
      if(input$mostrar_etiquetas) {
        mapa <- mapa %>%
          addLabelOnlyMarkers(
            lng = ~lon, lat = ~lat,
            label = ~paste("Comuna", com),
            labelOptions = labelOptions(
              noHide = TRUE,
              textOnly = TRUE,
              direction = "center",
              style = list(
                "color" = "#2c3e50",
                "font-weight" = "bold", 
                "font-size" = "11px",
                "border" = "2px solid white",
                "border-radius" = "8px",
                "background" = "rgba(255,255,255,0.9)",
                "padding" = "3px 6px",
                "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"
              )
            )
          )
      }
      
      return(mapa)
    } else {
      # Mapa vacío si no hay datos
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -76.5225, lat = 3.4516, zoom = 11)
    }
  })
  
  # ================================================================
  # GRÁFICOS COMPLEMENTARIOS
  # ================================================================
  
  # Gráfico temporal
  output$plot_temporal <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      temporal <- datos %>%
        group_by(fechao) %>%
        summarise(Total = n(), .groups = 'drop')
      
      p <- ggplot(temporal, aes(x = fechao, y = Total)) +
        geom_line(color = "steelblue", size = 1.2) +
        geom_point(color = "steelblue", size = 2) +
        labs(title = "Evolución Temporal", x = "Año", y = "Casos") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Gráfico por sexo
  output$plot_sexo <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      sexo_data <- datos %>%
        group_by(SEXO) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        mutate(Porcentaje = round(Total/sum(Total)*100, 1))
      
      p <- ggplot(sexo_data, aes(x = SEXO, y = Total, fill = SEXO, 
                                 text = paste("Sexo:", SEXO, "<br>Casos:", Total, "<br>Porcentaje:", Porcentaje, "%"))) +
        geom_col() +
        scale_fill_manual(values = c("M" = "lightblue", "F" = "pink")) +
        labs(title = "Distribución por Sexo", x = "Sexo", y = "Casos") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    } else {
      plotly_empty()
    }
  })
  
  # Gráfico por edad
  output$plot_edad <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      edad_data <- datos %>%
        group_by(g_edad) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))
      
      p <- ggplot(edad_data, aes(x = reorder(g_edad, Total), y = Total)) +
        geom_col(fill = "coral") +
        coord_flip() +
        labs(title = "Casos por Grupo de Edad", x = "Grupo de Edad", y = "Casos") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Gráfico comunas para mapa
  output$plot_comunas_mapa <- renderPlotly({
    datos_map <- datos_mapa()
    
    # Top 10 comunas
    top_comunas <- datos_map %>%
      arrange(desc(total_casos)) %>%
      head(10)
    
    if(nrow(top_comunas) > 0) {
      p <- ggplot(top_comunas, aes(x = reorder(factor(com), total_casos), y = total_casos)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Top 10 Comunas", x = "Comuna", y = "Casos") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Gráfico modalidades para mapa
  output$plot_modalidad_mapa <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      modalidad_data <- datos %>%
        group_by(modalidad) %>%
        summarise(Total = n(), .groups = 'drop')
      
      p <- ggplot(modalidad_data, aes(x = modalidad, y = Total, fill = modalidad)) +
        geom_col() +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        labs(title = "Casos por Modalidad", x = "Modalidad", y = "Casos") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Gráfico violencia tiempo
  output$plot_violencia_tiempo <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      violencia_tiempo <- datos %>%
        group_by(fechao, tipo_violencia) %>%
        summarise(Total = n(), .groups = 'drop')
      
      p <- ggplot(violencia_tiempo, aes(x = fechao, y = Total, fill = tipo_violencia)) +
        geom_area(position = "stack") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        labs(title = "Tipo de Violencia en el Tiempo", x = "Año", y = "Casos", fill = "Tipo") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Modalidad detalle - CORREGIDO
  output$plot_modalidad_detalle <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      # Verificar que existe la columna modalidad
      if("modalidad" %in% names(datos)) {
        modalidad_tiempo <- datos %>%
          group_by(fechao, modalidad) %>%
          summarise(Total = n(), .groups = 'drop') %>%
          filter(!is.na(modalidad), !is.na(fechao))
        
        if(nrow(modalidad_tiempo) > 0) {
          p <- ggplot(modalidad_tiempo, aes(x = fechao, y = Total, color = modalidad)) +
            geom_line(size = 1.2) +
            geom_point(size = 2) +
            scale_color_brewer(type = "qual", palette = "Set1") +
            labs(title = "Evolución de Modalidades", x = "Año", y = "Casos", color = "Modalidad") +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              legend.position = "bottom"
            )
          
          ggplotly(p)
        } else {
          plotly_empty() %>% 
            layout(title = "No hay datos de modalidades en el rango seleccionado")
        }
      } else {
        plotly_empty() %>% 
          layout(title = "Columna 'modalidad' no encontrada en los datos")
      }
    } else {
      plotly_empty() %>% 
        layout(title = "No hay datos para mostrar")
    }
  })
  
  # Modalidad pie - CORREGIDO
  output$plot_modalidad_pie <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      if("modalidad" %in% names(datos)) {
        modalidad_data <- datos %>%
          filter(!is.na(modalidad)) %>%
          group_by(modalidad) %>%
          summarise(Total = n(), .groups = 'drop') %>%
          mutate(Porcentaje = round(Total/sum(Total)*100, 1))
        
        if(nrow(modalidad_data) > 0) {
          # Crear gráfico de pie con plotly directamente para mejor control
          plot_ly(modalidad_data, 
                  labels = ~modalidad, 
                  values = ~Total, 
                  type = 'pie',
                  textinfo = 'label+percent',
                  textposition = 'inside',
                  marker = list(colors = RColorBrewer::brewer.pal(n = nrow(modalidad_data), "Set1")),
                  hovertemplate = "<b>%{label}</b><br>Casos: %{value}<br>Porcentaje: %{percent}<extra></extra>") %>%
            layout(
              title = list(text = "Distribución de Modalidades", font = list(size = 14)),
              showlegend = TRUE,
              legend = list(orientation = "v", x = 1.02, y = 0.5)
            )
        } else {
          plotly_empty() %>% 
            layout(title = "No hay datos de modalidades válidos")
        }
      } else {
        plotly_empty() %>% 
          layout(title = "Columna 'modalidad' no encontrada")
      }
    } else {
      plotly_empty() %>% 
        layout(title = "No hay datos para mostrar")
    }
  })
  
  # Heatmap modalidades - CORREGIDO
  output$plot_modalidad_heatmap <- renderPlotly({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      if("modalidad" %in% names(datos) && "com" %in% names(datos)) {
        heatmap_data <- datos %>%
          filter(!is.na(modalidad), !is.na(com)) %>%
          group_by(com, modalidad) %>%
          summarise(Total = n(), .groups = 'drop')
        
        if(nrow(heatmap_data) > 0) {
          p <- ggplot(heatmap_data, aes(x = factor(com), y = modalidad, fill = Total)) +
            geom_tile(color = "white", size = 0.5) +
            scale_fill_gradient2(
              low = "#f7fbff", 
              mid = "#6baed6", 
              high = "#08306b",
              midpoint = max(heatmap_data$Total) / 2,
              name = "Casos"
            ) +
            labs(
              title = "Heatmap: Modalidades por Comuna", 
              x = "Comuna", 
              y = "Modalidad"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 14, face = "bold"),
              panel.grid = element_blank()
            )
          
          ggplotly(p)
        } else {
          plotly_empty() %>% 
            layout(title = "No hay datos válidos para el heatmap")
        }
      } else {
        plotly_empty() %>% 
          layout(title = "Columnas necesarias no encontradas (modalidad, com)")
      }
    } else {
      plotly_empty() %>% 
        layout(title = "No hay datos para mostrar")
    }
  })
  
  # Tabla de datos
  output$tabla <- DT::renderDataTable({
    datos <- datos_filtrados()
    
    if(nrow(datos) > 0) {
      # Seleccionar columnas principales
      columnas_mostrar <- intersect(names(datos), 
                                    c("fechao", "com", "barrio", "SEXO", "EDAD", "g_edad", 
                                      "modalidad", "tipo_violencia", "categoria_movil"))
      
      DT::datatable(
        datos[, columnas_mostrar],
        options = list(
          pageLength = 15, 
          scrollX = TRUE,
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros"
          )
        ),
        filter = 'top'
      )
    } else {
      DT::datatable(data.frame(Mensaje = "No hay datos para mostrar"))
    }
  })
  
  # Descarga
  output$download <- downloadHandler(
    filename = function() {
      paste("homicidios_cali_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================

shinyApp(ui = ui, server = server)