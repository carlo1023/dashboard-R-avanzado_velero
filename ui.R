# ui.R -------------------------------------------------------------------------
# Description: Este script crea la interfaz de usuario de la aplicación de
# Shiny.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar la UI ------------------------------------------------------------
fluidPage(
  fluidRow(
    box(width = 12,
        background = "red",
        div(
          HTML(paste0(
            '<font color="white"><strong>',
            "Este tablero es producto del curso avanzado de R organizado por OPS en marzo de 2024.
    Los datos presentados en este tablero son completamente ficticios y han sido creados 
    únicamente con fines didácticos. Cualquier similitud con datos reales, personas o 
    eventos es pura coincidencia y no debe interpretarse como una representación exacta 
    de la realidad. Este contenido está diseñado para ilustrar conceptos y promover el 
    aprendizaje a través de ejemplos construidos para este fin.",
    '</strong></font>'
          )
          ))
    )
  ),
  titlePanel(span("Dashboard de Campaña de Vacunación de SR 2024",img(src="Velero.jpg",height=50))),
  
  ## CSS -------------------------------------------------------------------------
  includeCSS("style.scss"),
  ## Inicializar dashboard -------------------------------------------------------
  dashboardPage(
    ## Header dashboard ------------------------------------------------------------
    dashboardHeader(title = paste0(Sys.Date())),
    ## Sidebar dashboard -----------------------------------------------------------
    dashboardSidebar(
      sidebarMenu(
        # Modulo 1: Inicio
        menuItem(text = "Inicio",
                 tabName = "inicio",
                 icon = icon("home"),
                 selected = TRUE
        ),
        # Modulo 2: Justificacion
        menuItem(text = "Justificación",
                 tabName = "justificacion",
                 icon = icon("th-large")
        ),
        # Modulo 3: Avance Campaña
        menuItem(text = "Avance Campaña",
                 tabName = "avance_campana",
                 icon = icon("th-large")
        ),
        # Modulo 4: Georreferenciacion
        menuItem(text = "Georreferenciacion",
                 tabName = "georreferenciacion",
                 icon = icon("th-large")
        )
      )
    ),
    ## Cuerpo dashboard ------------------------------------------------------------
    dashboardBody(
      tabItems(
        ### Inicio -------------------------------------------------------------
        tabItem(tabName = "inicio",
                fluidRow(box(
                  width = 12,
                  title = "Inicio",
                  uiOutput(outputId = "inicio_textbox")
                )),
                fluidRow(
                  box(width = 6,
                      height = "100%",
                      imageOutput(outputId = "foto")
                  ),
                  box(
                    width = 6,
                    title = "Descripción del equipo",
                    uiOutput(outputId = "descripcion_textbox"
                  )
                ))),
        ### Justificacion ------------------------------------------------------
        tabItem(tabName = "justificacion",
                fluidRow(
                  box(
                    width = 12,
                    title = "Justificacion",
                    uiOutput(outputId = "justificacion_textbox"))),
                fluidRow(
                box(
                  width = 6,
                  height = "100%",
                  plotOutput(
                    outputId = "grafica_justificacion"
                  )),
                box(
                  width = 6,
                  uiOutput(
                    outputId = "tabla_justificacion"
                  )
                ))),
        ### Avance de campaña --------------------------------------------------
        ###---------------------------------------------------------------------
        ###---------------------------------------------------------------------
        tabItem(tabName = "avance_campana",
                fluidRow(
                  box(
                    width = 12,
                    title = "Avance de Campaña",
                    textOutput(outputId = "avance_campana_textbox")  
                  )),
                fluidRow(
                  box(
                    width = 12,
                    selectInput("avance_input_depto",
                     choices = unique(campana_departamento$departamento_res_mad),
                     label = "Seleccione departamento",selected = "all"
                    ))  
                  ),
                fluidRow(
                  box(
                    width = 6,
                    title = "Avance de cobertura Departamental",
                    plotlyOutput(outputId = "avance_campana_depto")  
                  ),
                  box(
                    width = 6,
                    title = "Avance de Campaña Nacional",
                    plotOutput(outputId = "avance_campana_nacional")
                   
                  ))),
        ### Georreferenciación -------------------------------------------------
        tabItem(tabName = "georreferenciacion",
                fluidRow(
                  box(
                    width = 12,
                    title = "Un mapa de calor (heat map, en inglés) es una técnica de visualización de datos que mide la magnitud de un fenómeno en colores en dos dimensiones. La variación del color puede ser por tono o intensidad, haciendo obvia la lectura del fenómeno sobre el espacio que se trata. Existen dos categorías fundamentales de mapas de calor: el mapa de calor de análisis de grupos y el mapa de calor espacial. En un mapa de calor de conglomerados, las magnitudes se disponen en una matriz de tamaño de celda fijo, cuyas filas y columnas son fenómenos y categorías discretos, además la clasificación de filas y columnas es intencional y algo arbitraria, con el objetivo de sugerir los conglomerados o representarlos como descubrimientos a través del análisis estadístico. El tamaño de la celda es arbitrario pero lo suficientemente grande para ser claramente visible. Por el contrario, la posición de una magnitud en un mapa de calor espacial está forzada por la ubicación de la magnitud en ese espacio, y no existe la noción de celdas; se considera que el fenómeno varía continuamente",
                    textOutput(outputId = "georreferenciacion_textbox")
                  )), #acaba fluidRow texto
                fluidRow(
                  column(
                    width = 6,
                    pickerInput(
                      inputId = "selector_dpto",
                      label = "Seleccion departamento", 
                      choices = unique(datos_map2$ADM1_ISON),
                      options = pickerOptions(container = "body"), 
                      width = "100%"
                    )
                  ),
                  column(width = 6,
                         checkboxGroupButtons(
                           inputId = "edad",
                           label = "Seleccione edad simple",
                           choices = c("1","2","3","4","5"),
                           status = "success", selected = "all"
                         ))
                ),
                fluidRow(
                  box(
                    width = 6,
                    title = "No_vacunados",
                    leafletOutput(outputId = "no_vacunados")  
                  ),
                  box(
                    width = 6,
                    title = "Heat_no_vacunados",
                    leafletOutput(outputId = "heat_no_vacunados")
                     
                 )
                ))
      )
    )))
  
