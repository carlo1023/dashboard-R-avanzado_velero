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
                  textOutput(outputId = "inicio_textbox")
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
                    plotOutput(outputId = "avance_campana_depto")  
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
                    title = "Georreferenciación",
                    textOutput(outputId = "georreferenciacion_textbox")
                  )), #acaba fluidRow texto
                # fluidRow(
                #   column(
                #     width = 3,
                #     awesomeRadio(inputId = "mapa", label = "Tipo de capa", choices = c("Calor", "Puntos", "Avance"),
                #                 selected = "Puntos"
                #     ))  
                # ),
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
  
