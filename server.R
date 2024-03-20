# server.R ---------------------------------------------------------------------
# Description: Este script crea un servidor, que representa una sesión de R
# que corre código y devuelve resultados (p. ej., una gráfica).
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Inicializar el servidor ------------------------------------------------------
shinyServer(function(input, output) {
  ## Elementos del UI ----------------------------------------------------------
  ### Inicio -------------------------------------------------------------------
  # Cuadro informativo para seccion de Inicio
  output$inicio_textbox <- renderText({
    "Dashboard Campaña Vacunacion Sarampion Rubeola 2024"
  })
  
  output$foto <-renderImage({
    list(src ="equipo_velero.png",
         contentType = "image/png", 
         width = "100%",
         height = "100%")
  },
  deleteFile = FALSE)
  
  output$descripcion_textbox <- renderUI({
    HTML(paste("Kricia Castillo, Costa Rica",
    "Fernanda Velásquez, Guatemala",
    "Sandy Santiago, Rep. Dominicana", 
    "Carlos Hernández, Colombia", 
    "Marcela Contreras, Chile", sep = "<br/>"))
  })
  
 #fin de shinyServer
  
  ### Justificacion ------------------------------------------------------------
  # Cuadro informativo para seccion de Justificacion
  output$justificacion_textbox <- renderText({
    "Descripción"
  })
  
  output$grafica_justificacion <- renderPlot({
    ggplot(
      campana_nacional,
      aes(x = fecha_vac)
    ) +
      labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
      geom_bar(aes(y = vacunados), stat = "identity", position = "stack") +
      geom_line(aes(y = cobertura_acumulada * 50), linewidth = 1) +
      scale_y_continuous(
        limits = c(0, 7500),
        sec.axis = sec_axis( trans= ~./50, name = "Cobertura (%)")
      ) +
      theme_classic() +
      theme(text = element_text(size = 16))
  })
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"
    
    #Gráfico Interactivo
    
    campana_departamento_reactive <- reactive({
      campana_departamento %>% 
        filter(departamento_res_mad == input$avance_input_depto)
    })
    
    output$avance_campana_depto <- renderPlot({
      
      grafico_texto <- ggplot(data = campana_departamento_reactive()) + #aca se agrega la funcion reactive interactiva
      geom_bar(aes(x = fecha_vac,
                     y = vacunados, 
                     text = paste0(
                       "Departamento ", departamento_res_mad, "<br>"
                     )
      ),
      alpha = 0.5, stat = "identity") +
      labs(x = "Fecha de vacunación",
           y = "Número de dosis administradas",
           title = "Número de dosis administradas por departamento",
           caption = "Avances de vacunación"
      ) + 
      theme_bw()
    
    grafico_texto
    
    })
    
    output$avance_campana_nacional <- renderPlot({
      
      grafico_texto <- ggplot(data = campana_departamento) + #aca se agrega la funcion reactive interactiva
        geom_bar(aes(x = fecha_vac,
                     y = vacunados, fill = "lightblue", 
                     text = paste0(
                       "Departamento ", departamento_res_mad, "<br>"
                     )
        ),
        alpha = 0.5, stat = "identity") +
        labs(x = "Fecha de vacunación",
             y = "Número de dosis administradas",
             title = "Número de dosis administradas por departamento",
             caption = "Avances de vacunación"
        ) + 
        theme_bw() + 
        theme(legend.position = "none")
      
      grafico_texto
      
    })
    
    ggplotly(grafico_texto, tooltip = "text") %>% 
      config(
        locale = "es",
        displaylogo = FALSE,
        scrollZoom = TRUE,
        modeBarButtonsToAdd = c(
          "drawline", # dibujar líneas rectas
          "drawopenpath", # dibujar líneas libres
          "drawcircle", # dibujar círculos
          "drawrect", #dibujar rectángulos
          "eraseshape"
        )
      )
    
    
  })
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})