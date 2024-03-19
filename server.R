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
    susceptibles_municipio %>% 
      filter(ano <= 2023) %>% 
      group_by(ano) %>% 
      summarise(susceptibles = sum(susceptibles, na.rm = T),cobertura = first(cobertura)) %>% 
      ggplot(aes(x= ano, y= cobertura))+
      geom_bar(stat = "identity") +
      geom_line(aes(y = susceptibles))
  })
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"
  })
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })
})