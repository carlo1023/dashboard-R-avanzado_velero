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
  output$inicio_textbox <- renderUI({
    HTML("<p> Este dashboard fue elaborado para monitorización del avance de campaña de vacunación contra Sarampión y Rubéola 2024 en Yuruguay.</p>")
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
  output$justificacion_textbox <- renderUI({
    
    HTML("<p> El sarampión es una enfermedad exantemática febril que puede presentarse en todas las edades, siendo de mayor gravedad en niños menores de 5 años o desnutridos, 
    en los cuales puede causar graves complicaciones respiratorias como neumonía y del sistema nervioso central como convulsiones, meningoencefalitis, ceguera, 
    encefalomielitis postinfecciosa con retraso mental grave y trastornos degenerativos tardíos que no tienen tratamiento, como la panencefalitis esclerosante subaguda. 
    
    La tasa de letalidad del sarampión es del 3 al 6%; la mayor se verifica entre lactantes de 6 a 11 meses, que son los que aún no ha recibido la vacuna antisarampionosa, 
    según corresponde por el Calendario Nacional de Vacunación. Esta tasa de mortalidad puede ascender hasta el 25% en poblaciones con desnutrición, inmunodeficiencias y con acceso deficiente a la atención de salud. 
    
    La rubéola es una enfermedad exantemática benigna en adultos y niños. Sin embargo, cuando una mujer embarazada susceptible se expone al virus durante el primer trimestre se produce infección fetal en el 90 % de los casos, que lleva a la muerte fetal y neonatal o al síndrome de rubéola congénita (SRC). 
    El SRC se caracteriza por graves malformaciones en el recién nacido que producen sordera, ceguera y cardiopatías congénitas entre otras, así como otros trastornos de aparición tardía. No existe tratamiento específico para la rubéola ni el SRC. 
    Las estrategias recomendadas por OPS para mantener la eliminación del sarampión y rubéola establecen que es necesario asegurar la inmunidad de la población evaluando la cantidad de susceptibles y programando una campaña de seguimiento cada 4-5 años o cuando la cantidad de susceptibles es similar a una cohorte de nacidos vivos. </p>")
  })
  
  output$grafica_justificacion <- renderPlot({
    ggplot(campana_nacional,
      aes(x = fecha_vac)) +
      geom_density(aes(y = vacunados, fill = "Dosis"), stat = "identity", alpha= 0.5) +
      scale_fill_manual(values = "steelblue") +
      geom_line(aes(y = cobertura_acumulada * 50, colour = "Cobertura"),  linewidth = 1, linetype= "dashed") +
      labs( 
        title = "Dosis aplicadas acumuladas por dia vs cobertura preliminar",
        x = "",
        y = "Dosis", 
        fill = "", colour = "") +
      scale_y_continuous(
        limits = c(0, 7500),
        sec.axis = sec_axis( trans= ~./50, name = "Cobertura (%)")) +
      theme_minimal() +
      theme(text = element_text(size = 16), legend.position = "bottom")
  })
  colnames(campana_departamento)
  
  output$tabla_justificacion <- renderUI({
   campana_departamento <- campana_departamento %>% 
     mutate(cobertura = round(cobertura, 2),
            cobertura_acumulada=round(cobertura_acumulada, 2)) %>% 
     rename("Fecha de vacunación" = "fecha_vac",
            "Departamento" = "departamento_res_mad",
            "Vacunados" = "vacunados",
            "Cobertura" = "cobertura",
            "Cobertura acumulada" = "cobertura_acumulada")
   
   t <- datatable(campana_departamento)
   t
  })
  ### Avance de campaña --------------------------------------------------------
  # Cuadro informativo para seccion de Avance de campaña
  output$avance_campana_textbox <- renderText({
    "Descripción"})
    
    #Gráfico Interactivo
    
    campana_departamento_reactive <- reactive({
      campana_departamento %>% 
        filter(departamento_res_mad == input$avance_input_depto)
    })
    
    output$avance_campana_depto <- renderPlotly({
      
      grafico_texto <- ggplot(data = campana_departamento_reactive()) + #aca se agrega la funcion reactive interactiva
      geom_bar(aes(x = fecha_vac,
                     y = vacunados, 
                     text = paste0(
                       "Número de vacunados: ", vacunados, "<br>"
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
    
    output$avance_campana_nacional <- renderPlot({
      
      grafico_texto <- ggplot(data = campana_departamento) + #aca se agrega la funcion reactive interactiva
        geom_bar(aes(x = fecha_vac,
                     y = vacunados, 
                     text = paste0(
                       "Departamento ", departamento_res_mad, "<br>"
                     )
        ),
        fill= "lightblue",
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
  ### Georreferenciación -------------------------------------------------------
  # Cuadro informativo para seccion de Georreferenciación
  output$georreferenciacion_textbox <- renderText({
    "Descripción"
  })

  

output$no_vacunados <- renderLeaflet({
  
  datos_map2 <- datos_map2 %>% 
    filter(ADM1_ISON%in%input$selector_dpto)
  
datos_map2$no_vac <-round(abs(datos_map2$no_vac),0)
  breaks <- quantile(datos_map2$no_vac, na.rm = T)

  pal <- colorBin(c("#BF233C","#EACF65", "orange" ,"#8CCE7D", "#24693D"), reverse = T , domain = datos_map2$no_vac, bins = breaks)
  
  
  labels_cor <- sprintf("<b>%s", paste("no_vac",datos_map$ADM2_ISON, datos_map2$lab_novac)) %>%
    lapply(htmltools::HTML)
  
  map <- leaflet(datos_map2) %>% 
    setView(-55.5, -32.5, zoom = 6) %>% 
    addProviderTiles("OpenStreetMap") %>% 
    addEasyButton(
      easyButton(
        icon = "fa-globe",
        title = "Zoom Inicial",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      )
    )
  #map
  
  map <- map %>% 
    addPolygons(
      fillColor = ~pal(no_vac),
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = labels_cor,
      group = "no_vac" )%>% 
    addLegend(
      position = "bottomleft",
      pal = pal,
      values = ~labels_cor,
      na.label = "Sin Dato",
      title = "Número de no vacunados")
  map
  # map <- map %>% 
  #   addCircles(
  #     data = rc1_rnv_nuevo %>% head(100),
  #     lng = ~longitude,
  #     lat = ~latitude,
  #     group = "no_vacunados_pts",
  #     #label = labels_punt,
  #     fillOpacity = 0.4)
  #%>% addLayersControl(overlayGroups = c("Avance", "Puntos", "Calor") ,
  #                                     options = layersControlOptions(collapsed = TRUE ))
})

output$heat_no_vacunados <- renderLeaflet({
  
  rc1_rnv_nuevo <- rc1_rnv_nuevo %>% 
    filter(edad %in% input$edad)
  
    map2 <- leaflet(datos_map2) %>% 
    setView(-55.5, -32.5, zoom = 6) %>% 
    addProviderTiles("OpenStreetMap") %>% 
    addEasyButton(
      easyButton(
        icon = "fa-globe",
        title = "Zoom Inicial",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      )
    )
  #map
  
  map2 <- map2 %>% 
      addHeatmap(
      data = rc1_rnv_nuevo,
      lng = ~longitude,
      lat = ~latitude,
      group = "Calor",
      intensity = 2,
      blur = 50)
  map2
})
})