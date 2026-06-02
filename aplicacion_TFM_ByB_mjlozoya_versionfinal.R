
library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Analizador de Cobertura Genómica"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Procesamiento en Galaxy"),
      p("Ejecuta el pipeline con tus archivos según el genoma de referencia usado:"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 20px;",
        a(href = "https://galaxy-main.usegalaxy.org/u/mariajesus_l/w/wf-ngs-coverage-hg19", 
          target = "_blank", class = "btn btn-primary", "Workflow hg19"),
        a(href = "https://galaxy-main.usegalaxy.org/u/mariajesus_l/w/wf-ngs-coverage-hg38", 
          target = "_blank", class = "btn btn-primary", "Workflow hg38")
      ),
      
      
      
      hr(),
      
      h4("2. Carga los resultados procedentes de Galaxy"),
      fileInput("file1", "Seleccionar archivo",
                accept = c(".tabular", ".txt", ".csv")),
      
      
      hr(),
      
      h4("3. Configuración"),
      numericInput("umbral", "Umbral de Cobertura mínima:", 
                   value = 20, min = 0, step = 1),
      
      selectInput("gen_seleccionado", "Filtrar por Gen:",
                  choices = "Todos",
                  selected = "Todos"),
      
      actionButton("run_analysis", "Ejecutar Análisis", 
                   class = "btn-success", style = "width: 100%; font-weight: bold; margin-bottom: 15px"),
      
      downloadButton("descargar_csv", "Descargar Alertas (.csv)",
                      class = "btn-info", style = "width: 100%; font-weight: bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Lista de Alertas",
                 h3("Regiones bajo el umbral"),
                 uiOutput("resultado_pantalla")
        ),
        
        tabPanel("Análisis Gráfico",
                 h3("Perfil Global de Cobertura vs. Contenido en GC"),
                 plotOutput("grafico_gc")
        ),
        
        tabPanel("Información del Pipeline",
                 h3("Especificaciones Técnicas y Rendimiento"),
                 br(),
                 p("- Flujo de trabajo en Galaxy: filtro de calidad de mapeo (MapQ) mínimo de 20"),
                 p("- Estadística: Sensibilidad - 100% y Especificidad - 99'01% *"),
                 p("(*Estadística evaluada en un conjunto de datos de 11 archivos)")
        )
      )  
    )             
  )
)

server <- function(input, output, session) {
  

  datos_crudos <- reactive({
    req(input$file1)
    read.table(input$file1$datapath, header = FALSE, sep = "\t",
               col.names = c("Chr", "Start", "End", "Gen", "Cobertura", "GC"))
  })
  
  observeEvent(datos_crudos(), {
    genes_archivo <- unique(datos_crudos()$Gen)
    
    updateSelectInput(session, "gen_seleccionado",
                      choices = c("Todos", sort(genes_archivo)),
                      selected = "Todos")
  })

  datos_filtrados <- eventReactive(input$run_analysis, {
    req(datos_crudos())
    
  df <- datos_crudos() %>% 
      filter(Cobertura < input$umbral) %>%
      mutate(Posible_Causa = case_when(
        GC < 0.40  ~ "Bajo GC (<40%)",
        GC > 0.55  ~ "Alto GC (>55%)",
        TRUE       ~ "Fallo aleatorio"
      )) 
  
  if(input$gen_seleccionado != "Todos"){
    df <- df %>% filter(Gen == input$gen_seleccionado)
  }
  
  df %>% arrange(Cobertura)
 
  })
  
  output$resultado_pantalla <- renderUI({
    if(is.null(datos_filtrados())) return(p("Configura los parámetros y pulsa 'Ejectuar Análisis'."))
    
    if(nrow(datos_filtrados()) == 0) {
      p("Análisis completado: No se han detectado regiones bajo el umbral de cobertura establecido.")
    } else{
      tableOutput("tabla_filtrada")
    }
  })

  output$tabla_filtrada <- renderTable({
    datos_filtrados()
  })
  
  output$grafico_gc <- renderPlot({
    req(input$run_analysis, datos_crudos())
    
    df_grafico <- datos_crudos() %>%
      mutate(Estado = if_else(Cobertura < input$umbral, "Alerta (< Umbral", "Correcto"))
    
    if(input$gen_seleccionado != "Todos"){
      df_grafico <- df_grafico %>% filter(Gen == input$gen_seleccionado)
    }
    
    ggplot(df_grafico, aes(x = GC, y = Cobertura)) +
      geom_point(alpha = 0.6, size = 2, color = "#999999") + 
      geom_hline(yintercept = input$umbral, linetype = "dashed", color = "black", linewidth = 0.8) +
      geom_vline(xintercept = c(0.40, 0.55), linetype = "dotted", color = "maroon", linewidth = 1.0) +
      labs(x = "Contenido en GC", 
           y = "Profundidad de Cobertura (X)") +
      theme_minimal() +
      theme(axis.title = element_text(size = 14, face = "plain"))
  })
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste("alerta_cobertura_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(datos_filtrados())
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
}


shinyApp(ui = ui, server = server)
