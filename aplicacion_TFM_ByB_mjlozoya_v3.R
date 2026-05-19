

library(shiny)
library(dplyr)
library(ggplot2)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Validación de Cobertura - Panel NGS"),
  
  sidebarLayout(
    sidebarPanel(
      # PASO 1: Enlaces a Galaxy
      h4("1. Procesamiento en Galaxy"),
      p("Ejecuta el pipeline según el genoma de referencia con tus archivos:"),
      div(
        style = "display: flex; gap: 10px; margin-bottom: 20px;",
        a(href = "https://galaxy-main.usegalaxy.org/u/mariajesus_l/w/workflow-ngs-cobertura-hg19", 
          target = "_blank", class = "btn btn-primary", "Workflow hg19"),
        a(href = "https://galaxy-main.usegalaxy.org/u/mariajesus_l/w/workflow-ngs-cobertura-hg38", 
          target = "_blank", class = "btn btn-primary", "Workflow hg38")
      ),
      
      
      
      hr(),
      
      # PASO 2: Carga de archivos
      h4("2. Carga los resultados procedentes de Galaxy"),
      fileInput("file1", "Seleccionar archivo",
                accept = c(".tabular", ".txt", ".csv")),
      
      
      hr(),
      
      # PASO 3: Parámetros y ejecución
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
      h3("Regiones bajo el umbral"),
      uiOutput("resultado_pantalla")
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
    if(is.null(datos_filtrados())) return(p("Configura los parámetrs y pulsa 'Ejectuar Análisis'."))
    
    if(nrow(datos_filtrados()) == 0) {
      p("Análisis completado: No se han detectado regiones bajo el umbral de cobertura establecido.")
    } else{
      tableOutput("tabla_filtrada")
    }
  })

  output$tabla_filtrada <- renderTable({
    datos_filtrados()
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
