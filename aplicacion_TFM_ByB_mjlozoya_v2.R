

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
      
      actionButton("run_analysis", "Ejecutar Análisis", 
                   class = "btn-success", style = "width: 100%; font-weight: bold;")
    ),
    
    mainPanel(
      h3("Regiones bajo el umbral"),
      tableOutput("tabla_filtrada")
    )
  )
)

server <- function(input, output) {
  

  datos_crudos <- reactive({
    req(input$file1)
    read.table(input$file1$datapath, header = FALSE, sep = "\t",
               col.names = c("Chr", "Start", "End", "Gen", "Cobertura", "GC"))
  })
  

  datos_filtrados <- eventReactive(input$run_analysis, {
    req(datos_crudos()) 
    
    datos_crudos() %>% 
      filter(Cobertura < input$umbral) %>%
      mutate(Posible_Causa = case_when(
        GC < 0.40  ~ "Bajo GC (<40%)",
        GC > 0.55  ~ "Alto GC (>55%)",
        TRUE       ~ "Fallo aleatorio"
      )) %>%
      arrange(Cobertura)
  })
  

  output$tabla_filtrada <- renderTable({
    datos_filtrados()
  })
}


shinyApp(ui = ui, server = server)
