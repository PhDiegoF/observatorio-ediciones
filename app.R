# =====================================================================
# App Shiny: Observatorio de costos de producción agropecuarios de Colombia (beta)
# Ejecutar con "Run App" en RStudio o shiny::runApp(). Detalles en LEEME.txt
# ESTADO: NO PROBADA EN R (el módulo de evidencia y el tema sí fueron probados por el usuario).
# =====================================================================
library(shiny)
source("tema_agrosavia.R")
source("almacen_ediciones.R")
source("mod_linea_base.R")
source("mod_edicion.R")
source("mod_evidencia.R")

datos   <- cargar_linea_base("datos_linea_base")
evid    <- if (dir.exists("evidencia_salida")) cargar_evidencia("evidencia_salida") else NULL
almacen <- almacen_desde_entorno()    # GitHub si hay EDICIONES_REPO y GITHUB_PAT; si no, carpeta local

ui <- fluidPage(
  theme = tema_agrosavia(),
  title = "Observatorio de costos de producción agropecuarios de Colombia",
  encabezado_agrosavia("Observatorio de costos de producción agropecuarios de Colombia",
                       "Versión beta: línea base de Finagro, ajustes de expertos y evidencia científica"),
  selectInput("sistema", "Sistema productivo",
              choices = setNames(datos$sistemas$sistema_id, datos$sistemas$sistema)),
  tabsetPanel(id = "pestana",
    tabPanel("Línea base", br(), mod_linea_base_ui("lb")),
    tabPanel("Ajustes y edición", br(), mod_edicion_ui("ed")),
    tabPanel("Evidencia", br(),
      if (is.null(evid)) tags$p("Falta la carpeta evidencia_salida/ (salida de evidencia_03_unificar.R).")
      else mod_evidencia_ui("ev"))),
  pie_agrosavia()
)

server <- function(input, output, session) {
  sistema_id <- reactive(input$sistema)
  ed <- mod_edicion_server("ed", sistema_id = sistema_id, datos = datos, almacen = almacen)
  mod_linea_base_server("lb", sistema_id = sistema_id, datos = datos, ajustes = ed$ajustes)
  if (!is.null(evid)) {
    sistema_evidencia <- reactive(datos$sistemas$sistema_evidencia[match(input$sistema, datos$sistemas$sistema_id)])
    mod_evidencia_server("ev", sistema = sistema_evidencia, datos = evid)
  }
}

shinyApp(ui, server)
