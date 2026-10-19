# =====================================================================
# App Shiny: Observatorio de costos de producción agropecuarios de Colombia (beta)
# Ejecutar con "Run App" en RStudio o shiny::runApp(). Detalles en LEEME.txt
# ESTADO: NO PROBADA EN R (el módulo de evidencia y el tema sí fueron probados por el usuario).
# =====================================================================
library(shiny)
source("tema_agrosavia.R")
source("identidad_diydct.R")   # logo DIyDCT, créditos y doña Violeta
source("almacen_ediciones.R")
source("mod_linea_base.R")
source("mod_edicion.R")
source("mod_evidencia.R")

datos   <- cargar_linea_base("datos_linea_base")
evid    <- if (dir.exists("evidencia_salida")) cargar_evidencia("evidencia_salida") else NULL
almacen <- almacen_desde_entorno()    # GitHub si hay EDICIONES_REPO y GITHUB_PAT; si no, carpeta local

ui <- fluidPage(
  theme = tema_agrosavia() |> bslib::bs_add_rules(CSS_DIYDCT),
  title = "Observatorio de costos de producción agropecuarios de Colombia",
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
  encabezado_observatorio("Observatorio de costos de producción agropecuarios de Colombia",
                          "Versión beta: línea base de Finagro, otras fuentes, ajustes de expertos y evidencia científica"),
  violeta_hero(),
  selectInput("sistema", "Sistema productivo",
              choices = setNames(datos$sistemas$sistema_id, datos$sistemas$sistema)),
  tabsetPanel(id = "pestana",
    tabPanel("Línea base", br(),
      violeta_nota("lupa", "Mire de cerca cada rubro",
        "Cada rubro es una parte de lo que cuesta producir. Compárela con lo que usted gasta en esa labor."),
      mod_linea_base_ui("lb")),
    tabPanel("Ajustes y edición", br(),
      violeta_nota("duda", "¿Su finca no se parece a la guía?",
        "Cuéntenos su valor y por qué es distinto: la región, la época o la forma de producir cambian los costos. Su ajuste queda registrado con su justificación."),
      mod_edicion_ui("ed")),
    tabPanel("Evidencia", br(),
      violeta_nota("saludo", "La ciencia también ha medido los costos",
        "Aquí hay estudios y publicaciones sobre costos y economía de este sistema productivo."),
      if (is.null(evid)) tags$p("Falta la carpeta evidencia_salida/ (salida de evidencia_03_unificar.R).")
      else tagList(uiOutput("aviso_evidencia"), mod_evidencia_ui("ev"))),
    tabPanel("Créditos", br(), pagina_creditos())),
  pie_creditos()
)

server <- function(input, output, session) {
  sistema_id <- reactive(input$sistema)
  ed <- mod_edicion_server("ed", sistema_id = sistema_id, datos = datos, almacen = almacen)
  mod_linea_base_server("lb", sistema_id = sistema_id, datos = datos, ajustes = ed$ajustes)
  if (!is.null(evid)) {
    sistema_evidencia <- reactive(datos$sistemas$sistema_evidencia[match(input$sistema, datos$sistemas$sistema_id)])
    output$aviso_evidencia <- renderUI({
      if (is.na(sistema_evidencia()))
        tags$p(class = "ev-nota", "Este sistema es nuevo en el Observatorio: todavía no tiene evidencia consolidada.")
    })
    mod_evidencia_server("ev", sistema = sistema_evidencia, datos = evid)
  }
}

shinyApp(ui, server)
