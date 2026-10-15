# =====================================================================
# Módulo de Shiny: evidencia científica y técnica por sistema productivo
# Observatorio de costos de producción agropecuarios de Colombia (piloto)
#
# ESTADO: NO PROBADO EN R (no hay R en mi entorno). Revisado solo en su
# estructura. Pruébalo con app_demo_evidencia.R antes de integrarlo.
#
# QUÉ MUESTRA (ficha de un sistema)
#   - Cabecera: nivel de evidencia (alto / medio / bajo) y conteos por origen.
#   - Lista de documentos, primero los de prioridad A (sistema + costos en el título),
#     con la cita en APA, el enlace (DOI o handle de la BAC) y las bases que lo encontraron.
#   - Filtros: solo documentos que mencionan costos, solo prioridad A, origen, años, texto.
#   - Descargas: tabla en CSV y lista de citas APA en texto.
#   Solo se muestran metadatos y enlace, nunca el texto completo (decisión del 15 de
#   octubre de 2026: citar todo en APA y dar crédito a la fuente de origen).
#
# DATOS (salida de evidencia_03_unificar.R, carpeta evidencia_salida/)
#   evidencia.csv, evidencia_sistema.csv, resumen_evidencia_por_sistema.csv
#   Para publicar desde GitHub en Posit Connect Cloud, esa carpeta va en el repositorio.
#
# USO EN LA APP
#   source("mod_evidencia.R")
#   datos <- cargar_evidencia("evidencia_salida")              # una vez, al iniciar
#   ui:     mod_evidencia_ui("ev")
#   server: mod_evidencia_server("ev", sistema = reactive(input$sistema), datos = datos)
#   "sistema" debe ser uno de datos$resumen$sistema (p. ej. "papa", "leche/lechería").
#
# PAQUETES: shiny, dplyr, readr, stringr, htmltools
# =====================================================================
library(shiny); library(dplyr); library(readr); library(stringr); library(htmltools)

# ---------------------------------------------------------------------
# Carga de datos
# ---------------------------------------------------------------------
cargar_evidencia <- function(dir = "evidencia_salida") {
  leer <- function(f) read_csv(file.path(dir, f), col_types = cols(.default = col_character()),
                               na = c("", "NA"), show_col_types = FALSE, progress = FALSE)
  ev <- leer("evidencia.csv") |>
    mutate(anio = suppressWarnings(as.integer(anio)),
           n_bases = suppressWarnings(as.integer(n_bases)),
           costo_explicito = costo_explicito %in% c("TRUE", "true", "T"))
  vin <- leer("evidencia_sistema.csv") |> select(id_evidencia, sistema, vinculo)
  res <- leer("resumen_evidencia_por_sistema.csv") |>
    mutate(across(c(documentos_directos, prioridad_A_directos, con_costo_explicito,
                    contexto_bovino, en_BAC, en_AGRIS, anio_mas_reciente),
                  \(x) suppressWarnings(as.integer(x))))
  list(evidencia = ev, vinculos = vin, resumen = res)
}

# ---------------------------------------------------------------------
# Utilidades de presentación
# ---------------------------------------------------------------------
# La cita APA trae etiquetas <i> (cursiva). Se escapa todo el HTML y solo se restauran
# las <i>: los metadatos vienen de fuentes externas y no deben inyectar código.
cita_segura <- function(x) {
  x <- htmlEscape(coalesce(x, ""))
  x <- gsub("&lt;i&gt;", "<i>", x, fixed = TRUE)
  gsub("&lt;/i&gt;", "</i>", x, fixed = TRUE)
}
sin_etiquetas <- function(x) gsub("<[^>]+>", "", coalesce(x, ""))
es_url <- function(x) !is.na(x) && grepl("^https?://", x)

insignia <- function(texto, clase = "") tags$span(class = paste("ev-insignia", clase), texto)

tarjeta_evidencia <- function(r) {
  est <- coalesce(r$cita_estado, "")
  nota_cita <- if (startsWith(est, "borrador")) "Cita por revisar"
               else if (startsWith(est, "BAC")) "Cita armada con metadatos de la BAC" else NULL
  tags$div(class = "ev-tarjeta",
    tags$div(class = "ev-cita", HTML(cita_segura(r$cita_apa))),
    tags$div(class = "ev-meta",
      if (es_url(r$enlace)) tags$a(href = r$enlace, target = "_blank", rel = "noopener noreferrer", "Abrir documento"),
      if (identical(r$prioridad, "A")) insignia("Costos en el título", "ev-a"),
      if (isTRUE(r$costo_explicito)) insignia("Menciona costos de producción"),
      if (identical(r$vinculo, "contexto")) insignia("Contexto bovino general", "ev-ctx"),
      if (!is.null(nota_cita)) insignia(nota_cita, "ev-aviso"),
      tags$span(class = "ev-bases", paste("Encontrado en:", coalesce(r$bases, "—")))),
    if (!is.na(r$resumen) && nzchar(r$resumen))
      tags$details(tags$summary("Resumen"), tags$p(r$resumen)))
}

ESTILO_EVIDENCIA <- "
.ev-cabecera{border:1px solid rgba(0,79,159,.25);border-left:6px solid #009F3C;border-radius:8px;padding:12px 14px;margin-bottom:12px;background:#F6FAF7}
.ev-cabecera .ev-cifras{display:flex;flex-wrap:wrap;gap:18px;margin-top:6px}
.ev-cabecera .ev-cifra b{font-size:1.25em;display:block;color:#004F9F}
.ev-tarjeta{border:1px solid rgba(0,79,159,.2);border-radius:8px;padding:10px 12px;margin-bottom:8px;background:#fff}
.ev-cita{line-height:1.4}
.ev-meta{margin-top:6px;display:flex;flex-wrap:wrap;gap:6px;align-items:center;font-size:.88em}
.ev-bases{opacity:.8}
.ev-insignia{border:1px solid rgba(0,79,159,.45);border-radius:10px;padding:0 8px;color:#004F9F}
.ev-a{border-color:#007C3A;color:#007C3A;font-weight:600}.ev-aviso{border-style:dashed;border-color:#E9531B;color:#1D1D1B}.ev-ctx{opacity:.85}
.ev-nota{font-size:.85em;opacity:.85}
.ev-nivel-alto,.ev-nivel-medio,.ev-nivel-bajo{font-weight:700;font-size:1.1em;padding:0 8px;border-radius:4px;color:#fff}
.ev-nivel-alto{background:#007C3A}.ev-nivel-medio{background:#0067B1}.ev-nivel-bajo{background:#1D1D1B;border-bottom:4px solid #E9531B}
"

# ---------------------------------------------------------------------
# Interfaz
# ---------------------------------------------------------------------
mod_evidencia_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(ESTILO_EVIDENCIA)),
    uiOutput(ns("cabecera")),
    fluidRow(
      column(3, checkboxInput(ns("solo_costos"), "Solo documentos que mencionan costos de producción", TRUE)),
      column(3, checkboxInput(ns("solo_A"), "Solo prioridad A (costos en el título)", FALSE)),
      column(3, selectInput(ns("origen"), "Origen",
        c("Todos" = "todos", "Repositorio y literatura gris (BAC)" = "bac",
          "Artículos y bases científicas" = "articulos", "AGRIS" = "agris"))),
      column(3, textInput(ns("buscar"), "Buscar en título o resumen", ""))),
    fluidRow(
      column(6, uiOutput(ns("ui_anios"))),
      column(6, uiOutput(ns("ui_contexto")))),
    uiOutput(ns("conteo")),
    uiOutput(ns("lista")),
    uiOutput(ns("ui_mas")),
    tags$hr(),
    fluidRow(
      column(6, downloadButton(ns("bajar_csv"), "Descargar tabla (CSV)"),
                downloadButton(ns("bajar_citas"), "Descargar citas APA (texto)")),
      column(6, tags$p(class = "ev-nota",
        "La app muestra metadatos, enlace y cita de cada documento, no su texto completo. ",
        "Los derechos de cada documento son de sus titulares. Las citas con la etiqueta ",
        "“por revisar” se armaron con los metadatos disponibles y requieren verificación."))))
}

# ---------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------
mod_evidencia_server <- function(id, sistema, datos, por_pagina = 15,
                                 sistemas_con_contexto = c("leche/lechería", "ganadería de ceba")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Documentos del sistema (directos y, si hay, de contexto), con todos sus datos
    base <- reactive({
      sis <- sistema(); req(sis)
      datos$vinculos |>
        filter(sistema == sis) |>
        select(id_evidencia, vinculo) |>
        inner_join(datos$evidencia, by = "id_evidencia")
    })

    resumen_sis <- reactive({
      sis <- sistema(); req(sis)
      filter(datos$resumen, sistema == sis)
    })

    output$cabecera <- renderUI({
      r <- resumen_sis(); req(nrow(r) == 1)
      tags$div(class = "ev-cabecera",
        tags$div(tags$b("Evidencia científica y técnica: "),
                 tags$span(class = paste0("ev-nivel-", r$nivel_evidencia), toupper(r$nivel_evidencia))),
        tags$div(class = "ev-cifras",
          tags$div(class = "ev-cifra", tags$b(r$documentos_directos), "documentos directos"),
          tags$div(class = "ev-cifra", tags$b(r$con_costo_explicito), "mencionan costos de producción"),
          tags$div(class = "ev-cifra", tags$b(r$prioridad_A_directos), "con costos en el título"),
          tags$div(class = "ev-cifra", tags$b(r$en_BAC), "en la BAC (AGROSAVIA)"),
          tags$div(class = "ev-cifra", tags$b(r$en_AGRIS), "en AGRIS"),
          if (isTRUE(r$contexto_bovino > 0))
            tags$div(class = "ev-cifra", tags$b(r$contexto_bovino), "de contexto bovino general")),
        tags$p(class = "ev-nota",
          "Nivel según los documentos directos que mencionan costos de producción en el título o el resumen: ",
          "alto, 30 o más; medio, de 15 a 29; bajo, menos de 15 (umbrales propuestos). ",
          "Mide la literatura disponible, no la calidad de las cifras de costos."))
    })

    output$ui_anios <- renderUI({
      a <- base()$anio; a <- a[!is.na(a)]; req(length(a) > 0)
      sliderInput(ns("anios"), "Años de publicación", min = min(a), max = max(a),
                  value = c(min(a), max(a)), sep = "", width = "100%")
    })

    output$ui_contexto <- renderUI({
      if (sistema() %in% sistemas_con_contexto)
        checkboxInput(ns("incluir_contexto"), "Incluir contexto de ganadería bovina general", FALSE)
    })

    filtrado <- reactive({
      d <- base()
      if (!isTRUE(input$incluir_contexto)) d <- filter(d, vinculo == "directo")
      if (isTRUE(input$solo_costos)) d <- filter(d, costo_explicito)
      if (isTRUE(input$solo_A)) d <- filter(d, prioridad == "A")
      d <- switch(coalesce(input$origen, "todos"),
        bac       = filter(d, str_detect(bases, "BAC")),
        articulos = filter(d, str_detect(bases, "Scopus|WoS|SciELO|Lens|Dimensions|OpenAlex")),
        agris     = filter(d, str_detect(bases, "AGRIS")),
        d)
      if (!is.null(input$anios))
        d <- filter(d, is.na(anio) | (anio >= input$anios[1] & anio <= input$anios[2]))
      q <- tolower(trimws(coalesce(input$buscar, "")))
      if (nzchar(q))
        d <- filter(d, str_detect(tolower(paste(coalesce(titulo, ""), coalesce(resumen, ""))), fixed(q)))
      arrange(d, prioridad, desc(costo_explicito), vinculo != "directo", desc(anio))
    })

    # Paginación: se muestran 'por_pagina' y un botón "Mostrar más"
    n_mostrar <- reactiveVal(por_pagina)
    observeEvent(filtrado(), n_mostrar(por_pagina))
    observeEvent(input$mas, n_mostrar(n_mostrar() + por_pagina))

    output$conteo <- renderUI({
      tags$p(class = "ev-nota", sprintf("%d documentos con estos filtros.", nrow(filtrado())))
    })

    output$lista <- renderUI({
      d <- filtrado()
      if (!nrow(d)) return(tags$p("No hay documentos con estos filtros. Prueba quitar “solo documentos que mencionan costos”."))
      d <- head(d, n_mostrar())
      tagList(lapply(seq_len(nrow(d)), \(i) tarjeta_evidencia(d[i, ])))
    })

    output$ui_mas <- renderUI({
      if (nrow(filtrado()) > n_mostrar())
        actionButton(ns("mas"), sprintf("Mostrar %d más", por_pagina))
    })

    # Descargas (lo filtrado, no solo lo visible)
    output$bajar_csv <- downloadHandler(
      filename = \() paste0("evidencia_", gsub("[^a-z0-9]+", "_", tolower(sistema())), ".csv"),
      content = \(file) {
        d <- filtrado() |>
          transmute(sistema = sistema(), anio, titulo, autores, fuente, tipo, doi, enlace,
                    cita_apa = sin_etiquetas(cita_apa), cita_estado, prioridad, vinculo,
                    menciona_costos = costo_explicito, bases)
        write_excel_csv(d, file, na = "")      # con BOM para que Excel respete las tildes
      })
    output$bajar_citas <- downloadHandler(
      filename = \() paste0("citas_apa_", gsub("[^a-z0-9]+", "_", tolower(sistema())), ".txt"),
      content = \(file) {
        cit <- sort(unique(sin_etiquetas(filtrado()$cita_apa)))
        writeLines(enc2utf8(cit), file, useBytes = TRUE)
      })
  })
}
