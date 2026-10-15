# =====================================================================
# Módulo de Shiny: consulta de la línea base de costos por sistema productivo
# Observatorio de costos de producción agropecuarios de Colombia (beta)
#
# ESTADO: NO PROBADO EN R (no hay R en mi entorno). Revisado solo en su estructura.
#
# QUÉ MUESTRA (ficha de un sistema)
#   - Cabecera: grado de completitud, número de guías, años de los datos, observaciones,
#     enlaces de contraste (UPRA/Agronet) y cómo citar la fuente.
#   - Pestaña "Guías": una fila por guía de Finagro, con el año de la fuente y la
#     actualización de Finagro como dos momentos (sin ajuste propio, decisión del 15/10).
#   - Pestaña "Estructura de costos": barras apiladas por rubro de cada guía.
#   - Pestaña "Comparar": costo por unidad, por hectárea y ciclo, por hectárea y año
#     (perennes) y margen sobre ventas.
#   - Pestaña "Notas y límites": la nota técnica de comparabilidad.
#   - Opción "Incluir ajustes de usuarios": superpone los ajustes vigentes, rotulados.
#
# DATOS (carpeta datos_linea_base/, generada a partir de mra_linea_base_preliminar.xlsx)
#   sistemas.csv, guias.csv, rubros.csv, enlaces_contraste.csv
#
# USO
#   datos <- cargar_linea_base("datos_linea_base")
#   mod_linea_base_ui("lb")
#   mod_linea_base_server("lb", sistema_id = reactive(input$sistema), datos = datos,
#                         ajustes = reactive(tabla_ajustes_vacia()))
#
# PAQUETES: shiny, dplyr, tidyr, readr, stringr, ggplot2, scales, DT, htmltools
#   Requiere cargar antes tema_agrosavia.R (PALETA_CATEGORICA, COLORES_AGROSAVIA, theme_agrosavia).
# =====================================================================
library(shiny); library(dplyr); library(tidyr); library(readr); library(stringr)
library(ggplot2); library(DT); library(htmltools)

CITA_FINAGRO <- paste0("Finagro. (2026). Marcos de Referencia Agroeconómicos (MRA) [Agroguías]. ",
                       "Fondo para el Financiamiento del Sector Agropecuario. https://www.finagro.com.co")

cargar_linea_base <- function(dir = "datos_linea_base") {
  leer <- function(f) read_csv(file.path(dir, f), show_col_types = FALSE, progress = FALSE,
                               na = c("", "NA"), locale = locale(encoding = "UTF-8"))
  list(sistemas = leer("sistemas.csv"), guias = leer("guias.csv"), rubros = leer("rubros.csv"),
       enlaces = leer("enlaces_contraste.csv"))
}

# ---------------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------------
fmt_num <- function(x, dec = 0) {
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = dec, big.mark = ".", decimal.mark = ","))
}
fmt_cop <- function(x) ifelse(is.na(x), "—", paste0("$", fmt_num(x)))

etiqueta_guias <- function(g) {
  base <- paste0(coalesce(g$departamento, "s. d."), " · ", g$anio_original)
  make.unique(base, sep = " #")
}

# Rubros por guía con ajustes vigentes aplicados (rubro agregado: mano de obra + insumos)
rubros_por_guia <- function(rubros, slugs, momento, ajustes = NULL) {
  d <- rubros |> filter(slug %in% slugs, momento == !!momento) |>
    group_by(slug, rubro) |> summarise(valor = sum(valor), unidad = first(unidad), .groups = "drop") |>
    mutate(ajustado = FALSE)
  if (!is.null(ajustes) && nrow(ajustes)) {
    a <- ajustes |> filter(vigente, momento == !!momento, startsWith(campo, "rubro:")) |>
      mutate(rubro = sub("^rubro:", "", campo)) |> select(slug, rubro, nuevo = valor_propuesto)
    d <- d |> left_join(a, by = c("slug", "rubro")) |>
      mutate(ajustado = !is.na(nuevo), valor = ifelse(ajustado, nuevo, valor)) |> select(-nuevo)
  }
  d
}

insignia_grado <- function(g) {
  clase <- switch(g, "Alta" = "ev-nivel-alto", "Media" = "ev-nivel-medio", "ev-nivel-bajo")
  tags$span(class = clase, toupper(g))
}

NOTA_COMPARABILIDAD <- list(
  "Los valores son los de las guías de Finagro (Marcos de Referencia Agroeconómicos). No se ajustan por inflación: cada cifra lleva el año de su fuente. Finagro publica dos momentos por guía, el original y su propia actualización de precios.",
  "Los costos excluyen tierra, impuestos y gastos administrativos y financieros. La guía de leche cundiboyacense incluye arrendamiento.",
  "En los cultivos perennes (café, cacao, aguacate, banano, entre otros), costo y producción son acumulados nominales de toda la vida productiva (5 a 30 años). El costo por hectárea y año divide el total por los años del ciclo: es una simplificación que no incluye el costo del dinero.",
  "El margen se muestra sobre ventas, (precio − costo) / precio. Los XLSX de Finagro lo calculan sobre costos (utilidad / costo), por lo que se convierte.",
  "La ganadería de ceba incluye la compra del ternero y muestra su desglose cuando la guía lo trae.",
  "Las unidades de las guías pecuarias varían (litro, kg, kg en pie, ave, estanque). Compara solo guías de la misma unidad.",
  "El detalle por ítem (precios y cantidades de cada insumo) es solo para aliados de la Red Nacional de MRA.")

# ---------------------------------------------------------------------
# Interfaz
# ---------------------------------------------------------------------
mod_linea_base_ui <- function(id) {
  ns <- NS(id)
  tagList(
    if (exists("ESTILO_EVIDENCIA")) tags$style(HTML(ESTILO_EVIDENCIA)),   # insignias y cabecera (de mod_evidencia.R)
    uiOutput(ns("cabecera")),
    checkboxInput(ns("con_ajustes"), "Incluir los ajustes de usuarios vigentes (se rotulan como ajustados)", FALSE),
    tabsetPanel(
      tabPanel("Guías", br(), DTOutput(ns("tabla_guias"))),
      tabPanel("Estructura de costos", br(),
        fluidRow(
          column(4, uiOutput(ns("ui_momento"))),
          column(8, uiOutput(ns("ui_guias")))),
        plotOutput(ns("grafico_rubros"), height = "430px"),
        uiOutput(ns("nota_rubros"))),
      tabPanel("Comparar", br(),
        fluidRow(column(5, uiOutput(ns("ui_medida")))),
        plotOutput(ns("grafico_comparar"), height = "430px")),
      tabPanel("Notas y límites", br(), uiOutput(ns("notas")))),
    tags$hr(),
    tags$p(class = "ev-nota", tags$b("Cómo citar: "), CITA_FINAGRO))
}

# ---------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------
mod_linea_base_server <- function(id, sistema_id, datos, ajustes = reactive(tabla_ajustes_vacia())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sis <- reactive({ req(sistema_id()); datos$sistemas |> filter(sistema_id == sistema_id()) })
    guias <- reactive({
      datos$guias |> filter(sistema_id == sistema_id()) |>
        arrange(desc(anio_original)) |> mutate(etiqueta = etiqueta_guias(pick(everything())))
    })
    aj <- reactive(if (isTRUE(input$con_ajustes)) ajustes() else NULL)

    output$cabecera <- renderUI({
      s <- sis(); g <- guias()
      enl <- datos$enlaces |> filter(sistema_id == sistema_id())
      tags$div(class = "ev-cabecera",
        tags$div(tags$h4(style = "display:inline;margin-right:10px", s$sistema),
                 "Completitud de la línea base: ", insignia_grado(s$grado_completitud)),
        tags$div(class = "ev-cifras",
          tags$div(class = "ev-cifra", tags$b(s$n_guias), "guías de Finagro"),
          tags$div(class = "ev-cifra", tags$b(s$n_con_rubros), "con estructura por rubro"),
          tags$div(class = "ev-cifra", tags$b(if (is.na(s$anio_datos_min)) "—" else
            paste0(s$anio_datos_min, if (s$anio_datos_max != s$anio_datos_min) paste0("–", s$anio_datos_max) else "")),
            "año de los datos originales")),
        if (!is.na(s$observacion) && nzchar(s$observacion)) tags$p(style = "margin:8px 0 0", s$observacion),
        if (s$n_guias == 0) tags$p(style = "margin:8px 0 0",
          "Este sistema no tiene agroguía de Finagro. Se cubrirá con los datos de Fedepalma y Cenipalma."),
        if (nrow(enl)) tags$p(class = "ev-nota", style = "margin:8px 0 0", "Para contrastar: ",
          tagList(lapply(seq_len(nrow(enl)), function(i)
            tagList(tags$a(href = enl$url[i], target = "_blank", rel = "noopener noreferrer", enl$etiqueta[i]), " ")))))
    })

    # ---- Guías ----
    output$tabla_guias <- renderDT({
      g <- guias(); req(nrow(g) > 0)
      idioma <- list(search = "Buscar:", lengthMenu = "Mostrar _MENU_", zeroRecords = "Sin resultados",
                     info = "_START_ a _END_ de _TOTAL_", paginate = list(previous = "Anterior", `next` = "Siguiente"))
      if (g$grupo[1] == "agrícola") {
        t <- g |> transmute(`Guía` = str_to_sentence(titulo), Departamento = departamento,
          `Datos originales` = periodo_original, `Costo original (COP/ha·ciclo)` = costo_original,
          `Actualización Finagro` = periodo_actualizado, `Costo actualizado (COP/ha·ciclo)` = costo_actualizado,
          `Ciclo (años)` = duracion_anios, `Costo por ha·año (perennes)` = costo_ha_anio,
          `Costo por kg` = costo_por_kg, `Producción (t/ha·ciclo)` = produccion_t_ha_ciclo,
          `Margen sobre ventas (%)` = margen_sobre_ventas_pct)
        datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, language = idioma)) |>
          formatCurrency(c("Costo original (COP/ha·ciclo)", "Costo actualizado (COP/ha·ciclo)",
                           "Costo por ha·año (perennes)", "Costo por kg"),
                         currency = "$", digits = 0, mark = ".", dec.mark = ",") |>
          formatRound(c("Producción (t/ha·ciclo)", "Margen sobre ventas (%)"), digits = 1, mark = ".", dec.mark = ",")
      } else {
        t <- g |> transmute(`Guía` = str_to_sentence(titulo), Departamento = departamento,
          `Año de los datos` = periodo_original, Unidad = unidad_costo, `Costo (COP/unidad)` = costo_original,
          `Precio de venta (COP/unidad)` = precio_venta, `Margen sobre ventas (%)` = margen_sobre_ventas_pct,
          Nota = nota)
        datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, language = idioma)) |>
          formatCurrency(c("Costo (COP/unidad)", "Precio de venta (COP/unidad)"),
                         currency = "$", digits = 0, mark = ".", dec.mark = ",") |>
          formatRound("Margen sobre ventas (%)", digits = 1, mark = ".", dec.mark = ",")
      }
    })

    # ---- Estructura de costos ----
    momentos <- reactive({
      m <- datos$rubros |> filter(slug %in% guias()$slug) |> distinct(momento) |> pull(momento)
      c("actualizado", "original")[c("actualizado", "original") %in% m]
    })
    output$ui_momento <- renderUI({
      m <- momentos()
      if (!length(m)) return(tags$p(class = "ev-nota", "Este sistema no tiene estructura por rubro."))
      radioButtons(ns("momento"), "Momento",
        choices = setNames(m, ifelse(m == "actualizado", "Actualización de Finagro", "Datos originales")))
    })
    guias_con_rubros <- reactive({
      g <- guias(); g[g$slug %in% datos$rubros$slug, ]
    })
    output$ui_guias <- renderUI({
      g <- guias_con_rubros(); req(nrow(g) > 0)
      selectInput(ns("guias"), "Guías", choices = setNames(g$slug, g$etiqueta),
                  selected = head(g$slug, 6), multiple = TRUE, width = "100%")
    })
    datos_rubros <- reactive({
      req(input$momento, length(input$guias) > 0)
      d <- rubros_por_guia(datos$rubros, input$guias, input$momento, aj())
      g <- guias_con_rubros()
      top <- d |> group_by(rubro) |> summarise(t = sum(valor), .groups = "drop") |>
        arrange(desc(t)) |> slice_head(n = 7) |> pull(rubro)
      d |> mutate(rubro_g = ifelse(rubro %in% top, rubro, "Otros rubros"),
                  etiqueta = g$etiqueta[match(slug, g$slug)]) |>
        group_by(etiqueta, rubro_g, unidad) |>
        summarise(valor = sum(valor), ajustado = any(ajustado), .groups = "drop") |>
        mutate(etiqueta = ifelse(ajustado, paste0(etiqueta, " (ajustado)"), etiqueta))
    })
    output$grafico_rubros <- renderPlot({
      d <- datos_rubros(); req(nrow(d) > 0)
      etq <- d |> group_by(etiqueta) |> summarise(t = sum(valor), .groups = "drop") |> arrange(t)
      d$etiqueta <- factor(d$etiqueta, levels = etq$etiqueta)
      grande <- max(etq$t) >= 1e6
      ggplot(d, aes(x = etiqueta, y = valor, fill = rubro_g)) +
        geom_col(width = 0.7) + coord_flip() +
        scale_fill_manual(values = setNames(rep_len(PALETA_CATEGORICA, length(unique(d$rubro_g))), sort(unique(d$rubro_g)))) +
        scale_y_continuous(labels = if (grande) scales::label_number(scale = 1e-6, suffix = " M", big.mark = ".", decimal.mark = ",")
                                    else scales::label_number(big.mark = ".", decimal.mark = ",")) +
        labs(x = NULL, y = paste0(unique(d$unidad)[1], if (grande) " (millones de pesos)" else ""), fill = "Rubro") +
        theme_agrosavia(13)
    })
    output$nota_rubros <- renderUI({
      tags$p(class = "ev-nota", "Se muestran los 7 rubros de mayor valor y el resto como “Otros rubros”. ",
             "Cada barra suma el costo total de la guía en el momento elegido (mano de obra más insumos).")
    })

    # ---- Comparar ----
    medidas <- reactive({
      g <- guias()
      if (nrow(g) == 0) return(character())
      if (g$grupo[1] == "agrícola")
        c("Costo por kg (COP)" = "costo_por_kg", "Costo actualizado por ha y ciclo (COP)" = "costo_actualizado",
          "Costo por ha y año, solo perennes (COP)" = "costo_ha_anio", "Margen sobre ventas (%)" = "margen_sobre_ventas_pct")
      else
        c("Costo por unidad (COP)" = "costo_original", "Precio de venta por unidad (COP)" = "precio_venta",
          "Margen sobre ventas (%)" = "margen_sobre_ventas_pct")
    })
    output$ui_medida <- renderUI({
      m <- medidas(); req(length(m) > 0)
      selectInput(ns("medida"), "Medida", choices = m)
    })
    output$grafico_comparar <- renderPlot({
      req(input$medida)
      g <- guias() |> mutate(v = .data[[input$medida]]) |> filter(!is.na(v))
      shiny::validate(shiny::need(nrow(g) > 0, "Ninguna guía de este sistema tiene esa medida."))
      if (g$grupo[1] == "pecuario" && input$medida != "margen_sobre_ventas_pct")
        g <- g |> mutate(etiqueta = paste0(etiqueta, " · ", sub("^COP por ", "", unidad_costo)))
      g <- g |> arrange(v); g$etiqueta <- factor(g$etiqueta, levels = g$etiqueta)
      es_margen <- input$medida == "margen_sobre_ventas_pct"
      grande <- !es_margen && max(abs(g$v)) >= 1e6
      g$signo <- ifelse(g$v < 0, "Negativo", "Positivo")
      ggplot(g, aes(x = etiqueta, y = v, fill = signo)) +
        geom_col(width = 0.7) + coord_flip() +
        geom_text(aes(label = if (es_margen) paste0(fmt_num(v, 1), " %") else if (grande) paste0(fmt_num(v / 1e6, 1), " M") else fmt_num(v)),
                  hjust = ifelse(g$v < 0, 1.1, -0.1), size = 3.6) +
        scale_fill_manual(values = c(Positivo = COLORES_AGROSAVIA$verde_agro, Negativo = COLORES_AGROSAVIA$naranja), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0.12, 0.18))) +
        labs(x = NULL, y = names(medidas())[medidas() == input$medida]) + theme_agrosavia(13)
    })

    output$notas <- renderUI(tags$ul(lapply(NOTA_COMPARABILIDAD, tags$li)))
  })
}
