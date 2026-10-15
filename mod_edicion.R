# =====================================================================
# Módulo de Shiny: ajuste y edición de la línea base por usuarios expertos
# Observatorio de costos de producción agropecuarios de Colombia (beta)
#
# ESTADO: NO PROBADO EN R (no hay R en mi entorno). Revisado solo en su estructura.
#
# QUÉ HACE
#   - El usuario elige una guía de Finagro, ve cada rubro con su valor de la línea base y
#     escribe su valor ajustado. También puede ajustar producción y valor del jornal
#     (agrícolas) o precio y costo por unidad (pecuarios).
#   - Debe escribir una justificación (mínimo 15 caracteres) y puede citar su fuente en APA.
#   - Al guardar se crea un ajuste firmado con su identidad, que aparece de inmediato
#     y rotulado junto a la línea base (decisión del 6 de octubre: sin moderación previa).
#   - Otros usuarios pueden confirmar un ajuste. Con UMBRAL_CONFIRMACIONES confirmaciones
#     de personas distintas al autor, el ajuste se marca "validado" (el umbral es una
#     propuesta; la regla definitiva está pendiente, T6).
#   - El autor puede retirar su ajuste. Nada se borra: queda en el historial.
#   - La línea base nunca se modifica; los ajustes viven en el almacén (almacen_ediciones.R).
#
# IDENTIDAD
#   En Posit Connect Cloud, con el contenido compartido en modo privado, el correo del
#   visitante llega en session$user. Si no llega (pruebas locales o contenido público), el
#   módulo pide escribir el correo: SIN verificación, solo sirve para probar.
#
# USO
#   alm <- almacen_desde_entorno()
#   ed <- mod_edicion_server("ed", sistema_id = reactive(input$sistema), datos = datos, almacen = alm)
#   # ed$ajustes() devuelve la tabla de ajustes del sistema (para mod_linea_base_server)
#
# REQUIERE haber cargado antes: almacen_ediciones.R y mod_linea_base.R (fmt_cop, etiqueta_guias)
# PAQUETES: shiny, dplyr, tibble, DT
# =====================================================================
library(shiny); library(dplyr); library(tibble); library(DT)

MIN_JUSTIFICACION <- 15

# Las fechas se guardan en UTC; en pantalla se muestran en hora de Colombia
fecha_local <- function(x) {
  t <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ifelse(is.na(t), as.character(x), format(t, "%Y-%m-%d %H:%M", tz = "America/Bogota"))
}

params_guia <- function(g) {
  if (g$grupo == "agrícola") {
    tibble(campo = c("param:produccion_t_ha_ciclo", "param:valor_jornal"),
           etiqueta = c("Producción (t/ha en el ciclo)", "Valor del jornal (COP)"),
           valor = c(g$produccion_t_ha_ciclo, g$valor_jornal), unidad = c("t/ha en el ciclo", "COP por jornal"))
  } else {
    tibble(campo = c("param:costo_unidad", "param:precio_venta"),
           etiqueta = c("Costo de producción por unidad", "Precio de venta por unidad"),
           valor = c(g$costo_original, g$precio_venta), unidad = rep(g$unidad_costo, 2))
  }
}

mod_edicion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("estado_almacen")),
    uiOutput(ns("identidad")),
    fluidRow(column(7, uiOutput(ns("ui_guia"))), column(5, uiOutput(ns("ui_momento")))),
    uiOutput(ns("formulario")),
    uiOutput(ns("resumen")),
    textAreaInput(ns("justificacion"), "Justificación del ajuste (obligatoria)", rows = 3, width = "100%",
                  placeholder = "Por qué este valor es más adecuado: región, época, tecnología, fuente de los datos…"),
    textInput(ns("fuente"), "Fuente que respalda el ajuste (cita en APA, opcional)", width = "100%"),
    actionButton(ns("guardar"), "Guardar ajuste", class = "btn-primary"),
    tags$hr(),
    fluidRow(column(8, tags$h4("Ajustes registrados para esta guía")),
             column(4, actionButton(ns("actualizar"), "Actualizar lista", class = "btn-default"))),
    DTOutput(ns("tabla_ajustes")),
    uiOutput(ns("ui_confirmar")))
}

mod_edicion_server <- function(id, sistema_id, datos, almacen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresco <- reactiveVal(0)
    observeEvent(input$actualizar, refresco(refresco() + 1))

    # ---- identidad ----
    usuario_cuenta <- function() { u <- session$user; if (is.null(u)) "" else as.character(u) }
    usuario <- reactive({
      u <- usuario_cuenta()
      if (nzchar(u)) tolower(trimws(u)) else tolower(trimws(input$correo %||% ""))
    })
    output$identidad <- renderUI({
      if (nzchar(usuario_cuenta())) tags$p(class = "ev-nota", "Sesión de: ", tags$b(usuario_cuenta()))
      else textInput(ns("correo"), "Tu correo (modo de prueba, sin verificación)", width = "100%",
                     placeholder = "nombre@organizacion.org")
    })
    output$estado_almacen <- renderUI({
      if (isTRUE(almacen$persistente))
        tags$p(class = "ev-nota", "Los ajustes se guardan en: ", almacen$descripcion, ".")
      else
        tags$div(class = "ev-cabecera", style = "border-left-color:#E9531B",
          tags$b("Modo de prueba. "), "Los ajustes se guardan en ", almacen$descripcion,
          ". En Posit Connect Cloud el disco se borra al reiniciar la app: para que los ajustes ",
          "persistan hay que configurar el almacén de GitHub (ver almacen_ediciones.R).")
    })

    # ---- datos de la guía elegida ----
    guias <- reactive({
      req(sistema_id())
      datos$guias |> filter(sistema_id == sistema_id()) |> arrange(desc(anio_original)) |>
        mutate(etiqueta = etiqueta_guias(pick(everything())))
    })
    output$ui_guia <- renderUI({
      g <- guias()
      if (!nrow(g)) return(tags$p(class = "ev-nota", "Este sistema no tiene guías de Finagro para ajustar."))
      selectInput(ns("guia"), "Guía", choices = setNames(g$slug, paste0(g$etiqueta, " — ", stringr::str_to_sentence(g$titulo))),
                  width = "100%")
    })
    guia <- reactive({ req(input$guia); guias() |> filter(slug == input$guia) })
    momentos <- reactive({
      req(input$guia)
      m <- datos$rubros |> filter(slug == input$guia) |> distinct(momento) |> pull(momento)
      m <- c("actualizado", "original")[c("actualizado", "original") %in% m]
      if (length(m)) m else "original"
    })
    output$ui_momento <- renderUI({
      req(input$guia); m <- momentos()
      radioButtons(ns("momento"), "Momento que se ajusta", inline = TRUE,
                   choices = setNames(m, ifelse(m == "actualizado", "Actualización de Finagro", "Datos originales")))
    })
    rubros_guia <- reactive({
      req(input$guia, input$momento)
      d <- datos$rubros |> filter(slug == input$guia, momento == input$momento)
      orden <- unique(d$rubro)
      # Se redondea la línea base (pesos enteros; 2 decimales si es menor a 1.000) para que la casilla
      # muestre el mismo valor que la tabla y un valor sin tocar no cuente como cambio.
      d |> group_by(rubro) |> summarise(valor = sum(valor), unidad = first(unidad), .groups = "drop") |>
        mutate(valor = ifelse(abs(valor) >= 1000, round(valor, 0), round(valor, 2))) |>
        arrange(match(rubro, orden))
    })
    params <- reactive({ params_guia(guia()) })
    ajustes_sistema <- reactive({
      refresco(); req(sistema_id())
      tryCatch(procesar_ediciones(almacen$leer(sistema_id())), error = function(e) {
        showNotification(paste("No se pudieron leer los ajustes:", conditionMessage(e)), type = "error")
        tabla_ajustes_vacia()
      })
    })
    ajustes_guia <- reactive({ req(input$guia); ajustes_sistema() |> filter(slug == input$guia) })

    # ---- formulario ----
    output$formulario <- renderUI({
      rb <- rubros_guia(); pr <- params()
      vig <- ajustes_guia() |> filter(vigente, momento == input$momento)
      vigente_de <- function(campo) { v <- vig$valor_propuesto[vig$campo == campo]; if (length(v)) fmt_num(v[1]) else "" }
      cab <- fluidRow(column(4, tags$b("Rubro")), column(3, tags$b("Línea base")),
                      column(2, tags$b("Ajuste vigente")), column(3, tags$b("Tu valor")))
      filas_r <- lapply(seq_len(nrow(rb)), function(i)
        fluidRow(column(4, tags$div(style = "padding-top:7px", rb$rubro[i])),
                 column(3, tags$div(style = "padding-top:7px", fmt_cop(rb$valor[i]))),
                 column(2, tags$div(style = "padding-top:7px", vigente_de(paste0("rubro:", rb$rubro[i])))),
                 column(3, numericInput(ns(paste0("r_", i)), NULL, value = rb$valor[i], min = 0))))
      filas_p <- lapply(seq_len(nrow(pr)), function(i)
        fluidRow(column(4, tags$div(style = "padding-top:7px", pr$etiqueta[i])),
                 column(3, tags$div(style = "padding-top:7px", if (is.na(pr$valor[i])) "—" else fmt_num(pr$valor[i], 2))),
                 column(2, tags$div(style = "padding-top:7px", vigente_de(pr$campo[i]))),
                 column(3, numericInput(ns(paste0("p_", i)), NULL, value = if (is.na(pr$valor[i])) NA else pr$valor[i], min = 0))))
      tagList(
        if (nrow(rb)) tagList(tags$h5(paste0("Rubros (", rb$unidad[1], ")")), cab, filas_r)
        else tags$p(class = "ev-nota", "Esta guía no tiene estructura por rubro; solo puedes ajustar los parámetros."),
        tags$h5("Parámetros"), filas_p)
    })

    cambios <- reactive({
      rb <- rubros_guia(); pr <- params()
      tomar <- function(pref, n) vapply(seq_len(n), function(i) {
        v <- input[[paste0(pref, i)]]; if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v) }, numeric(1))
      bind_rows(
        tibble(campo = paste0("rubro:", rb$rubro), valor_base = rb$valor, valor_propuesto = tomar("r_", nrow(rb)), unidad = rb$unidad),
        tibble(campo = pr$campo, valor_base = pr$valor, valor_propuesto = tomar("p_", nrow(pr)), unidad = pr$unidad)) |>
        mutate(slug = input$guia, momento = input$momento)
    })
    # Solo lo que el usuario cambió
    cambios_hechos <- reactive({
      cb <- cambios()
      cb |> filter(!is.na(valor_propuesto), is.na(valor_base) | abs(valor_propuesto - valor_base) > 1e-9)
    })

    output$resumen <- renderUI({
      cb <- cambios(); rb <- cb |> filter(startsWith(campo, "rubro:"))
      if (nrow(rb) == 0 || any(is.na(rb$valor_propuesto))) return(NULL)
      tb <- sum(rb$valor_base); ta <- sum(rb$valor_propuesto)
      g <- guia()
      kg <- NULL
      if (g$grupo == "agrícola" && input$momento == "actualizado" && !is.na(g$costo_por_kg)) {
        prod <- cb |> filter(campo == "param:produccion_t_ha_ciclo")
        f_prod <- if (nrow(prod) && !is.na(prod$valor_propuesto) && !is.na(prod$valor_base) && prod$valor_base > 0)
          prod$valor_propuesto / prod$valor_base else 1
        kg <- g$costo_por_kg * (ta / tb) / f_prod
      }
      tags$div(class = "ev-cabecera",
        tags$div(class = "ev-cifras",
          tags$div(class = "ev-cifra", tags$b(fmt_cop(tb)), "total de la línea base"),
          tags$div(class = "ev-cifra", tags$b(fmt_cop(ta)), "total con tus valores"),
          tags$div(class = "ev-cifra", tags$b(paste0(if (ta >= tb) "+" else "", fmt_num((ta / tb - 1) * 100, 1), " %")), "diferencia"),
          if (!is.null(kg)) tags$div(class = "ev-cifra", tags$b(fmt_cop(kg)), "costo por kg con tus valores (base: ", fmt_cop(g$costo_por_kg), ")")),
        tags$p(class = "ev-nota", style = "margin:6px 0 0",
               "Es una vista previa. El margen no se recalcula en esta versión."))
    })

    # ---- guardar ----
    observeEvent(input$guardar, {
      u <- usuario(); just <- trimws(input$justificacion %||% ""); ch <- cambios_hechos()
      falla <- function(m) showNotification(m, type = "warning", duration = 8)
      if (!grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", u)) return(falla("Escribe un correo válido para firmar el ajuste."))
      if (anyNA(cambios()$valor_propuesto[!is.na(cambios()$valor_base)]))
        return(falla("Hay casillas vacías: completa todos los valores (o déjalos como la línea base)."))
      if (nrow(ch) == 0) return(falla("No cambiaste ningún valor respecto a la línea base."))
      if (any(!is.finite(ch$valor_propuesto) | ch$valor_propuesto < 0 | ch$valor_propuesto > 1e13))
        return(falla("Los valores deben ser números entre 0 y 10 billones."))
      if (nchar(just) < MIN_JUSTIFICACION)
        return(falla(paste0("La justificación es obligatoria (mínimo ", MIN_JUSTIFICACION, " caracteres).")))
      tryCatch({
        registrar_ajuste(almacen, sistema_id(), u, just, input$fuente,
                         ch |> select(slug, momento, campo, valor_base, valor_propuesto, unidad))
        showNotification(paste0("Ajuste guardado (", nrow(ch), " cambio", if (nrow(ch) > 1) "s" else "", ")."), type = "message")
        updateTextAreaInput(session, "justificacion", value = ""); updateTextInput(session, "fuente", value = "")
        refresco(refresco() + 1)
      }, error = function(e) showNotification(paste("No se pudo guardar:", conditionMessage(e)), type = "error", duration = NULL))
    })

    # ---- historial, confirmar y retirar ----
    estado <- function(d) ifelse(d$retirado, "Retirado",
                          ifelse(d$vigente & d$validado, "Vigente y validado",
                          ifelse(d$vigente, "Vigente", "Reemplazado")))
    output$tabla_ajustes <- renderDT({
      d <- ajustes_guia()
      if (!nrow(d)) d <- tabla_ajustes_vacia()
      t <- d |> transmute(Fecha = fecha_local(fecha), Usuario = usuario, Momento = momento,
                          Campo = sub("^(rubro|param):", "", campo), `Línea base` = valor_base, Propuesto = valor_propuesto,
                          Confirmaciones = n_confirmaciones, Estado = estado(d), `Justificación` = justificacion,
                          Fuente = fuente)
      datatable(t, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, order = list(list(0, "desc")),
        language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ registros", zeroRecords = "Aún no hay ajustes para esta guía",
                        info = "_START_ a _END_ de _TOTAL_", paginate = list(previous = "Anterior", `next` = "Siguiente")))) |>
        formatRound(c("Línea base", "Propuesto"), digits = 0, mark = ".", dec.mark = ",")
    })
    opciones_ajustes <- function(d) {
      if (nrow(d) == 0) return(character(0))   # paste0() con un vector vacío devuelve 1 elemento y rompe setNames()
      d <- d |> group_by(ajuste_id, usuario, fecha) |> summarise(n = n(), .groups = "drop") |> arrange(desc(ajuste_id))
      setNames(d$ajuste_id, paste0(fecha_local(d$fecha), " · ", d$usuario, " · ", d$n, " cambio(s)"))
    }
    output$ui_confirmar <- renderUI({
      u <- usuario(); d <- ajustes_sistema() |> filter(!retirado)
      if (!nrow(d) || !nzchar(u)) return(NULL)
      ajenos <- opciones_ajustes(d |> filter(usuario != u)); propios <- opciones_ajustes(d |> filter(usuario == u))
      tagList(tags$hr(),
        if (length(ajenos)) fluidRow(
          column(8, selectInput(ns("ajuste_confirmar"), "Confirmar el ajuste de otro usuario", choices = ajenos, width = "100%")),
          column(4, tags$div(style = "margin-top:25px", actionButton(ns("confirmar"), "Confirmo este ajuste", class = "btn-default")))),
        if (length(propios)) fluidRow(
          column(8, selectInput(ns("ajuste_retirar"), "Retirar un ajuste tuyo", choices = propios, width = "100%")),
          column(4, tags$div(style = "margin-top:25px", actionButton(ns("retirar"), "Retirar mi ajuste", class = "btn-default")))),
        tags$p(class = "ev-nota", "Con ", UMBRAL_CONFIRMACIONES, " confirmaciones de personas distintas al autor, el ajuste se marca como validado. ",
               "Confirma solo lo que puedas respaldar con tu experiencia o una fuente."))
    })
    observeEvent(input$confirmar, {
      req(input$ajuste_confirmar, nzchar(usuario()))
      tryCatch({ registrar_confirmacion(almacen, sistema_id(), usuario(), input$ajuste_confirmar)
                 showNotification("Confirmación registrada.", type = "message"); refresco(refresco() + 1) },
               error = function(e) showNotification(paste("No se pudo guardar:", conditionMessage(e)), type = "error", duration = NULL))
    })
    observeEvent(input$retirar, {
      req(input$ajuste_retirar, nzchar(usuario()))
      tryCatch({ registrar_retiro(almacen, sistema_id(), usuario(), input$ajuste_retirar)
                 showNotification("Ajuste retirado (queda en el historial).", type = "message"); refresco(refresco() + 1) },
               error = function(e) showNotification(paste("No se pudo guardar:", conditionMessage(e)), type = "error", duration = NULL))
    })

    list(ajustes = ajustes_sistema)
  })
}
