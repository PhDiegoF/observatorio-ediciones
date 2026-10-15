# =====================================================================
# Almacén de ajustes de los usuarios (local o GitHub)
# Observatorio de costos de producción agropecuarios de Colombia (beta)
#
# ESTADO: NO PROBADO EN R (no hay R ni acceso a GitHub en mi entorno).
#
# DISEÑO
#   - Solo se AGREGAN archivos, nunca se edita uno existente: cada guardado crea un
#     archivo JSON nuevo con nombre único. Así dos usuarios guardando a la vez no se
#     pisan (GitHub devuelve error 409 solo cuando se actualiza un archivo existente).
#   - Tres tipos de registro: "ajuste" (uno o varios cambios con una justificación),
#     "confirmacion" (otro usuario respalda un ajuste) y "retiro" (el autor lo retira).
#   - Estructura: <carpeta>/<sistema_id>/<id>.json
#   - El estado vigente se calcula al leer: gana el ajuste más reciente no retirado
#     por cada (guía, momento, campo). Los ajustes se muestran rotulados junto a la
#     línea base; nunca la reemplazan.
#
# ALMACENES
#   almacen_local(dir)    carpeta del equipo: sirve para probar; en Posit Connect Cloud
#                         el disco es efímero y se pierde.
#   almacen_github(repo)  repositorio de GitHub (recomendado: UN REPOSITORIO APARTE y
#                         PRIVADO solo para las ediciones). Motivos: (1) Connect Cloud
#                         vuelve a publicar la app con cada commit a la rama conectada,
#                         así que guardar ediciones en el repositorio de la app la
#                         reiniciaría cada vez; (2) los correos de los usuarios no deben
#                         quedar en un repositorio público.
#   Variables de entorno (en Connect Cloud: sección Variables del contenido):
#     EDICIONES_REPO   p. ej. "mi-organizacion/observatorio-ediciones"
#     GITHUB_PAT       token de grano fino con acceso SOLO a ese repositorio y permiso
#                      "Contents: Read and write"
#     EDICIONES_RAMA   opcional, por defecto "main"
#   almacen_desde_entorno() elige GitHub si están EDICIONES_REPO y GITHUB_PAT; si no, local.
#
# LÍMITES CONOCIDOS
#   - La API de GitHub lista como máximo 1.000 archivos por carpeta: por eso hay una
#     carpeta por sistema. Si un sistema llegara a ese tope, habría que partir por mes.
#   - Cada ajuste leído es una petición; los archivos no cambian, así que se guardan en
#     memoria y solo se piden los nuevos.
#   - El token vence (máximo un año): hay que renovarlo y actualizar la variable.
#   - Quien pueda abrir la app puede escribir ajustes con su identidad de cuenta; la app
#     no verifica la calidad de lo que escriben.
#
# PAQUETES: jsonlite, dplyr, tibble, readr, httr2 (solo para GitHub)
# =====================================================================
library(dplyr); library(tibble); library(readr)   # jsonlite se usa con prefijo: su validate() taparía al de shiny

LINEA_BASE_VERSION <- "Finagro MRA (descarga 2026-10)"
UMBRAL_CONFIRMACIONES <- 2   # propuesto; la regla definitiva está pendiente (T6)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
.nz <- function(x, por) if (is.null(x) || length(x) == 0 || all(is.na(x))) por else x

nuevo_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"), "-",
         sprintf("%08x", sample.int(.Machine$integer.max, 1)))
}
ahora_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
a_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                          digits = NA, pretty = TRUE))

# ---------------------------------------------------------------------
# Almacén local
# ---------------------------------------------------------------------
almacen_local <- function(dir = "ediciones_local") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  list(
    tipo = "local", persistente = FALSE,
    descripcion = paste("carpeta local", normalizePath(dir, mustWork = FALSE)),
    guardar = function(rec) {
      carpeta <- file.path(dir, rec$sistema_id)
      dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
      writeLines(enc2utf8(a_json(rec)), file.path(carpeta, paste0(rec$id, ".json")), useBytes = TRUE)
      invisible(rec$id)
    },
    leer = function(sistema_id) {
      f <- list.files(file.path(dir, sistema_id), pattern = "\\.json$", full.names = TRUE)
      lapply(f, function(x) jsonlite::fromJSON(read_file(x), simplifyVector = TRUE))
    }
  )
}

# ---------------------------------------------------------------------
# Almacén en GitHub (API de contenidos)
# ---------------------------------------------------------------------
almacen_github <- function(repo, token = Sys.getenv("GITHUB_PAT"), rama = "main", carpeta = "ediciones") {
  stopifnot(nzchar(repo), nzchar(token))
  cache <- new.env(parent = emptyenv())   # sha del archivo -> registro ya leído
  pedir <- function(ruta, accept = "application/vnd.github+json") {
    httr2::request(paste0("https://api.github.com/repos/", repo, "/", ruta)) |>
      httr2::req_headers(Authorization = paste("Bearer", token), Accept = accept,
                         `X-GitHub-Api-Version` = "2022-11-28",
                         `User-Agent` = "observatorio-costos-agropecuarios") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_retry(max_tries = 3,
                       is_transient = function(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L))
  }
  falla <- function(r, que) stop(que, ": GitHub respondió ", httr2::resp_status(r), ". ",
                                 substr(httr2::resp_body_string(r), 1, 300), call. = FALSE)
  list(
    tipo = "github", persistente = TRUE,
    descripcion = paste0("GitHub, repositorio ", repo, " (rama ", rama, ")"),
    guardar = function(rec) {
      cuerpo <- list(message = paste("ajuste", rec$tipo, rec$sistema_id, rec$id),
                     content = jsonlite::base64_enc(charToRaw(enc2utf8(a_json(rec)))), branch = rama)
      r <- pedir(paste0("contents/", carpeta, "/", rec$sistema_id, "/", rec$id, ".json")) |>
        httr2::req_method("PUT") |> httr2::req_body_json(cuerpo) |> httr2::req_perform()
      if (!(httr2::resp_status(r) %in% c(200L, 201L))) falla(r, "No se pudo guardar el ajuste")
      invisible(rec$id)
    },
    leer = function(sistema_id) {
      r <- pedir(paste0("contents/", carpeta, "/", sistema_id, "?ref=", rama)) |> httr2::req_perform()
      if (httr2::resp_status(r) == 404L) return(list())   # aún no hay ajustes de ese sistema
      if (httr2::resp_status(r) != 200L) falla(r, "No se pudo leer la lista de ajustes")
      res <- lapply(httr2::resp_body_json(r), function(f) {
        if (!identical(f$type, "file") || !grepl("\\.json$", f$name)) return(NULL)
        if (is.null(cache[[f$sha]])) {
          r2 <- pedir(paste0("contents/", f$path, "?ref=", rama),
                      accept = "application/vnd.github.raw+json") |> httr2::req_perform()
          if (httr2::resp_status(r2) != 200L) return(NULL)
          cache[[f$sha]] <- jsonlite::fromJSON(httr2::resp_body_string(r2), simplifyVector = TRUE)
        }
        cache[[f$sha]]
      })
      Filter(Negate(is.null), res)
    }
  )
}

almacen_desde_entorno <- function(dir_local = "ediciones_local") {
  repo <- Sys.getenv("EDICIONES_REPO"); tok <- Sys.getenv("GITHUB_PAT")
  rama <- Sys.getenv("EDICIONES_RAMA", "main")
  if (nzchar(repo) && nzchar(tok)) almacen_github(repo, tok, rama) else almacen_local(dir_local)
}

# Prueba de conexión: escribe y lee un registro de prueba (sistema "_prueba").
# Úsala una vez al configurar el token:  probar_almacen(almacen_desde_entorno())
probar_almacen <- function(alm) {
  id <- registrar_ajuste(alm, "_prueba", "prueba@ejemplo.org", "Registro de prueba de conexión", "",
                         tibble(slug = "prueba", momento = "actualizado", campo = "param:prueba",
                                valor_base = 1, valor_propuesto = 2, unidad = "u"))
  r <- alm$leer("_prueba")
  ids <- vapply(r, function(x) as.character(x$id), "")
  message("Almacén: ", alm$descripcion, ". Registros de prueba leídos: ", length(ids),
          ". ¿Se leyó el que acabo de escribir? ", id %in% ids)
  invisible(id %in% ids)
}

# ---------------------------------------------------------------------
# Escribir registros
# ---------------------------------------------------------------------
# cambios: data.frame con slug, momento, campo, valor_base, valor_propuesto, unidad.
#   campo = "rubro:<nombre del rubro>" o "param:<produccion_t_ha_ciclo|valor_jornal|precio_venta>"
registrar_ajuste <- function(alm, sistema_id, usuario, justificacion, fuente, cambios) {
  rec <- list(id = nuevo_id(), tipo = "ajuste", sistema_id = sistema_id,
              usuario = tolower(trimws(usuario)), fecha = ahora_utc(),
              justificacion = trimws(justificacion), fuente = trimws(.nz(fuente, "")),
              linea_base = LINEA_BASE_VERSION, cambios = as.data.frame(cambios))
  alm$guardar(rec)
}
registrar_confirmacion <- function(alm, sistema_id, usuario, referencia) {
  alm$guardar(list(id = nuevo_id(), tipo = "confirmacion", sistema_id = sistema_id,
                   usuario = tolower(trimws(usuario)), fecha = ahora_utc(), referencia = referencia))
}
registrar_retiro <- function(alm, sistema_id, usuario, referencia) {
  alm$guardar(list(id = nuevo_id(), tipo = "retiro", sistema_id = sistema_id,
                   usuario = tolower(trimws(usuario)), fecha = ahora_utc(), referencia = referencia))
}

# ---------------------------------------------------------------------
# Leer y resumir: una fila por cambio, con confirmaciones y estado
# ---------------------------------------------------------------------
tabla_ajustes_vacia <- function() {
  tibble(ajuste_id = character(), usuario = character(), fecha = character(),
         justificacion = character(), fuente = character(), sistema_id = character(),
         slug = character(), momento = character(), campo = character(),
         valor_base = numeric(), valor_propuesto = numeric(), unidad = character(),
         n_confirmaciones = integer(), validado = logical(), retirado = logical(), vigente = logical())
}

procesar_ediciones <- function(registros, umbral = UMBRAL_CONFIRMACIONES) {
  if (length(registros) == 0) return(tabla_ajustes_vacia())
  tipo <- vapply(registros, function(r) as.character(.nz(r$tipo, "")), "")
  aj <- registros[tipo == "ajuste"]
  if (!length(aj)) return(tabla_ajustes_vacia())
  refs <- function(t) {
    x <- registros[tipo == t]
    if (!length(x)) return(tibble(referencia = character(), usuario = character()))
    bind_rows(lapply(x, function(r) tibble(referencia = as.character(r$referencia),
                                           usuario = tolower(as.character(r$usuario)))))
  }
  conf <- refs("confirmacion"); ret <- refs("retiro")
  filas <- bind_rows(lapply(aj, function(r) {
    cb <- as_tibble(r$cambios)
    if (!nrow(cb)) return(NULL)
    tibble(ajuste_id = as.character(r$id), usuario = tolower(as.character(r$usuario)),
           fecha = as.character(r$fecha), justificacion = as.character(.nz(r$justificacion, "")),
           fuente = as.character(.nz(r$fuente, "")), sistema_id = as.character(r$sistema_id),
           slug = as.character(cb$slug), momento = as.character(cb$momento),
           campo = as.character(cb$campo), valor_base = suppressWarnings(as.numeric(cb$valor_base)),
           valor_propuesto = suppressWarnings(as.numeric(cb$valor_propuesto)),
           unidad = as.character(cb$unidad))
  }))
  if (is.null(filas) || !nrow(filas)) return(tabla_ajustes_vacia())
  nconf <- filas |> distinct(ajuste_id, usuario) |>
    left_join(distinct(conf, referencia, u_conf = usuario), by = c("ajuste_id" = "referencia"),
              relationship = "many-to-many") |>
    filter(!is.na(u_conf), u_conf != usuario) |>
    group_by(ajuste_id) |> summarise(n_confirmaciones = n_distinct(u_conf), .groups = "drop")
  retirados <- filas |> distinct(ajuste_id, usuario) |>
    inner_join(distinct(ret, referencia, u_ret = usuario), by = c("ajuste_id" = "referencia"),
               relationship = "many-to-many") |>
    filter(u_ret == usuario) |> distinct(ajuste_id) |> mutate(retirado = TRUE)
  filas |>
    left_join(nconf, by = "ajuste_id") |> left_join(retirados, by = "ajuste_id") |>
    mutate(n_confirmaciones = coalesce(n_confirmaciones, 0L), retirado = coalesce(retirado, FALSE),
           validado = n_confirmaciones >= umbral) |>
    group_by(slug, momento, campo) |>
    mutate(ultimo = if (any(!retirado)) max(ajuste_id[!retirado]) else NA_character_,
           vigente = !retirado & ajuste_id == ultimo) |>
    ungroup() |> select(-ultimo) |> arrange(desc(ajuste_id))
}
