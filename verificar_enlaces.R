# =====================================================================
# Verifica que los enlaces de datos_linea_base/enlaces_contraste.csv abran
# desde TU equipo (red, país y navegador cuentan: algunos sitios bloquean
# solicitudes automáticas o desde fuera de Colombia).
# ESTADO: NO PROBADO EN R. Requiere: httr2, readr, dplyr, purrr.
# USO: setwd("carpeta de la app"); source("verificar_enlaces.R")
#   Escribe enlaces_verificados.csv con el código HTTP y el tipo de contenido.
#   Un enlace sano da 200 y content-type application/pdf (o text/html si es una página).
# =====================================================================
library(httr2); library(readr); library(dplyr); library(purrr)

enlaces <- read_csv("datos_linea_base/enlaces_contraste.csv", show_col_types = FALSE, na = c("", "NA"))
if (!"url_alterno" %in% names(enlaces)) enlaces$url_alterno <- NA_character_

probar <- function(url) {
  if (is.na(url)) return(tibble(estado = NA_integer_, tipo = NA_character_, detalle = NA_character_))
  r <- tryCatch(
    request(url) |>
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36") |>
      req_headers(Range = "bytes=0-1023") |>       # solo los primeros bytes: no baja el PDF completo
      req_timeout(30) |> req_error(is_error = \(x) FALSE) |> req_perform(),
    error = function(e) conditionMessage(e))
  if (is.character(r)) return(tibble(estado = NA_integer_, tipo = NA_character_, detalle = substr(r, 1, 120)))
  tibble(estado = resp_status(r), tipo = resp_content_type(r), detalle = resp_status_desc(r))
}

resultado <- bind_rows(
  enlaces |> transmute(sistema_id, entidad, cual = "url",        url = url),
  enlaces |> filter(!is.na(url_alterno)) |> transmute(sistema_id, entidad, cual = "url_alterno", url = url_alterno)) |>
  distinct(sistema_id, cual, url, .keep_all = TRUE)

resultado <- bind_cols(resultado, map_dfr(resultado$url, probar)) |>
  mutate(sano = !is.na(estado) & estado %in% c(200, 206))

write_csv(resultado, "enlaces_verificados.csv")
print(resultado |> select(sistema_id, cual, estado, tipo, sano), n = Inf)
cat("\nEnlaces que NO abrieron:", sum(!resultado$sano), "de", nrow(resultado), "\n")
