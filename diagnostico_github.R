# =====================================================================
# Diagnóstico de la conexión con GitHub (independiente de la app).
# 1. Escribe abajo el repositorio y el token. 2. Selecciona todo y ejecuta (Ctrl+Enter).
# NO subas este archivo a GitHub con el token escrito.
# ESTADO: NO PROBADO EN R.
# =====================================================================
if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2")

REPO  <- "AGROSAVIA/observatorio-ediciones"   # organización/repositorio
TOKEN <- "PEGA-AQUI-EL-TOKEN"                 # token de grano fino
RAMA  <- "main"

diagnosticar_github <- function(repo = Sys.getenv("EDICIONES_REPO"), token = Sys.getenv("GITHUB_PAT"),
                                rama = Sys.getenv("EDICIONES_RAMA", "main")) {
  repo <- trimws(repo); token <- trimws(token); rama <- trimws(rama); if (!nzchar(rama)) rama <- "main"
  pedir <- function(ruta) httr2::request(paste0("https://api.github.com/", ruta)) |>
    httr2::req_headers(Authorization = paste("Bearer", token), Accept = "application/vnd.github+json",
                       `X-GitHub-Api-Version` = "2022-11-28", `User-Agent` = "observatorio-costos-agropecuarios") |>
    httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
  say <- function(...) message(...)
  say("Repositorio: [", repo, "]  Rama: [", rama, "]  Token: ", if (nzchar(token)) paste0(nchar(token), " caracteres") else "VACÍO")
  r <- pedir("user"); s <- httr2::resp_status(r)
  if (s == 200L) say("1. Token válido (usuario: ", httr2::resp_body_json(r)$login, ").")
  else { say("1. FALLA (", s, "): el token no es válido o venció. Crea uno nuevo y pégalo sin espacios."); return(invisible(FALSE)) }
  r <- pedir(paste0("repos/", repo)); s <- httr2::resp_status(r)
  if (s == 200L) {
    b <- httr2::resp_body_json(r)
    say("2. El token ve el repositorio (privado: ", b$private, "; puede escribir: ", isTRUE(b$permissions$push), ").")
    if (!isTRUE(b$permissions$push)) say("   OJO: no tiene permiso de escritura. Revisa 'Contents: Read and write'.")
  } else {
    say("2. FALLA (", s, "): el token NO ve el repositorio. Causas típicas: (a) el nombre está mal escrito ",
        "(debe ser organizacion/repositorio, sin espacios); (b) el token se creó con 'Resource owner' = tu cuenta ",
        "personal y el repositorio es de una organización: créalo de nuevo eligiendo la organización como dueña; ",
        "(c) el token no incluye este repositorio en 'Repository access'; (d) la organización exige aprobar los tokens.")
    return(invisible(FALSE))
  }
  r <- pedir(paste0("repos/", repo, "/branches/", rama)); s <- httr2::resp_status(r)
  if (s == 200L) say("3. La rama '", rama, "' existe.")
  else { say("3. FALLA (", s, "): la rama '", rama, "' no existe. Si el repositorio está vacío, crea un archivo ",
             "(p. ej. un README) desde la web de GitHub y revisa el nombre de la rama."); return(invisible(FALSE)) }
  say("Todo en orden. Ahora prueba: probar_almacen(almacen_desde_entorno())")
  invisible(TRUE)
}

diagnosticar_github(REPO, TOKEN, RAMA)
