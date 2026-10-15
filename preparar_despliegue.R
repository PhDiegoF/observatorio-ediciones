# =====================================================================
# Prepara la app para subirla a GitHub y publicarla en Posit Connect Cloud.
# Ejecutar UNA vez, con la carpeta de la app como directorio de trabajo
# Descomprime primero el zip completo y abre ESTE archivo desde la carpeta descomprimida.
# ESTADO: NO PROBADO EN R.
# =====================================================================
# 0. Carpeta de trabajo: la de este script (la carpeta descomprimida de la app)
ruta <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "")
if (nzchar(ruta) && ruta != ".") setwd(ruta)
message("Carpeta de trabajo: ", getwd())
message("Archivos aquí: ", paste(list.files(), collapse = ", "))

paquetes <- c("shiny", "bslib", "dplyr", "tidyr", "tibble", "readr", "stringr", "ggplot2",
              "scales", "DT", "htmltools", "jsonlite", "httr2", "rsconnect")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)

# 1. Archivos necesarios
necesarios <- c("app.R", "tema_agrosavia.R", "almacen_ediciones.R", "mod_linea_base.R", "mod_edicion.R",
                "mod_evidencia.R", "www/logo_agrosavia.png",
                file.path("datos_linea_base", c("sistemas.csv", "guias.csv", "rubros.csv", "enlaces_contraste.csv")),
                file.path("evidencia_salida", c("evidencia.csv", "evidencia_sistema.csv", "resumen_evidencia_por_sistema.csv")))
ausentes <- necesarios[!file.exists(necesarios)]
if (length(ausentes))
  stop("Faltan archivos: ", paste(ausentes, collapse = ", "), "\nCarpeta de trabajo actual: ", getwd(),
       "\nDescomprime el zip completo y ejecuta este script desde esa carpeta (no desde dentro del zip, ",
       "ni desde otra app).", call. = FALSE)

# 2. Que todos los scripts se lean sin errores de sintaxis
for (f in list.files(pattern = "\\.R$")) invisible(parse(f))

# 3. Peso de los archivos (GitHub rechaza archivos de más de 100 MB; aquí el mayor es evidencia.csv)
tam <- file.info(necesarios)$size / 1e6
message("Archivo más pesado: ", necesarios[which.max(tam)], " (", round(max(tam), 1), " MB)")

# 4. Manifiesto para Connect Cloud (lista los paquetes y sus versiones)
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
message("Listo: sube TODA esta carpeta (incluido manifest.json) a un repositorio de GitHub.")
message("No subas el token: GITHUB_PAT va solo en las Variables de Connect Cloud.")
