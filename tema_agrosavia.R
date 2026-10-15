# =====================================================================
# Tema de marca AGROSAVIA para la app Shiny del Observatorio de costos
# Fuente: Brand book y Manual de identidad visual de marca (feb. 2025)
#   - Colores: Verde AGRO #009F3C y Azul SAVIA #004F9F (págs. 33 y 36); paleta
#     secundaria de apoyo (pág. 37). El manual pide no variar estas referencias.
#   - Tipografía: KievitOT (corporativa); alterna Myriad Pro; en documentos y
#     presentaciones, Arial (pág. 38). KievitOT y Myriad Pro son fuentes con
#     licencia: la app las usa si el equipo del visitante las tiene y, si no,
#     cae en Arial. No se suben archivos de fuente al repositorio.
#   - Logo: logotipo con descriptor, sin alterar. Tamaño mínimo digital: 40 px
#     de ancho sin descriptor y 85 px con descriptor (pág. 29). Respetar el
#     área de reserva: no poner texto ni elementos pegados al logo.
#   - Las tramas de verde y azul (10 % a 100 %) son solo para fondos, texturas y
#     texto secundario, nunca sobre el logotipo (pág. 36).
#
# NOTA DE ACCESIBILIDAD (contraste sobre blanco, calculado):
#   Verde AGRO 3,5:1 (solo para bandas, botones y texto grande o en negrita);
#   Verde 356 (#007C3A) 5,3:1 (texto pequeño y enlaces); Azul SAVIA 8,0:1;
#   Naranja 7579 3,7:1 (solo texto grande o en negrita, nunca el único indicador).
#
# ESTADO: NO PROBADO EN R. Requiere: shiny, bslib, htmltools (ggplot2 opcional).
# USO
#   source("tema_agrosavia.R")
#   ui <- fluidPage(theme = tema_agrosavia(), encabezado_agrosavia("Título", "Subtítulo"),
#                   ..., pie_agrosavia())
#   El logo debe estar en www/logo_agrosavia.png (ya viene en la carpeta).
# =====================================================================
library(bslib); library(htmltools)

COLORES_AGROSAVIA <- list(
  verde_agro  = "#009F3C",   # Pantone 355 C
  azul_savia  = "#004F9F",   # Pantone 293 C
  verde_texto = "#007C3A",   # Pantone 356 C: texto pequeño y enlaces sobre fondo claro
  tinta       = "#1D1D1B",   # texto corriente (casi negro, como el descriptor del logo)
  # secundaria (inspiración del campo)
  verde_pasto = "#95C11F", rojo_fruta = "#E30613", amarillo_sol = "#FFED00",
  azul_cielo  = "#0067B1", azul_agua  = "#00B1EB", tierra = "#68380F",
  naranja     = "#E9531B"
)

# Paleta para gráficos de categorías (máximo 8): primero los dos colores de marca
PALETA_CATEGORICA <- unname(unlist(COLORES_AGROSAVIA[c(
  "verde_agro", "azul_savia", "naranja", "azul_agua", "verde_pasto", "tierra",
  "rojo_fruta", "azul_cielo")]))

# Trama de un color de marca al p % (10 a 100), mezclada con blanco
trama_marca <- function(hex, p) {
  rgb0 <- grDevices::col2rgb(hex)[, 1]
  m <- round(255 - (255 - rgb0) * p / 100)
  sprintf("#%02X%02X%02X", m[1], m[2], m[3])
}

CSS_AGROSAVIA <- "
:root{--ag-verde:#009F3C;--ag-azul:#004F9F;--ag-verde-txt:#007C3A;--ag-tinta:#1D1D1B}
body{color:var(--ag-tinta)}
a{color:var(--ag-azul)}
a:hover{color:var(--ag-verde-txt)}
h1,h2,h3,h4{color:var(--ag-verde-txt);font-weight:600}
/* Cabecera: logo a la izquierda y banda azul inclinada con el título (como en el manual) */
.ag-cabecera{display:flex;align-items:stretch;gap:0;margin:0 -15px 18px -15px;background:#fff}
.ag-logo{padding:14px 22px;display:flex;align-items:center;flex:0 0 auto}
.ag-logo img{height:56px;width:auto;display:block}
.ag-titulo{flex:1 1 auto;background:var(--ag-azul);color:#fff;padding:14px 40px 14px 34px;
  clip-path:polygon(28px 0,100% 0,100% 100%,0 100%);display:flex;flex-direction:column;justify-content:center}
.ag-titulo .ag-t1{font-size:1.35rem;font-weight:700;line-height:1.2;color:#fff}
.ag-titulo .ag-t2{font-size:.95rem;font-weight:400;color:#fff;opacity:.95}
@media (max-width:700px){.ag-cabecera{flex-direction:column}
  .ag-titulo{clip-path:none;padding:12px 18px}}
/* Pie: banda verde con el logotipo en blanco no disponible; se usa texto */
.ag-pie{background:var(--ag-verde);color:#fff;margin:28px -15px 0 -15px;padding:14px 24px;
  font-size:1rem;font-weight:600;display:flex;flex-wrap:wrap;gap:6px 24px;justify-content:space-between}
.ag-pie a{color:#fff;text-decoration:underline}
/* Controles */
.btn-primary,.btn-default.active{background:var(--ag-verde);border-color:var(--ag-verde);font-weight:600}
.btn-primary:hover{background:var(--ag-verde-txt);border-color:var(--ag-verde-txt)}
.btn-default{border-color:var(--ag-azul);color:var(--ag-azul)}
.btn-default:hover{background:var(--ag-azul);color:#fff}
.form-control:focus,.selectize-input.focus{border-color:var(--ag-verde);box-shadow:0 0 0 .2rem rgba(0,159,60,.25)}
.irs--shiny .irs-bar{background:var(--ag-verde);border-color:var(--ag-verde)}
.irs--shiny .irs-handle{border-color:var(--ag-azul)}
.nav-tabs .nav-link.active,.nav-pills .nav-link.active{background:var(--ag-azul);color:#fff;border-color:var(--ag-azul)}
.nav-tabs .nav-link{color:var(--ag-azul);font-weight:600}
"

tema_agrosavia <- function() {
  bs_theme(
    version = 5,
    bg = "#FFFFFF", fg = COLORES_AGROSAVIA$tinta,
    primary = COLORES_AGROSAVIA$verde_agro,
    secondary = COLORES_AGROSAVIA$azul_savia,
    success = COLORES_AGROSAVIA$verde_texto,
    info = COLORES_AGROSAVIA$azul_agua,
    warning = COLORES_AGROSAVIA$naranja,
    danger = COLORES_AGROSAVIA$rojo_fruta,
    base_font = c("KievitOT", "Kievit OT", "Myriad Pro", "Arial", "sans-serif"),
    heading_font = c("KievitOT", "Kievit OT", "Myriad Pro", "Arial", "sans-serif")
  ) |> bs_add_rules(CSS_AGROSAVIA)
}

encabezado_agrosavia <- function(titulo, subtitulo = NULL, logo = "logo_agrosavia.png") {
  tags$header(class = "ag-cabecera",
    tags$div(class = "ag-logo",
      tags$img(src = logo, alt = "AGROSAVIA, Corporación colombiana de investigación agropecuaria")),
    tags$div(class = "ag-titulo",
      tags$div(class = "ag-t1", titulo),
      if (!is.null(subtitulo)) tags$div(class = "ag-t2", subtitulo)))
}

pie_agrosavia <- function(texto = "Observatorio de costos de producción agropecuarios de Colombia · piloto") {
  tags$footer(class = "ag-pie",
    tags$span(texto),
    tags$a(href = "https://www.agrosavia.co", target = "_blank", rel = "noopener noreferrer", "www.agrosavia.co"))
}

# Tema de ggplot2 (opcional): fondo blanco, títulos en verde de texto, paleta de marca
theme_agrosavia <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "sans", colour = COLORES_AGROSAVIA$tinta),
      plot.title = ggplot2::element_text(colour = COLORES_AGROSAVIA$verde_texto, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom")
}
scale_fill_agrosavia   <- function(...) ggplot2::scale_fill_manual(values = PALETA_CATEGORICA, ...)
scale_colour_agrosavia <- function(...) ggplot2::scale_colour_manual(values = PALETA_CATEGORICA, ...)
