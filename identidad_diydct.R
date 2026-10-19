# =====================================================================
# Identidad visual del Observatorio: logo del Departamento de Inteligencia y
# Divulgación Científica y Tecnológica (DIyDCT), créditos de autoría y doña Violeta
# como guía para productores. Se apoya en tema_agrosavia.R, que NO se modifica:
# la paleta, la tipografía y el logo de AGROSAVIA siguen el Brand book (feb. 2025).
#
# Reglas de marca que se respetan aquí:
#   - El logo de AGROSAVIA va a la izquierda, sin alterar y sobre fondo blanco.
#   - El logo del DIyDCT va sobre fondo blanco, sin tramas ni texto pegado.
#   - Los fondos usan solo tramas de azul SAVIA y verde AGRO (10 % a 100 %).
#   - El morado de doña Violeta (su bufanda y su lazo) aparece únicamente como
#     filete de acento junto al personaje; no es un color de la paleta de la app.
#
# Activos en www/: logo_diydct.png, favicon.png y violeta_{lupa,duda,saludo,
# siembra,sombrero}.png (tomados del documento «Personajes del Departamento IyDCT»).
#
# ESTADO: NO PROBADO EN R. Requiere: shiny, htmltools. Uso (ver app.R):
#   source("tema_agrosavia.R"); source("identidad_diydct.R")
#   theme = tema_agrosavia() |> bslib::bs_add_rules(CSS_DIYDCT)
# =====================================================================
library(htmltools)

CREDITOS <- list(
  autor        = "Diego Hernando Flórez Martínez, PhD.",
  orcid        = "0000-0002-3904-9543",
  orcid_url    = "https://orcid.org/0000-0002-3904-9543",
  departamento = "Departamento de Inteligencia y Divulgación Científica y Tecnológica",
  direccion    = "Dirección de Investigación y Desarrollo",
  institucion  = "AGROSAVIA",
  anio         = "2026"
)

CSS_DIYDCT <- "
/* Cabecera con tres piezas: logo AGROSAVIA | banda azul con título | logo DIyDCT */
.ag-logo-dep{padding:10px 22px;display:flex;align-items:center;flex:0 0 auto;background:#fff}
.ag-logo-dep img{height:80px;width:auto;display:block}
.ag-titulo{padding-right:34px}
@media (max-width:700px){
  .ag-logo-dep{justify-content:center;border-top:1px solid #E6EDF5}
  .ag-logo-dep img{height:60px}}

/* Doña Violeta: fondo en trama de azul SAVIA al 10 %, filete morado de acento */
.violeta-hero{display:flex;align-items:center;gap:24px;background:#E6EDF5;
  border-left:8px solid #7A4A8C;border-radius:14px;padding:16px 24px;margin:0 0 18px 0}
.violeta-hero .vh-img{flex:0 0 auto}
.violeta-hero .vh-img img{height:170px;width:auto;display:block}
.violeta-hero h2{margin:0 0 6px 0;font-size:1.45rem}
.violeta-hero p{margin:0 0 8px 0;font-size:1.05rem}
.violeta-hero details{margin-top:8px}
.violeta-hero summary{cursor:pointer;font-weight:700;color:var(--ag-azul)}
.violeta-hero summary:hover{color:var(--ag-verde-txt)}
.violeta-hero ol,.violeta-hero dl{margin:8px 0 4px 0}
.violeta-hero dt{font-weight:700;color:var(--ag-verde-txt)}
.violeta-hero dd{margin:0 0 6px 0}
.violeta-hero .vh-cols{display:flex;gap:28px;flex-wrap:wrap}
.violeta-hero .vh-cols>div{flex:1 1 280px}
@media (max-width:700px){.violeta-hero{flex-direction:column;text-align:left}
  .violeta-hero .vh-img img{height:130px}}

.violeta-nota{display:flex;align-items:center;gap:16px;background:#E6EDF5;
  border-left:6px solid #7A4A8C;border-radius:12px;padding:10px 18px;margin:0 0 16px 0}
.violeta-nota img{height:92px;width:auto;flex:0 0 auto}
.violeta-nota .vn-t{font-weight:700;color:var(--ag-verde-txt);margin:0 0 2px 0}
.violeta-nota p{margin:0}

/* Pie con créditos */
.ag-pie.ag-pie-creditos{flex-direction:column;align-items:stretch;gap:6px}
.ag-pie .ag-pie-fila{display:flex;flex-wrap:wrap;gap:4px 24px;justify-content:space-between}
.ag-pie .ag-pie-cred{font-weight:400;font-size:.92rem}

/* Pestaña Créditos */
.cred-wrap{display:flex;gap:28px;flex-wrap:wrap;align-items:flex-start}
.cred-wrap .cred-txt{flex:1 1 420px}
.cred-wrap .cred-img{flex:0 0 auto;text-align:center}
.cred-wrap .cred-img img{display:block;margin:0 auto 14px auto}
.cred-wrap h4{margin-top:18px}
.cred-cita{background:#E6EDF5;border-left:6px solid var(--ag-azul);padding:10px 16px;border-radius:8px}
"

# ---- Cabecera: logo AGROSAVIA, banda azul con el título y logo del DIyDCT ----
encabezado_observatorio <- function(titulo, subtitulo = NULL,
                                    logo = "logo_agrosavia.png", logo_dep = "logo_diydct.png") {
  tags$header(class = "ag-cabecera",
    tags$div(class = "ag-logo",
      tags$img(src = logo, alt = "AGROSAVIA, Corporación colombiana de investigación agropecuaria")),
    tags$div(class = "ag-titulo",
      tags$div(class = "ag-t1", titulo),
      if (!is.null(subtitulo)) tags$div(class = "ag-t2", subtitulo)),
    tags$div(class = "ag-logo-dep",
      tags$img(src = logo_dep, alt = paste0("Logo del ", CREDITOS$departamento))))
}

# ---- Doña Violeta: bienvenida y guía rápida para productores ----
violeta_hero <- function() {
  tags$section(class = "violeta-hero", `aria-label` = "Guía para productores con doña Violeta",
    tags$div(class = "vh-img",
      tags$img(src = "violeta_lupa.png",
               alt = "Doña Violeta, una cuy con sombrero de paja y bufanda morada, sostiene una lupa y una ramita")),
    tags$div(class = "vh-txt",
      tags$h2("¿Cuánto cuesta producir? Miremos juntos"),
      tags$p("Soy doña Violeta. Aquí encuentra en qué se va la plata al producir en su sistema, según las guías ",
             "de Finagro y otras fuentes. Son valores de referencia: sirven para comparar con sus propias cuentas, ",
             "no reemplazan el costo real de su finca."),
      tags$details(
        tags$summary("Guía rápida y palabras del campo"),
        tags$div(class = "vh-cols",
          tags$div(
            tags$h3(class = "h6", "Cómo usarlo, paso a paso"),
            tags$ol(
              tags$li("Escoja su sistema productivo en la lista de abajo."),
              tags$li("En «Línea base» mire en qué se va el costo: mano de obra, insumos y cada labor."),
              tags$li("Compare con sus cuentas, en la misma unidad de la guía, y con «Otras fuentes»."),
              tags$li("Si su finca es distinta, vaya a «Ajustes y edición» y cuéntenos su valor con una justificación."))),
          tags$div(
            tags$h3(class = "h6", "Palabras del campo"),
            tags$dl(
              tags$dt("Rubro"),
              tags$dd("Cada labor o gasto en que se divide el costo: fertilización, control de arvenses (malezas), cosecha…"),
              tags$dt("Mano de obra e insumos"),
              tags$dd("Mano de obra es el trabajo de las personas; insumos es lo que se compra para producir (fertilizantes, semillas, alimento, medicinas)."),
              tags$dt("Ciclo"),
              tags$dd("Lo que dura una cosecha o una producción. En cultivos de varios años, la guía suma los costos de toda la vida productiva."),
              tags$dt("Margen"),
              tags$dd("Lo que queda de cada peso vendido después de pagar los costos: (precio − costo) / precio."),
              tags$dt("Lo que no cuenta la guía"),
              tags$dd("Tierra, impuestos y gastos administrativos y financieros.")))))))
}

# ---- Nota breve de doña Violeta al inicio de cada pestaña ----
violeta_nota <- function(pose = c("lupa", "duda", "saludo", "siembra"), titulo, texto) {
  pose <- match.arg(pose)
  alt <- switch(pose,
    lupa    = "Doña Violeta con una lupa",
    duda    = "Doña Violeta con la mano en el sombrero y un signo de interrogación",
    saludo  = "Doña Violeta, cuy con bufanda morada, saluda",
    siembra = "Doña Violeta sostiene una plántula")
  tags$aside(class = "violeta-nota",
    tags$img(src = paste0("violeta_", pose, ".png"), alt = alt),
    tags$div(tags$p(class = "vn-t", titulo), tags$p(texto)))
}

# ---- Pie: marca AGROSAVIA + créditos de diseño y conceptualización ----
pie_creditos <- function(texto = "Observatorio de costos de producción agropecuarios de Colombia · piloto") {
  tags$footer(class = "ag-pie ag-pie-creditos",
    tags$div(class = "ag-pie-fila",
      tags$span(texto),
      tags$a(href = "https://www.agrosavia.co", target = "_blank", rel = "noopener noreferrer", "www.agrosavia.co")),
    tags$div(class = "ag-pie-cred",
      "Conceptualización y diseño: ", CREDITOS$autor, " · ORCID ",
      tags$a(href = CREDITOS$orcid_url, target = "_blank", rel = "noopener noreferrer", CREDITOS$orcid),
      " · ", CREDITOS$departamento, ", ", CREDITOS$direccion, ", ", CREDITOS$institucion, "."))
}

# ---- Pestaña «Créditos» ----
pagina_creditos <- function() {
  tags$div(class = "cred-wrap",
    tags$div(class = "cred-txt",
      tags$h4("Conceptualización y diseño"),
      tags$p(tags$strong(CREDITOS$autor), tags$br(),
             "ORCID: ", tags$a(href = CREDITOS$orcid_url, target = "_blank", rel = "noopener noreferrer", CREDITOS$orcid), tags$br(),
             CREDITOS$departamento, tags$br(),
             CREDITOS$direccion, ", ", CREDITOS$institucion),
      tags$h4("Personaje"),
      tags$p("Doña Violeta es uno de los personajes del ", CREDITOS$departamento,
             ": la voz de la conciencia ambiental, social y comunitaria. En el Observatorio acompaña a los ",
             "productores para leer los costos de producción."),
      tags$h4("Datos"),
      tags$p("La línea base proviene de las guías de Finagro (Marcos de Referencia Agroeconómicos). ",
             "Las demás fuentes, sus licencias y el estado del permiso de uso están en «Línea base › Otras fuentes». ",
             "El Observatorio es una herramienta de investigación de AGROSAVIA, sin fines comerciales."),
      tags$h4("Cómo citar"),
      tags$p(class = "cred-cita",
        "Flórez Martínez, D. H. (", CREDITOS$anio, "). ",
        tags$em("Observatorio de costos de producción agropecuarios de Colombia"),
        " (versión beta) [Aplicación Shiny]. ", CREDITOS$departamento, ", ", CREDITOS$institucion, ".")),
    tags$div(class = "cred-img",
      tags$img(src = "logo_diydct.png", height = "150",
               alt = paste0("Logo del ", CREDITOS$departamento)),
      tags$img(src = "violeta_siembra.png", height = "190",
               alt = "Doña Violeta sostiene una plántula entre el pasto")))
}
