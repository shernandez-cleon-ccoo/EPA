# -----------------------------------------------------------------------------
# app.R — Dashboard EPA (CCOO Castilla y León, pensado para cualquier
# territorio de referencia, no hardcodeado)
#
# Estructura:
#  - Filtros globales (periodo, territorio de referencia, territorio de
#    comparación -opcional-, sexo) viven aquí como reactiveValues y se pasan
#    a cada módulo; los filtros avanzados (nacionalidad, educación, sector...)
#    son locales a cada módulo y no aparecen aquí.
#  - Navegación por niveles (§34 del briefing): Nivel 1 lectura rápida,
#    Nivel 2 análisis, Nivel 3 exploración avanzada.
#  - Esta es la Oleada 1 mínima: solo el módulo de Cuadro de mando está
#    conectado; el resto de mod_*.R se añaden con el mismo patrón.
# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmltools)
library(sf)
library(mapSpain)
library(ggbump)
library(DT)
library(scales)

source("epa_helpers.R")       # cargar_trimestre, diccionarios, con_totales... (pendiente de repartir en R/)
source("R/stat_quality.R")
source("R/indicators.R")
source("R/ui_helpers.R")
source("R/mod_cuadro_mando.R")
source("R/mod_territorio.R")

DATA_DIR <- "data_agregada"

# -----------------------------------------------------------------------------
# Carga de datos agregados desde el repositorio de GitHub, NO desde disco
# local: el workflow "Actualizar tablas EPA" (GitHub Actions) hace commit
# diario de los .rds a data_agregada/ en el repo, y esta app descarga esos
# mismos ficheros directamente al arrancar. Así el dashboard siempre lee la
# última tabla publicada sin que nadie tenga que bajarla ni redesplegar la
# app a mano.
#
# Requiere que el repositorio sea PÚBLICO (raw.githubusercontent.com no pide
# autenticación). Si en algún momento el repo pasa a privado, esta función
# necesitará añadir una cabecera Authorization con un token (GITHUB_PAT)
# guardado como variable de entorno en el servidor donde corra la app, no en
# el código.
# -----------------------------------------------------------------------------

# TODO: rellenar con el owner/repo/rama reales antes de desplegar.
GITHUB_OWNER  <- "shernandez-cleon-ccoo"
GITHUB_REPO   <- "EPA"
GITHUB_BRANCH <- "main"
GITHUB_RAW_BASE <- sprintf(
  "https://raw.githubusercontent.com/%s/%s/%s/data_agregada",
  GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH
)

# Caché en disco de la sesión del proceso R (tempdir(): se limpia sola en
# cada reinicio de la app, que es justo cuando queremos volver a descargar
# para coger la tabla más reciente). Evita volver a descargar dentro de la
# misma ejecución del proceso si cargar_tabla() se llamara más de una vez
# para la misma tabla.
CACHE_DIR_REMOTO <- file.path(tempdir(), "epa_data_cache")
dir.create(CACHE_DIR_REMOTO, showWarnings = FALSE, recursive = TRUE)

#' Descarga un .rds publicado por el workflow y lo lee. Devuelve NULL (en
#' vez de fallar) si no hay red, el repo aún no tiene esa tabla, o cualquier
#' otro problema de descarga -- cargar_tabla() decide qué hacer con el NULL.
descargar_tabla_remota <- function(nombre) {
  url <- paste0(GITHUB_RAW_BASE, "/", nombre, ".rds")
  destino <- file.path(CACHE_DIR_REMOTO, paste0(nombre, ".rds"))
  ok <- tryCatch({
    utils::download.file(url, destino, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message(sprintf("No se pudo descargar %s desde GitHub: %s", nombre, conditionMessage(e)))
    FALSE
  })
  if (!ok || !file.exists(destino) || file.size(destino) == 0) return(NULL)
  tryCatch(readRDS(destino), error = function(e) NULL)
}

# Carga de datos agregados (una vez al arrancar la app). Intenta primero la
# versión publicada en GitHub; si falla (sin red, tabla aún no publicada,
# desarrollo local sin conexión...), cae a data_agregada/ en disco si existe
# -- nunca deja la app sin arrancar por un fallo de descarga.
cargar_tabla <- function(nombre) {
  remoto <- descargar_tabla_remota(nombre)
  if (!is.null(remoto)) return(remoto)
  
  ruta_local <- file.path(DATA_DIR, paste0(nombre, ".rds"))
  if (file.exists(ruta_local)) {
    message(sprintf("Usando copia local de %s (no se pudo descargar de GitHub)", nombre))
    return(readRDS(ruta_local))
  }
  
  message(sprintf("No hay datos disponibles para %s (ni remotos ni locales)", nombre))
  NULL
}

epa_edad_df <- cargar_tabla("epa_edad")
epa_prov_df <- cargar_tabla("epa_prov")
# epa_form_df / epa_nac_df / epa_sector_df / epa_ocup_df / epa_paro_larga_df
# se cargan igual, cuando se conecten los módulos correspondientes (Nivel 2).
# No se cargan aquí para no hacer el skeleton más pesado de lo necesario.

# -----------------------------------------------------------------------------
# Relleno "al vuelo" del último trimestre si el INE ya lo publicó pero el cron
# diario (GitHub Actions -> build_epa_tablas.R) todavía no ha corrido para
# agregarlo a data_agregada/. Sin esto, tras la publicación del INE la app se
# quedaría hasta el siguiente cron (o más, si el cron falla ese día) mostrando
# el penúltimo trimestre.
#
# Compara el último trimestre YA publicado por el INE (detectar_ultimo_trimestre(),
# que hace HEAD contra el FTP del INE -- rápido, no descarga nada) frente al
# último `periodo` que traen las tablas recién descargadas de GitHub. Si el de
# GitHub se ha quedado atrás, descarga y agrega ESE ÚNICO trimestre en caliente
# con las mismas funciones que usa build_epa_tablas.R (cargar_trimestre() +
# calcular_tablas_trimestre()), y lo añade en memoria a las tablas ya cargadas
# -- NO lo escribe en disco ni en GitHub; eso lo sigue haciendo solo el cron
# cuando corra. Por tanto esto se repite cada vez que arranca el proceso R
# (cada reinicio de la app) mientras el cron no se ponga al día.
#
# Nunca lanza error hacia arriba: si el INE no responde, si el trimestre nuevo
# aún no tiene el zip de microdatos completo, etc., la app se queda con lo que
# ya trajo cargar_tabla() (los datos del cron) y solo avisa por mensaje.
# -----------------------------------------------------------------------------
completar_con_ultimo_trimestre <- function(tablas) {
  # tablas: lista nombrada con claves == a las de calcular_tablas_trimestre()
  # ("edad", "prov", "form", "nac", "sector", "ocup", "paro_larga"); los
  # elementos NULL (módulos aún no conectados) se ignoran sin más.
  tablas_presentes <- tablas[!vapply(tablas, is.null, logical(1))]
  if (length(tablas_presentes) == 0) return(tablas)
  
  ultimo_ine <- tryCatch(detectar_ultimo_trimestre(), error = function(e) NULL)
  if (is.null(ultimo_ine)) return(tablas)
  
  periodo_ine <- periodo_date(ultimo_ine$anio, ultimo_ine$trim)
  
  periodo_max_actual <- suppressWarnings(
    max(do.call(c, lapply(tablas_presentes, function(df) df$periodo)), na.rm = TRUE)
  )
  
  if (is.finite(as.numeric(periodo_max_actual)) && periodo_ine <= periodo_max_actual) {
    return(tablas)  # el cron ya está al día con lo que el INE tiene publicado
  }
  
  message(sprintf(
    "El INE ya tiene publicado %dT%d y las tablas de GitHub solo llegan a %s: cargando ese trimestre al vuelo...",
    ultimo_ine$anio, ultimo_ine$trim,
    if (is.finite(as.numeric(periodo_max_actual))) format(periodo_max_actual, "%Y-%m-%d") else "(sin datos)"
  ))
  
  res <- tryCatch(
    cargar_trimestre(ultimo_ine$anio, ultimo_ine$trim),
    error = function(e) list(error = conditionMessage(e))
  )
  err <- error_de(res)
  if (!is.null(err)) {
    message(sprintf("No se pudo completar el trimestre al vuelo (se sigue con los datos del cron): %s", err))
    return(tablas)
  }
  
  nuevas <- tryCatch(
    calcular_tablas_trimestre(res, ultimo_ine$anio, ultimo_ine$trim),
    error = function(e) {
      message(sprintf("Error calculando las tablas del trimestre al vuelo: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(nuevas)) return(tablas)
  
  for (nombre in names(tablas)) {
    if (is.null(tablas[[nombre]]) || is.null(nuevas[[nombre]])) next
    if (!(periodo_ine %in% tablas[[nombre]]$periodo)) {
      tablas[[nombre]] <- bind_rows(tablas[[nombre]], nuevas[[nombre]])
    }
  }
  
  message(sprintf(
    "Trimestre %dT%d añadido al vuelo para esta sesión del proceso R (no persistido; el próximo cron lo dejará ya en GitHub).",
    ultimo_ine$anio, ultimo_ine$trim
  ))
  tablas
}

tablas_completadas <- completar_con_ultimo_trimestre(list(
  edad = epa_edad_df,
  prov = epa_prov_df
))
epa_edad_df <- tablas_completadas$edad
epa_prov_df <- tablas_completadas$prov

territorios_disponibles <- if (!is.null(epa_edad_df)) sort(unique(epa_edad_df$region)) else c("Castilla y León", "España")
periodos_disponibles <- if (!is.null(epa_edad_df)) sort(unique(epa_edad_df$periodo), decreasing = TRUE) else Sys.Date()

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  title = "EPA — Herramienta de análisis del mercado laboral",
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Arial, sans-serif; }
    .kpi-card { background: #FAFAFA; }
  "))),
  
  titlePanel("EPA — Herramienta de análisis del mercado laboral"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros globales"),
      selectInput("f_territorio_ref", "Territorio de referencia",
                  choices = territorios_disponibles,
                  selected = if ("Castilla y León" %in% territorios_disponibles) "Castilla y León" else territorios_disponibles[1]),
      selectInput("f_territorio_comp", "Comparar con",
                  choices = c("Ninguno" = "", setdiff(territorios_disponibles, "")),
                  selected = if ("España" %in% territorios_disponibles) "España" else ""),
      selectInput("f_periodo", "Trimestre",
                  choices = setNames(as.character(periodos_disponibles), vapply(periodos_disponibles, formato_trimestre, character(1))),
                  selected = as.character(periodos_disponibles[1])),
      radioButtons("f_sexo", "Sexo", choices = c("Ambos sexos" = "total", "Hombres", "Mujeres"), selected = "total"),
      hr(),
      tags$p(
        style = "font-size:0.75em; color:#888;",
        "Las estimaciones con muestra insuficiente se marcan con ",
        tags$span(style = "color:#B3261E;", "\u26D4"),
        " y las de fiabilidad limitada con ",
        tags$span(style = "color:#B25E00;", "\u26A0"),
        ". Pasa el ratón sobre el icono para ver el detalle."
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "navegacion_principal",
        tabPanel("Resumen",
                 navlistPanel(
                   id = "nivel1",
                   widths = c(2, 10),
                   tabPanel("Cuadro de mando", mod_cuadro_mando_ui("cuadro_mando")),
                   tabPanel("¿Qué ha cambiado?", tags$p("Pendiente (Oleada 2)."))
                 )
        ),
        tabPanel("Análisis",
                 navlistPanel(
                   id = "nivel2",
                   widths = c(2, 10),
                   tabPanel("Empleo", tags$p("Pendiente.")),
                   tabPanel("Desempleo", tags$p("Pendiente.")),
                   tabPanel("Calidad del empleo", tags$p("Pendiente.")),
                   tabPanel("Temporalidad", tags$p("Pendiente.")),
                   tabPanel("Jornada y parcialidad", tags$p("Pendiente.")),
                   tabPanel("Mujeres / brecha de género", tags$p("Pendiente.")),
                   tabPanel("Jóvenes", tags$p("Pendiente.")),
                   tabPanel("Mayores de 55", tags$p("Pendiente.")),
                   tabPanel("Población extranjera", tags$p("Pendiente.")),
                   tabPanel("Sectores", tags$p("Pendiente.")),
                   tabPanel("Ocupaciones", tags$p("Pendiente.")),
                   tabPanel("Territorio", mod_territorio_ui("territorio"))
                 )
        ),
        tabPanel("Exploración avanzada",
                 navlistPanel(
                   id = "nivel3",
                   widths = c(2, 10),
                   tabPanel("Comparador / Explorador", tags$p("Pendiente (Oleada 2).")),
                   tabPanel("Comparativa CCAA", tags$p("Pendiente.")),
                   tabPanel("Histórico", tags$p("Pendiente.")),
                   tabPanel("Hogares", tags$p("Pendiente (Oleada 2).")),
                   tabPanel("Metodología", tags$p("Pendiente."))
                 )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Filtros globales compartidos, expuestos como reactives a cada módulo.
  # territorio_comp() devuelve NULL (no "") cuando no hay comparación, para
  # que los módulos solo tengan que comprobar is.null() una vez.
  filtros_globales <- list(
    territorio_ref  = reactive(input$f_territorio_ref),
    territorio_comp = reactive(if (identical(input$f_territorio_comp, "")) NULL else input$f_territorio_comp),
    periodo         = reactive(as.Date(input$f_periodo)),
    sexo            = reactive(switch(input$f_sexo, total = "total", Hombres = "Hombres", Mujeres = "Mujeres"))
  )
  
  mod_cuadro_mando_server("cuadro_mando", datos_edad = reactive(epa_edad_df), filtros = filtros_globales)
  mod_territorio_server("territorio", datos_edad = reactive(epa_edad_df), datos_prov = reactive(epa_prov_df), filtros = filtros_globales)
  
  # A medida que se conectan los módulos de Nivel 2/3, se llaman aquí con
  # el mismo patrón: mod_XXX_server("id", datos_YYY, filtros_globales)
}

shinyApp(ui, server)