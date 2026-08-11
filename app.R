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

source("epa_helpers.R")       # cargar_trimestre, diccionarios, con_totales... (pendiente de repartir en R/)
source("R/stat_quality.R")
source("R/indicators.R")
source("R/ui_helpers.R")
source("R/mod_cuadro_mando.R")

DATA_DIR <- "data_agregada"

# -----------------------------------------------------------------------------
# Carga de datos agregados (una vez al arrancar la app; los .rds ya están
# preprocesados por build_epa_tablas.R, así que esto es barato)
# -----------------------------------------------------------------------------
cargar_tabla <- function(nombre) {
  ruta <- file.path(DATA_DIR, paste0(nombre, ".rds"))
  if (!file.exists(ruta)) return(NULL)
  readRDS(ruta)
}

epa_edad_df <- cargar_tabla("epa_edad")
# epa_prov_df / epa_form_df / epa_nac_df / epa_sector_df / epa_ocup_df /
# epa_paro_larga_df se cargan igual, cuando se conecten los módulos
# correspondientes (Nivel 2). No se cargan aquí para no hacer el skeleton
# más pesado de lo necesario.

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
                  choices = setNames(as.character(periodos_disponibles), format(periodos_disponibles, "%Y T%q") |> tryCatch(error = function(e) as.character(periodos_disponibles))),
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
            tabPanel("Territorio", tags$p("Pendiente."))
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

  # A medida que se conectan los módulos de Nivel 2/3, se llaman aquí con
  # el mismo patrón: mod_XXX_server("id", datos_YYY, filtros_globales)
}

shinyApp(ui, server)
