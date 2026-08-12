# -----------------------------------------------------------------------------
# mod_extranjeros.R
#
# Pestaña "Población extranjera" (Nivel 2, Oleada 1). Usa epa_nac (region x
# nac x sexo), con nac en {ES, UE, no_UE, EX}: "EX" es el agregado UE+no_UE
# ya calculado en epa_helpers.R (con_totales sobre nac_ex), útil para el KPI
# principal sin tener que sumar UE+no_UE a mano en cada sitio.
# -----------------------------------------------------------------------------

ORDEN_NAC <- c("ES", "UE", "no_UE")
ETIQUETAS_NAC <- c(ES = "Española", UE = "Unión Europea", no_UE = "Resto del mundo")

mod_extranjeros_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(3, uiOutput(ns("kpi_tasa_paro_ex"))),
      column(3, uiOutput(ns("kpi_diferencia_es")))
    ),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_comparativa"), height = "420px")),
      column(6, plotly::plotlyOutput(ns("plot_evolucion"), height = "420px"))
    ),
    fluidRow(column(4, selectInput(ns("indicador_evolucion"), "Indicador de la evoluci\u00f3n",
                                    choices = c("Tasa de paro" = "tasa_par", "Tasa de empleo" = "tasa_emp", "Tasa de actividad" = "tasa_act")))),
    uiOutput(ns("aviso"))
  )
}

#' @param id id del módulo
#' @param datos_nac reactive() que devuelve la tabla epa_nac ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_extranjeros_server <- function(id, datos_nac, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    datos_actual <- reactive({
      req(datos_nac(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      datos_nac() |>
        dplyr::filter(region == filtros$territorio_ref(), periodo == filtros$periodo(), sexo == filtros$sexo()) |>
        anotar_fiabilidad("act")
    })

    fila_ex <- reactive({
      f <- datos_actual() |> dplyr::filter(nac == "EX")
      if (nrow(f) != 1) return(NULL)
      f
    })
    fila_es <- reactive({
      f <- datos_actual() |> dplyr::filter(nac == "ES")
      if (nrow(f) != 1) return(NULL)
      f
    })

    output$kpi_tasa_paro_ex <- renderUI({
      f <- fila_ex()
      kpi_card(titulo = "Tasa de paro (poblaci\u00f3n extranjera)", unidad = "%",
                valor = if (!is.null(f)) round(f$tasa_par, 1) else NA,
                fiabilidad = if (!is.null(f)) f$fiabilidad else NA, n_muestra = if (!is.null(f)) f$n_muestra else NA)
    })

    output$kpi_diferencia_es <- renderUI({
      fe <- fila_ex(); fs <- fila_es()
      dif <- if (!is.null(fe) && !is.null(fs)) round(fe$tasa_par - fs$tasa_par, 1) else NA
      kpi_card(titulo = "Diferencia vs poblaci\u00f3n espa\u00f1ola", unidad = " pp", valor = dif)
    })

    output$plot_comparativa <- plotly::renderPlotly({
      df <- datos_actual() |> dplyr::filter(nac %in% ORDEN_NAC)
      req(nrow(df) > 0)
      df <- df |> dplyr::mutate(nac_label = factor(ETIQUETAS_NAC[nac], levels = ETIQUETAS_NAC[ORDEN_NAC]))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = nac_label, y = tasa_par,
                                              text = sprintf("%s: %.1f%%", nac_label, tasa_par))) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::labs(x = NULL, y = "Tasa de paro (%)",
                      title = sprintf("Tasa de paro por nacionalidad \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_evolucion <- plotly::renderPlotly({
      req(datos_nac(), filtros$territorio_ref(), filtros$sexo(), input$indicador_evolucion)
      datos <- datos_nac() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), nac %in% ORDEN_NAC) |>
        dplyr::mutate(nac_label = factor(ETIQUETAS_NAC[nac], levels = ETIQUETAS_NAC[ORDEN_NAC]))
      etiqueta <- c(tasa_par = "Tasa de paro (%)", tasa_emp = "Tasa de empleo (%)", tasa_act = "Tasa de actividad (%)")[[input$indicador_evolucion]]
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = .data[[input$indicador_evolucion]], color = nac_label)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = etiqueta, color = NULL, title = "Evoluci\u00f3n por nacionalidad") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso <- renderUI({
      df <- datos_actual() |> dplyr::filter(nac %in% c(ORDEN_NAC, "EX"))
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(nac)
      if (length(insuficientes) == 0) return(NULL)
      etiquetas_insuf <- ifelse(insuficientes == "EX", "Extranjera (total)", ETIQUETAS_NAC[insuficientes])
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(etiquetas_insuf, collapse = ", ")))
    })

  })
}
