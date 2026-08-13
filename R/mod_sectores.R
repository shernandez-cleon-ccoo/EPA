# -----------------------------------------------------------------------------
# mod_sectores.R
#
# Pestaña "Sectores" (Nivel 2, Oleada 1). Usa epa_sector (region x sector x
# edad x sexo, solo valor_ocu/n_ocu -- ver nota de universo en
# epa_helpers.R::calcular_tablas_trimestre). Responde a: ¿dónde se crea o se
# pierde empleo por sector?
# -----------------------------------------------------------------------------

ORDEN_SECTORES <- c("agricultura", "industria", "construcción", "servicios")
ETIQUETAS_SECTOR <- c(agricultura = "Agricultura", industria = "Industria",
                       "construcción" = "Construcción", servicios = "Servicios")

mod_sectores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_peso_relativo"), height = "400px")),
      column(6, plotly::plotlyOutput(ns("plot_ranking_crecimiento"), height = "400px"))
    ),
    br(),
    plotly::plotlyOutput(ns("plot_evolucion"), height = "400px"),
    uiOutput(ns("aviso"))
  )
}

#' @param id id del módulo
#' @param datos_sector reactive() que devuelve la tabla epa_sector ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_sectores_server <- function(id, datos_sector, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    # Serie completa (todos los periodos), edad = total, sector != Total.
    serie_sectores <- reactive({
      req(datos_sector(), filtros$territorio_ref(), filtros$sexo())
      datos_sector() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), edad == "total", sector %in% ORDEN_SECTORES) |>
        dplyr::mutate(sector_label = factor(ETIQUETAS_SECTOR[sector], levels = ETIQUETAS_SECTOR[ORDEN_SECTORES])) |>
        dplyr::arrange(sector, periodo)
    })

    datos_actual <- reactive({
      req(serie_sectores(), filtros$periodo())
      serie_sectores() |> dplyr::filter(periodo == filtros$periodo()) |> anotar_fiabilidad("ocu")
    })

    output$plot_peso_relativo <- plotly::renderPlotly({
      df <- datos_actual()
      req(nrow(df) > 0)
      total <- sum(df$valor_ocu, na.rm = TRUE)
      df <- df |> dplyr::mutate(peso_pct = 100 * valor_ocu / total)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(sector_label, peso_pct), y = peso_pct,
                                              text = sprintf("%s: %.1f%% de los ocupados", sector_label, peso_pct))) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "% de los ocupados",
                      title = sprintf("Peso de cada sector \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    # Ranking de crecimiento interanual por sector.
    ranking_df <- reactive({
      req(serie_sectores())
      serie_sectores() |>
        dplyr::group_by(sector, sector_label) |>
        dplyr::group_modify(~ variacion_interanual(.x, "valor_ocu")) |>
        dplyr::ungroup() |>
        dplyr::filter(periodo == filtros$periodo())
    })

    output$plot_ranking_crecimiento <- plotly::renderPlotly({
      df <- ranking_df()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(sector_label, var_interanual_pct), y = var_interanual_pct,
                                              fill = var_interanual_pct >= 0,
                                              text = sprintf("%s: %+.1f%% interanual", sector_label, var_interanual_pct))) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 0, color = "#888888") +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#1B7A3D", "FALSE" = "#B3261E"), guide = "none") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Variación interanual de ocupados (%)",
                      title = "¿Qué sectores crecen o caen?") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_evolucion <- plotly::renderPlotly({
      req(serie_sectores())
      p <- ggplot2::ggplot(serie_sectores(), ggplot2::aes(x = periodo, y = valor_ocu, color = sector_label)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = "Ocupados", color = NULL,
                      title = sprintf("Evoluci\u00f3n de ocupados por sector \u2014 %s", filtros$territorio_ref())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso <- renderUI({
      df <- datos_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(sector_label)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

  })
}
