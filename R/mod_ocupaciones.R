# -----------------------------------------------------------------------------
# mod_ocupaciones.R
#
# Pestaña "Ocupaciones" (Nivel 2, Oleada 1). Usa epa_ocup (region x
# ocupacion x sexo, solo valor_ocu/n_ocu; ocupacion = 10 grandes grupos CNO
# vía OCUP1, ver ocupacion_label() en epa_helpers.R). Responde a: ¿qué
# ocupaciones ganan o pierden peso?
# -----------------------------------------------------------------------------

mod_ocupaciones_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_niveles"), height = "480px")),
      column(6, plotly::plotlyOutput(ns("plot_ranking_crecimiento"), height = "480px"))
    ),
    uiOutput(ns("aviso"))
  )
}

#' @param id id del módulo
#' @param datos_ocup reactive() que devuelve la tabla epa_ocup ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_ocupaciones_server <- function(id, datos_ocup, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    serie_ocup <- reactive({
      req(datos_ocup(), filtros$territorio_ref(), filtros$sexo())
      datos_ocup() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), ocupacion != "Total", !is.na(ocupacion)) |>
        dplyr::arrange(ocupacion, periodo)
    })

    datos_actual <- reactive({
      req(serie_ocup(), filtros$periodo())
      serie_ocup() |> dplyr::filter(periodo == filtros$periodo()) |> anotar_fiabilidad("ocu")
    })

    output$plot_niveles <- plotly::renderPlotly({
      df <- datos_actual()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(ocupacion, valor_ocu), y = valor_ocu,
                                              text = sprintf("%s: %s", ocupacion, format(round(valor_ocu), big.mark = ".", decimal.mark = ",")))) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Ocupados",
                      title = sprintf("Ocupados por grupo ocupacional \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    ranking_df <- reactive({
      req(serie_ocup())
      serie_ocup() |>
        dplyr::group_by(ocupacion) |>
        dplyr::group_modify(~ variacion_interanual(.x, "valor_ocu")) |>
        dplyr::ungroup() |>
        dplyr::filter(periodo == filtros$periodo())
    })

    output$plot_ranking_crecimiento <- plotly::renderPlotly({
      df <- ranking_df()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(ocupacion, var_interanual_pct), y = var_interanual_pct,
                                              fill = var_interanual_pct >= 0,
                                              text = sprintf("%s: %+.1f%% interanual", ocupacion, var_interanual_pct))) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 0, color = "#888888") +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#1B7A3D", "FALSE" = "#B3261E"), guide = "none") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Variación interanual de ocupados (%)",
                      title = "¿Qué ocupaciones más crecen / más caen?") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$aviso <- renderUI({
      df <- datos_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(ocupacion)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

  })
}
