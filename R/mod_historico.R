# -----------------------------------------------------------------------------
# mod_historico.R
#
# Pestaña "Histórico" (Nivel 3, Oleada 1). Serie completa desde ANIO_MIN
# (2005) para el indicador elegido, territorio de referencia vs territorio
# de comparación (si hay), más un segundo gráfico con la BRECHA entre ambos
# territorios en el tiempo (FASE 4 del briefing: "ver si converge o diverge
# de España en cada indicador desde 2008" -- aquí generalizado a cualquier
# territorio de comparación, no solo España). Reutiliza
# OPCIONES_INDICADOR_TERRITORIO de mod_territorio.R.
# -----------------------------------------------------------------------------

mod_historico_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(column(4, selectInput(ns("indicador"), "Indicador", choices = OPCIONES_INDICADOR_TERRITORIO))),
    plotly::plotlyOutput(ns("plot_historico"), height = "440px"),
    br(),
    uiOutput(ns("panel_brecha"))
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_historico_server <- function(id, datos_edad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = "serie completa", sexo = filtros$sexo()))
    })

    serie_ref <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), edad == "total", sexo == filtros$sexo()) |>
        dplyr::arrange(periodo)
    })

    serie_comp <- reactive({
      tc <- filtros$territorio_comp()
      if (is.null(tc)) return(NULL)
      req(datos_edad(), filtros$sexo())
      datos_edad() |>
        dplyr::filter(region == tc, edad == "total", sexo == filtros$sexo()) |>
        dplyr::arrange(periodo)
    })

    output$plot_historico <- plotly::renderPlotly({
      req(serie_ref(), input$indicador)
      etiqueta <- etiqueta_indicador_territorio(input$indicador)
      sc <- serie_comp()
      datos <- serie_ref() |> dplyr::mutate(serie_id = filtros$territorio_ref())
      if (!is.null(sc)) {
        datos <- dplyr::bind_rows(datos, sc |> dplyr::mutate(serie_id = filtros$territorio_comp()))
      }
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = .data[[input$indicador]], color = serie_id)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = etiqueta, color = NULL,
                      title = sprintf("%s desde %d", etiqueta, ANIO_MIN)) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    # --- Brecha territorio_ref - territorio_comp en el tiempo (solo si hay
    # comparación; si no, se omite el panel entero en vez de romper el
    # gráfico -- mismo principio ya aplicado en mod_territorio.R) -----------
    output$panel_brecha <- renderUI({
      if (is.null(filtros$territorio_comp())) return(NULL)
      tagList(
        tags$p(sprintf("Brecha %s \u2212 %s en el tiempo (\u00bfconverge o diverge?):",
                        filtros$territorio_ref(), filtros$territorio_comp())),
        plotly::plotlyOutput(session$ns("plot_brecha"), height = "340px")
      )
    })

    output$plot_brecha <- plotly::renderPlotly({
      sc <- serie_comp()
      req(serie_ref(), sc, input$indicador)
      etiqueta <- etiqueta_indicador_territorio(input$indicador)
      datos <- serie_ref() |>
        dplyr::select(periodo, valor_ref = dplyr::all_of(input$indicador)) |>
        dplyr::inner_join(sc |> dplyr::select(periodo, valor_comp = dplyr::all_of(input$indicador)), by = "periodo") |>
        dplyr::mutate(brecha = valor_ref - valor_comp)
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = brecha)) +
        ggplot2::geom_area(fill = "#B7C4D6", alpha = 0.6) +
        ggplot2::geom_line(color = "#0B5FA5") +
        ggplot2::geom_hline(yintercept = 0, color = "#888888") +
        ggplot2::labs(x = NULL, y = sprintf("Diferencia en %s", etiqueta),
                      title = sprintf("Brecha %s \u2212 %s", filtros$territorio_ref(), filtros$territorio_comp())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

  })
}
