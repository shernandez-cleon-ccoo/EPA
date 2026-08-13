# -----------------------------------------------------------------------------
# mod_comparativa_ccaa.R
#
# Pestaña "Comparativa CCAA" (Nivel 3, Oleada 1). Responde a: ¿en qué
# posición está el territorio de referencia frente a otras CCAA? A
# diferencia del ranking de mod_territorio.R (un único trimestre), aquí el
# valor añadido es la EVOLUCIÓN DEL PUESTO en el tiempo -- ¿el territorio de
# referencia mejora o empeora su posición relativa, no solo su valor
# absoluto? Reutiliza OPCIONES_INDICADOR_TERRITORIO de mod_territorio.R (ya
# en el entorno global tras el source() de app.R) para no duplicar la lista.
# -----------------------------------------------------------------------------

mod_comparativa_ccaa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(column(4, selectInput(ns("indicador"), "Indicador", choices = OPCIONES_INDICADOR_TERRITORIO))),
    plotly::plotlyOutput(ns("plot_ranking"), height = "560px"),
    uiOutput(ns("aviso_ranking")),
    br(),
    plotly::plotlyOutput(ns("plot_evolucion_puesto"), height = "380px"),
    tags$p(
      style = "font-size:0.75em; color:#888;",
      "El puesto 1 es siempre el mejor valor del indicador seleccionado entre las ",
      "17 CCAA + Ceuta/Melilla (para tasa de paro, el puesto 1 es la tasa más baja; ",
      "para tasa de empleo, la más alta)."
    )
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_comparativa_ccaa_server <- function(id, datos_edad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    # Serie completa, todas las CCAA (sin "España", que no es una CCAA a
    # rankear), edad = total, con el puesto ya calculado por periodo.
    # "Mejor" depende del indicador: para tasa de paro, menor es mejor; para
    # el resto (actividad, empleo, ocupados, activos), mayor es mejor.
    serie_ccaa <- reactive({
      req(datos_edad(), filtros$sexo(), input$indicador)
      col <- input$indicador
      menor_es_mejor <- identical(col, "tasa_par")
      datos_edad() |>
        dplyr::filter(sexo == filtros$sexo(), edad == "total", region != "España") |>
        dplyr::group_by(periodo) |>
        dplyr::mutate(puesto = if (menor_es_mejor) rank(.data[[col]], ties.method = "min") else rank(-.data[[col]], ties.method = "min")) |>
        dplyr::ungroup()
    })

    datos_actual <- reactive({
      req(serie_ccaa(), filtros$periodo())
      serie_ccaa() |> dplyr::filter(periodo == filtros$periodo()) |> anotar_fiabilidad("act")
    })

    output$plot_ranking <- plotly::renderPlotly({
      df <- datos_actual()
      req(nrow(df) > 0)
      etiqueta <- etiqueta_indicador_territorio(input$indicador)
      media_espana <- mean(df[[input$indicador]], na.rm = TRUE)
      df <- df |> dplyr::mutate(destacada = region == filtros$territorio_ref())

      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(region, .data[[input$indicador]]), y = .data[[input$indicador]], fill = destacada,
                                              text = sprintf("%s (puesto %d): %.1f", region, puesto, .data[[input$indicador]]))) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = media_espana, linetype = "dashed", color = "#888888") +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#0B5FA5", "FALSE" = "#B7C4D6"), guide = "none") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = etiqueta,
                      title = sprintf("%s por CCAA \u2014 %s (l\u00ednea = media)", etiqueta, formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$aviso_ranking <- renderUI({
      df <- datos_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(region)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

    output$plot_evolucion_puesto <- plotly::renderPlotly({
      req(serie_ccaa(), filtros$territorio_ref())
      etiqueta <- etiqueta_indicador_territorio(input$indicador)
      datos <- serie_ccaa() |> dplyr::filter(region == filtros$territorio_ref())
      n_ccaa <- length(unique(serie_ccaa()$region))
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = puesto)) +
        ggplot2::geom_line(color = "#0B5FA5") +
        ggplot2::scale_y_reverse(breaks = scales::pretty_breaks(), limits = c(n_ccaa, 1)) +
        ggplot2::labs(x = NULL, y = "Puesto (1 = mejor)",
                      title = sprintf("Evoluci\u00f3n del puesto de %s en %s", filtros$territorio_ref(), tolower(etiqueta))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

  })
}
