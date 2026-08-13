# -----------------------------------------------------------------------------
# mod_mayores.R
#
# Pestaña "Mayores de 55" (Nivel 2, Oleada 1). Mismo patrón que mod_jovenes.R
# (comparativa vs total + histórico), aplicado a las bandas 55-64/65+ --
# especialmente relevante en Castilla y León por el envejecimiento
# poblacional ya señalado en la pestaña Mercado laboral/Territorio.
# -----------------------------------------------------------------------------

BANDAS_MAYORES <- c("55-64", "65+")

OPCIONES_TRAMO_MAYORES <- c(
  "Mayores 55+ (combinado)" = "combinado",
  "55-64 años" = "55-64",
  "65 y más años" = "65+"
)

mod_mayores_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(column(4, selectInput(ns("tramo"), "Tramo de edad", choices = OPCIONES_TRAMO_MAYORES))),
    fluidRow(
      column(3, uiOutput(ns("kpi_tasa_actividad"))),
      column(3, uiOutput(ns("kpi_diferencia_total")))
    ),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_comparativa"), height = "400px")),
      column(6, plotly::plotlyOutput(ns("plot_historico_actividad"), height = "400px"))
    ),
    uiOutput(ns("aviso")),
    tags$p(
      style = "font-size:0.75em; color:#888;",
      "La tasa de actividad (no la de paro) es el indicador principal aquí: en la banda ",
      "65+ la mayoría ya está fuera del mercado laboral por jubilación, así que su ",
      "tasa de actividad mide sobre todo alargamiento voluntario/forzoso de la vida ",
      "laboral, no desempleo."
    )
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_mayores_server <- function(id, datos_edad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    serie_tramo <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      base <- datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), edad %in% BANDAS_MAYORES)

      if (identical(input$tramo, "combinado")) {
        base |>
          dplyr::group_by(periodo) |>
          dplyr::summarise(
            valor_act = sum(valor_act, na.rm = TRUE), valor_ocu = sum(valor_ocu, na.rm = TRUE),
            valor_par = sum(valor_par, na.rm = TRUE), valor_pob = sum(valor_pob, na.rm = TRUE),
            n_act = sum(n_act, na.rm = TRUE), n_ocu = sum(n_ocu, na.rm = TRUE), n_par = sum(n_par, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            tasa_par = 100 * valor_par / valor_act,
            tasa_act = 100 * valor_act / valor_pob,
            tasa_emp = 100 * valor_ocu / valor_pob
          )
      } else {
        base |> dplyr::filter(edad == input$tramo)
      }
    })

    serie_total <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      datos_edad() |> dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), edad == "total")
    })

    fila_tramo_actual <- reactive({
      req(serie_tramo(), filtros$periodo())
      f <- serie_tramo() |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1) return(NULL)
      f
    })
    fila_total_actual <- reactive({
      req(serie_total(), filtros$periodo())
      f <- serie_total() |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1) return(NULL)
      f
    })

    output$kpi_tasa_actividad <- renderUI({
      f <- fila_tramo_actual()
      n_muestra <- if (!is.null(f)) f$n_act else NA_integer_
      kpi_card(titulo = "Tasa de actividad", unidad = "%",
                valor = if (!is.null(f)) round(f$tasa_act, 1) else NA,
                fiabilidad = if (!is.null(f)) clasificar_fiabilidad(n_muestra) else NA, n_muestra = n_muestra)
    })

    output$kpi_diferencia_total <- renderUI({
      fm <- fila_tramo_actual(); ft <- fila_total_actual()
      dif <- if (!is.null(fm) && !is.null(ft)) round(fm$tasa_act - ft$tasa_act, 1) else NA
      kpi_card(titulo = "Diferencia vs tasa de actividad total", unidad = " pp", valor = dif)
    })

    output$plot_comparativa <- plotly::renderPlotly({
      fm <- fila_tramo_actual(); ft <- fila_total_actual()
      req(fm, ft)
      datos <- tibble::tibble(
        indicador = rep(c("Actividad", "Empleo", "Paro"), 2),
        grupo = rep(c("Mayores", "Total"), each = 3),
        valor = c(fm$tasa_act, fm$tasa_emp, fm$tasa_par, ft$tasa_act, ft$tasa_emp, ft$tasa_par)
      )
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = indicador, y = valor, fill = grupo,
                                                 text = sprintf("%s (%s): %.1f%%", indicador, grupo, valor))) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::scale_fill_manual(values = c("Mayores" = "#0B5FA5", "Total" = "#B7C4D6")) +
        ggplot2::labs(x = NULL, y = "%", fill = NULL,
                      title = sprintf("Mayores vs total \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_historico_actividad <- plotly::renderPlotly({
      req(serie_tramo(), serie_total())
      datos <- dplyr::bind_rows(
        serie_tramo() |> dplyr::mutate(grupo = "Mayores") |> dplyr::select(periodo, tasa_act, grupo),
        serie_total() |> dplyr::mutate(grupo = "Total") |> dplyr::select(periodo, tasa_act, grupo)
      )
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = tasa_act, color = grupo)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Mayores" = "#0B5FA5", "Total" = "#888888")) +
        ggplot2::labs(x = NULL, y = "Tasa de actividad (%)", color = NULL, title = "Evoluci\u00f3n de la actividad en mayores") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso <- renderUI({
      f <- fila_tramo_actual()
      req(f)
      badge <- texto_aviso_fiabilidad(clasificar_fiabilidad(f$n_act), f$n_act)
      if (is.null(badge)) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;", paste("\u26D4", badge))
    })

  })
}
