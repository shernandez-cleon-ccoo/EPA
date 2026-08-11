# -----------------------------------------------------------------------------
# mod_cuadro_mando.R
#
# Pestaña "Nivel 1 - Lectura rápida". Recibe los filtros globales
# (territorio de referencia, territorio de comparación -posible NULL-,
# periodo, sexo) como reactives desde app.R y no gestiona ningún filtro
# propio: esta pestaña es deliberadamente simple, la exploración fina vive
# en los módulos de Nivel 2/3.
# -----------------------------------------------------------------------------

mod_cuadro_mando_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(3, uiOutput(ns("kpi_activos"))),
      column(3, uiOutput(ns("kpi_ocupados"))),
      column(3, uiOutput(ns("kpi_parados"))),
      column(3, uiOutput(ns("kpi_tasa_paro")))
    ),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_evolucion"))),
      column(6, plotly::plotlyOutput(ns("plot_tasa_paro_comparada")))
    )
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param filtros lista de reactives compartidos desde app.R:
#'   territorio_ref, territorio_comp (puede resolver a NULL), periodo, sexo
mod_cuadro_mando_server <- function(id, datos_edad, filtros) {
  moduleServer(id, function(input, output, session) {

    # --- Serie del territorio de referencia, totales (edad=total, sexo
    # según filtro global), con variaciones ya calculadas -------------------
    serie_ref <- reactive({
      req(datos_edad(), filtros$territorio_ref())
      datos_edad() |>
        dplyr::filter(
          region == filtros$territorio_ref(),
          edad == "total",
          sexo == filtros$sexo()
        ) |>
        variacion_interanual("valor_par") |>
        variacion_intertrimestral("valor_par") |>
        anotar_fiabilidad("act")
    })

    # --- Serie del territorio de comparación (o NULL si no hay) -----------
    serie_comp <- reactive({
      tc <- filtros$territorio_comp()
      if (is.null(tc) || identical(tc, "")) return(NULL)
      datos_edad() |>
        dplyr::filter(region == tc, edad == "total", sexo == filtros$sexo())
    })

    fila_actual <- reactive({
      req(serie_ref())
      s <- serie_ref() |> dplyr::filter(periodo == filtros$periodo())
      req(nrow(s) == 1)
      s
    })

    fila_comp_actual <- reactive({
      sc <- serie_comp()
      if (is.null(sc)) return(NULL)
      f <- sc |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1) return(NULL)
      f
    })

    etiqueta_comp <- reactive({
      tc <- filtros$territorio_comp()
      if (is.null(tc) || identical(tc, "")) NULL else tc
    })

    output$resumen_seleccion <- renderUI({
      req(fila_actual())
      tags$div(
        style = "color:#555; font-size:0.9em;",
        resumen_filtros(
          territorio = filtros$territorio_ref(),
          periodo_label = format(filtros$periodo(), "T%q %Y") |> tryCatch(error = function(e) as.character(filtros$periodo())),
          sexo = filtros$sexo()
        )
      )
    })

    # --- KPIs: cada uno reutiliza kpi_card() de ui_helpers.R, sin volver a
    # calcular el aviso de fiabilidad a mano (lo hace badge_calidad()) -----
    output$kpi_activos <- renderUI({
      f <- fila_actual(); fc <- fila_comp_actual()
      kpi_card(
        titulo = "Activos", valor = round(f$valor_act),
        var_intertrim = f$var_intertrim_pct, var_interanual = f$var_interanual_pct,
        diferencia = if (!is.null(fc)) f$valor_act - fc$valor_act else NA_real_,
        etiqueta_comparacion = etiqueta_comp(),
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    output$kpi_ocupados <- renderUI({
      f <- fila_actual(); fc <- fila_comp_actual()
      kpi_card(
        titulo = "Ocupados", valor = round(f$valor_ocu),
        diferencia = if (!is.null(fc)) f$valor_ocu - fc$valor_ocu else NA_real_,
        etiqueta_comparacion = etiqueta_comp(),
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    output$kpi_parados <- renderUI({
      f <- fila_actual(); fc <- fila_comp_actual()
      kpi_card(
        titulo = "Parados", valor = round(f$valor_par),
        var_intertrim = f$var_intertrim_pct, var_interanual = f$var_interanual_pct,
        diferencia = if (!is.null(fc)) f$valor_par - fc$valor_par else NA_real_,
        etiqueta_comparacion = etiqueta_comp(),
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    output$kpi_tasa_paro <- renderUI({
      f <- fila_actual(); fc <- fila_comp_actual()
      kpi_card(
        titulo = "Tasa de paro", valor = round(f$tasa_par, 1), unidad = "%",
        diferencia = if (!is.null(fc)) f$tasa_par - fc$tasa_par else NA_real_,
        etiqueta_comparacion = etiqueta_comp(),
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    # --- Gráfico 1: evolución ocupados/parados desde el inicio disponible -
    output$plot_evolucion <- plotly::renderPlotly({
      req(serie_ref())
      p <- ggplot2::ggplot(serie_ref(), ggplot2::aes(x = periodo)) +
        ggplot2::geom_line(ggplot2::aes(y = valor_ocu, color = "Ocupados")) +
        ggplot2::geom_line(ggplot2::aes(y = valor_par, color = "Parados")) +
        ggplot2::scale_color_manual(values = c("Ocupados" = "#0B5FA5", "Parados" = "#B3261E")) +
        ggplot2::labs(x = NULL, y = "Personas", color = NULL, title = "Ocupados y parados") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    # --- Gráfico 2: tasa de paro, territorio de referencia vs comparación -
    # (versión reducida a una sola serie si no hay territorio_comp, según
    # el diseño acordado: NUNCA un gráfico roto por comparación ausente)
    output$plot_tasa_paro_comparada <- plotly::renderPlotly({
      req(serie_ref())
      base <- serie_ref() |> dplyr::mutate(serie = filtros$territorio_ref())
      sc <- serie_comp()
      datos_plot <- if (!is.null(sc)) {
        dplyr::bind_rows(base, sc |> dplyr::mutate(serie = filtros$territorio_comp()))
      } else {
        base
      }
      p <- ggplot2::ggplot(datos_plot, ggplot2::aes(x = periodo, y = tasa_par, color = serie)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = "Tasa de paro (%)", color = NULL, title = "Tasa de paro") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

  })
}
