# -----------------------------------------------------------------------------
# mod_jovenes.R
#
# Pestaña "Jóvenes" (Nivel 2, Oleada 1). Responde a: ¿cómo le va a la
# generación joven vs la media?
#
# NOTA DE COBERTURA: el briefing original pedía tramos 16-19/20-24/25-29,
# pero EDAD5 (T5EDAD) solo trae quinquenios "00".."65" y epa_edad ya agrupa
# "25"/"30" en una única banda "25-34" (ver edad_banda() en epa_helpers.R) --
# separar 25-29 de 30-34 exigiría recalcular la tabla estrella con una banda
# más fina, así que aquí "jóvenes" se define como 16-19 + 20-24 (16-24
# combinado), con las dos bandas individuales también disponibles para quien
# quiera el detalle.
# -----------------------------------------------------------------------------

BANDAS_JOVENES <- c("16-19", "20-24")

OPCIONES_TRAMO_JOVENES <- c(
  "Jóvenes 16-24 (combinado)" = "combinado",
  "16-19 años" = "16-19",
  "20-24 años" = "20-24"
)

mod_jovenes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(column(4, selectInput(ns("tramo"), "Tramo de edad", choices = OPCIONES_TRAMO_JOVENES))),
    fluidRow(
      column(3, uiOutput(ns("kpi_tasa_paro"))),
      column(3, uiOutput(ns("kpi_diferencia_total")))
    ),
    br(),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_comparativa"), height = "400px")),
      column(6, plotly::plotlyOutput(ns("plot_historico_paro"), height = "400px"))
    ),
    uiOutput(ns("aviso"))
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_jovenes_server <- function(id, datos_edad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(style = "color:#555; font-size:0.9em;",
                resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = filtros$sexo()))
    })

    # --- Serie del tramo elegido: si es "combinado", suma las 2 bandas y
    # recalcula tasas desde los agregados (nunca promedia tasas ya hechas) --
    serie_tramo <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      base <- datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo(), edad %in% BANDAS_JOVENES)

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

    output$kpi_tasa_paro <- renderUI({
      f <- fila_tramo_actual()
      n_muestra <- if (!is.null(f)) f$n_act else NA_integer_
      kpi_card(titulo = "Tasa de paro juvenil", unidad = "%",
                valor = if (!is.null(f)) round(f$tasa_par, 1) else NA,
                fiabilidad = if (!is.null(f)) clasificar_fiabilidad(n_muestra) else NA, n_muestra = n_muestra)
    })

    output$kpi_diferencia_total <- renderUI({
      fj <- fila_tramo_actual(); ft <- fila_total_actual()
      dif <- if (!is.null(fj) && !is.null(ft)) round(fj$tasa_par - ft$tasa_par, 1) else NA
      kpi_card(titulo = "Diferencia vs tasa de paro total", unidad = " pp", valor = dif)
    })

    output$plot_comparativa <- plotly::renderPlotly({
      fj <- fila_tramo_actual(); ft <- fila_total_actual()
      req(fj, ft)
      datos <- tibble::tibble(
        indicador = rep(c("Actividad", "Empleo", "Paro"), 2),
        grupo = rep(c("Jóvenes", "Total"), each = 3),
        valor = c(fj$tasa_act, fj$tasa_emp, fj$tasa_par, ft$tasa_act, ft$tasa_emp, ft$tasa_par)
      )
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = indicador, y = valor, fill = grupo,
                                                 text = sprintf("%s (%s): %.1f%%", indicador, grupo, valor))) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::scale_fill_manual(values = c("Jóvenes" = "#0B5FA5", "Total" = "#B7C4D6")) +
        ggplot2::labs(x = NULL, y = "%", fill = NULL,
                      title = sprintf("J\u00f3venes vs total \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_historico_paro <- plotly::renderPlotly({
      req(serie_tramo(), serie_total())
      datos <- dplyr::bind_rows(
        serie_tramo() |> dplyr::mutate(grupo = "Jóvenes") |> dplyr::select(periodo, tasa_par, grupo),
        serie_total() |> dplyr::mutate(grupo = "Total") |> dplyr::select(periodo, tasa_par, grupo)
      )
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = tasa_par, color = grupo)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Jóvenes" = "#0B5FA5", "Total" = "#888888")) +
        ggplot2::labs(x = NULL, y = "Tasa de paro (%)", color = NULL, title = "Evoluci\u00f3n del paro juvenil") +
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
