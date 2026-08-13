# -----------------------------------------------------------------------------
# mod_mujeres.R
#
# Pestaña "Mujeres / brecha de género" (Nivel 2, Oleada 1). Responde a: ¿qué
# brecha hay en cada indicador? Combina epa_edad (tasa_act/tasa_emp/tasa_par
# por edad × sexo) y epa_calidad (tasa_temporalidad/tasa_parcialidad por
# edad × sexo, con sector="Total" para no mezclar universos) en un único
# heatmap edad × indicador, con color = brecha (mujeres − hombres).
# -----------------------------------------------------------------------------

OPCIONES_INDICADOR_BRECHA <- c(
  "Tasa de actividad"   = "tasa_act",
  "Tasa de empleo"      = "tasa_emp",
  "Tasa de paro"        = "tasa_par",
  "Tasa de temporalidad" = "tasa_temporalidad",
  "Tasa de parcialidad"  = "tasa_parcialidad"
)

mod_mujeres_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    tags$p(
      "Brecha en puntos porcentuales (mujeres \u2212 hombres): positivo = el ",
      "indicador es mayor entre mujeres; negativo = mayor entre hombres."
    ),
    plotly::plotlyOutput(ns("heatmap"), height = "420px"),
    br(),
    fluidRow(column(4, selectInput(ns("indicador_evolucion"), "Evoluci\u00f3n por sexo del indicador",
                                    choices = OPCIONES_INDICADOR_BRECHA))),
    plotly::plotlyOutput(ns("plot_evolucion_sexo"), height = "380px")
  )
}

#' @param id id del módulo
#' @param datos_edad,datos_calidad reactives con las tablas estrella ya cargadas
#' @param filtros lista de reactives compartidos desde app.R (usa
#'   territorio_ref y periodo; el filtro global de sexo no aplica aquí, la
#'   pestaña entera ES el desglose por sexo)
mod_mujeres_server <- function(id, datos_edad, datos_calidad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(
        style = "color:#555; font-size:0.9em;",
        resumen_filtros(territorio = filtros$territorio_ref(), periodo_label = formato_trimestre(filtros$periodo()), sexo = "Hombres vs Mujeres")
      )
    })

    # --- Datos base por indicador, ya en formato largo (categoria = edad,
    # indicador, sexo, valor) para poder pivotar tanto al heatmap (edad x
    # indicador) como a la evolución (periodo x sexo) sin duplicar consultas.
    largo_edad <- reactive({
      req(datos_edad(), filtros$territorio_ref())
      datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), edad %in% ORDEN_BANDAS_EDAD, sexo %in% c("Hombres", "Mujeres")) |>
        dplyr::select(periodo, edad, sexo, tasa_act, tasa_emp, tasa_par) |>
        tidyr::pivot_longer(c(tasa_act, tasa_emp, tasa_par), names_to = "indicador", values_to = "valor")
    })

    largo_calidad <- reactive({
      req(datos_calidad(), filtros$territorio_ref())
      datos_calidad() |>
        dplyr::filter(region == filtros$territorio_ref(), edad %in% ORDEN_BANDAS_EDAD, sector == "Total", sexo %in% c("Hombres", "Mujeres")) |>
        dplyr::select(periodo, edad, sexo, tasa_temporalidad, tasa_parcialidad) |>
        tidyr::pivot_longer(c(tasa_temporalidad, tasa_parcialidad), names_to = "indicador", values_to = "valor")
    })

    largo_total <- reactive(dplyr::bind_rows(largo_edad(), largo_calidad()))

    # --- Heatmap edad x indicador, trimestre seleccionado -------------------
    output$heatmap <- plotly::renderPlotly({
      req(largo_total(), filtros$periodo())
      datos <- largo_total() |>
        dplyr::filter(periodo == filtros$periodo()) |>
        tidyr::pivot_wider(id_cols = c(edad, indicador), names_from = sexo, values_from = valor) |>
        dplyr::filter(!is.na(Hombres), !is.na(Mujeres)) |>
        dplyr::mutate(
          brecha = Mujeres - Hombres,
          edad = factor(edad, levels = ORDEN_BANDAS_EDAD),
          indicador_label = names(OPCIONES_INDICADOR_BRECHA)[match(indicador, OPCIONES_INDICADOR_BRECHA)]
        )
      shiny::validate(shiny::need(nrow(datos) > 0, "No hay datos suficientes para este trimestre."))

      p <- ggplot2::ggplot(datos, ggplot2::aes(
        x = indicador_label, y = edad, fill = brecha,
        text = sprintf("%s \u2014 %s: %+.1f pp", edad, indicador_label, brecha)
      )) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "#0B5FA5", mid = "#FAFAFA", high = "#B3261E", midpoint = 0, name = "Brecha (pp)") +
        ggplot2::labs(x = NULL, y = NULL,
                      title = sprintf("Brecha de g\u00e9nero por edad \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

      plotly::ggplotly(p, tooltip = "text")
    })

    # --- Evolución por sexo del indicador elegido, edad = total -------------
    output$plot_evolucion_sexo <- plotly::renderPlotly({
      req(input$indicador_evolucion, filtros$territorio_ref())
      es_calidad <- input$indicador_evolucion %in% c("tasa_temporalidad", "tasa_parcialidad")
      datos <- if (es_calidad) {
        req(datos_calidad())
        datos_calidad() |>
          dplyr::filter(region == filtros$territorio_ref(), edad == "total", sector == "Total", sexo %in% c("Hombres", "Mujeres"))
      } else {
        req(datos_edad())
        datos_edad() |>
          dplyr::filter(region == filtros$territorio_ref(), edad == "total", sexo %in% c("Hombres", "Mujeres"))
      }
      etiqueta <- names(OPCIONES_INDICADOR_BRECHA)[OPCIONES_INDICADOR_BRECHA == input$indicador_evolucion]

      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = .data[[input$indicador_evolucion]], color = sexo)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Hombres" = "#0B5FA5", "Mujeres" = "#B3261E")) +
        ggplot2::labs(x = NULL, y = etiqueta, color = NULL,
                      title = sprintf("Evoluci\u00f3n de %s por sexo \u2014 %s", tolower(etiqueta), filtros$territorio_ref())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

  })
}
