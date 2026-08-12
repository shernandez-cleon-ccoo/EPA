# -----------------------------------------------------------------------------
# mod_calidad.R
#
# Pestañas "Calidad del empleo" + "Temporalidad" + "Jornada y parcialidad"
# (Nivel 2, Oleada 1). Las tres viven en un único módulo porque comparten la
# misma tabla estrella (epa_calidad, ver epa_helpers.R::agregar_calidad_base)
# y el mismo filtro local de desglose (edad/sector) -- separarlas en 3
# ficheros distintos solo duplicaría la UI de gráfico sin beneficio real
# (mismo criterio que ya se aplicó al fusionar Comparador/Explorador en la
# arquitectura).
#
# Reforma laboral 2021/2022: Real Decreto-ley 32/2021, en vigor formalmente
# desde el 31/12/2021 pero con aplicación efectiva generalizada a partir del
# 30/03/2022 (fin del periodo transitorio de adaptación de contratos). Se
# marca la línea vertical en el trimestre T1-2022 (2022-03-31, fecha de fin
# de ese trimestre en la columna `periodo`) como aproximación razonable del
# punto de inflexión, documentada aquí para que quien lea el gráfico sepa
# que es una fecha de referencia, no un corte exacto.
# -----------------------------------------------------------------------------

FECHA_REFORMA_LABORAL <- as.Date("2022-03-31")
PERIODO_REFERENCIA_2019 <- as.Date("2019-12-31")

OPCIONES_DESGLOSE_CALIDAD <- c("Sexo" = "__sexo__", "Edad" = "edad", "Sector" = "sector")

mod_calidad_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    tabsetPanel(
      id = ns("subtabs"),
      tabPanel(
        "Calidad del empleo",
        br(),
        tags$p("Comparativa actual vs. interanual vs. pre-pandemia (T4 2019). Semáforo: verde = mejora vs. hace un año, rojo = empeora."),
        DT::dataTableOutput(ns("tabla_calidad"))
      ),
      tabPanel(
        "Temporalidad",
        br(),
        fluidRow(column(4, selectInput(ns("desglose_temp"), "Desglosar por", choices = OPCIONES_DESGLOSE_CALIDAD))),
        plotly::plotlyOutput(ns("plot_temporalidad_historico"), height = "380px"),
        tags$p(style = "font-size:0.75em; color:#888;",
               "La línea vertical marca T1-2022, aplicación efectiva de la reforma laboral (RDL 32/2021)."),
        br(),
        plotly::plotlyOutput(ns("plot_temporalidad_colectivo"), height = "380px"),
        uiOutput(ns("aviso_temporalidad"))
      ),
      tabPanel(
        "Jornada y parcialidad",
        br(),
        fluidRow(
          column(3, uiOutput(ns("kpi_parcialidad"))),
          column(3, uiOutput(ns("kpi_parcialidad_involuntaria")))
        ),
        br(),
        fluidRow(column(4, selectInput(ns("desglose_jor"), "Desglosar por", choices = OPCIONES_DESGLOSE_CALIDAD))),
        fluidRow(
          column(6, plotly::plotlyOutput(ns("plot_parcialidad_colectivo"), height = "400px")),
          column(6, plotly::plotlyOutput(ns("plot_parcialidad_historico"), height = "400px"))
        ),
        uiOutput(ns("aviso_jornada"))
      )
    )
  )
}

#' @param id id del módulo
#' @param datos_calidad reactive() que devuelve la tabla epa_calidad ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_calidad_server <- function(id, datos_calidad, filtros) {
  moduleServer(id, function(input, output, session) {

    output$resumen_seleccion <- renderUI({
      req(filtros$periodo())
      tags$div(
        style = "color:#555; font-size:0.9em;",
        resumen_filtros(
          territorio = filtros$territorio_ref(),
          periodo_label = formato_trimestre(filtros$periodo()),
          sexo = filtros$sexo()
        )
      )
    })

    # Fila "total" (edad=total, sexo=filtro global, sector=Total) del
    # territorio de referencia, para toda la serie histórica.
    serie_total <- reactive({
      req(datos_calidad(), filtros$territorio_ref(), filtros$sexo())
      datos_calidad() |>
        dplyr::filter(
          region == filtros$territorio_ref(), sexo == filtros$sexo(),
          edad == "total", sector == "Total"
        ) |>
        dplyr::arrange(periodo)
    })

    # -----------------------------------------------------------------------
    # Calidad del empleo: tabla comparativa con semáforo
    # -----------------------------------------------------------------------
    tabla_calidad_df <- reactive({
      req(serie_total(), filtros$periodo())
      s <- serie_total() |>
        variacion_interanual("tasa_temporalidad") |>
        variacion_desde("tasa_temporalidad", PERIODO_REFERENCIA_2019)
      fila <- s |> dplyr::filter(periodo == filtros$periodo())
      req(nrow(fila) == 1)

      construir_indicador <- function(col_valor, etiqueta) {
        serie <- serie_total() |>
          variacion_interanual(col_valor) |>
          variacion_desde(col_valor, PERIODO_REFERENCIA_2019)
        f <- serie |> dplyr::filter(periodo == filtros$periodo())
        tibble::tibble(
          Indicador = etiqueta,
          Actual = round(f[[col_valor]], 1),
          `Var. interanual (pp)` = round(f$var_interanual_abs, 1),
          `Var. vs 2019T4 (pp)` = round(f$var_desde_abs, 1)
        )
      }

      dplyr::bind_rows(
        construir_indicador("tasa_temporalidad", "Tasa de temporalidad"),
        construir_indicador("tasa_fijo_discontinuo", "Fijos discontinuos / indefinidos"),
        construir_indicador("tasa_parcialidad", "Tasa de parcialidad"),
        construir_indicador("tasa_parcialidad_involuntaria", "Parcialidad involuntaria")
      )
    })

    output$tabla_calidad <- DT::renderDataTable({
      df <- tabla_calidad_df()
      req(nrow(df) > 0)
      DT::datatable(df, rownames = FALSE, options = list(dom = "t", paging = FALSE)) |>
        DT::formatStyle(
          "Var. interanual (pp)",
          color = DT::styleInterval(0, c("#1B7A3D", "#B3261E"))
        ) |>
        DT::formatStyle(
          "Var. vs 2019T4 (pp)",
          color = DT::styleInterval(0, c("#1B7A3D", "#B3261E"))
        )
    })

    # -----------------------------------------------------------------------
    # Datos de desglose compartidos entre Temporalidad y Jornada (evita
    # repetir la misma lógica de filtro/rename dos veces)
    # -----------------------------------------------------------------------
    datos_desglose <- function(col_dim) {
      req(datos_calidad(), filtros$territorio_ref(), filtros$periodo())
      if (identical(col_dim, "__sexo__")) {
        return(
          datos_calidad() |>
            dplyr::filter(
              region == filtros$territorio_ref(), periodo == filtros$periodo(),
              edad == "total", sector == "Total", sexo != "total"
            ) |>
            dplyr::rename(categoria = sexo)
        )
      }
      excluir <- if (identical(col_dim, "edad")) "total" else "Total"
      datos_calidad() |>
        dplyr::filter(
          region == filtros$territorio_ref(), periodo == filtros$periodo(), sexo == filtros$sexo(),
          .data[[col_dim]] != excluir, !is.na(.data[[col_dim]])
        ) |>
        dplyr::rename(categoria = dplyr::all_of(col_dim))
    }

    # -----------------------------------------------------------------------
    # Temporalidad
    # -----------------------------------------------------------------------
    output$plot_temporalidad_historico <- plotly::renderPlotly({
      req(serie_total())
      p <- ggplot2::ggplot(serie_total(), ggplot2::aes(x = periodo, y = tasa_temporalidad)) +
        ggplot2::geom_line(color = "#0B5FA5") +
        ggplot2::geom_vline(xintercept = FECHA_REFORMA_LABORAL, linetype = "dashed", color = "#888888") +
        ggplot2::labs(x = NULL, y = "Tasa de temporalidad (%)",
                      title = sprintf("Evoluci\u00f3n de la temporalidad \u2014 %s", filtros$territorio_ref())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    datos_temp_colectivo <- reactive(datos_desglose(input$desglose_temp) |> anotar_fiabilidad("asal"))

    output$plot_temporalidad_colectivo <- plotly::renderPlotly({
      df <- datos_temp_colectivo()
      req(nrow(df) > 0)
      etiqueta_dim <- names(OPCIONES_DESGLOSE_CALIDAD)[OPCIONES_DESGLOSE_CALIDAD == input$desglose_temp]
      p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(categoria, tasa_temporalidad), y = tasa_temporalidad,
                                              text = sprintf("%s: %.1f%%", categoria, tasa_temporalidad))) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Tasa de temporalidad (%)",
                      title = sprintf("Temporalidad por %s \u2014 %s", tolower(etiqueta_dim), formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$aviso_temporalidad <- renderUI({
      df <- datos_temp_colectivo()
      req(nrow(df) > 0)
      # anotar_fiabilidad() usa n_<universo>; "asal" no existe como columna
      # n_asal directamente vía ese helper genérico (espera "n_asal", que sí
      # existe en epa_calidad), así que esto sí resuelve bien.
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(categoria)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

    # -----------------------------------------------------------------------
    # Jornada y parcialidad
    # -----------------------------------------------------------------------
    fila_parcialidad_actual <- reactive({
      req(serie_total(), filtros$periodo())
      f <- serie_total() |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1) return(NULL)
      f
    })

    output$kpi_parcialidad <- renderUI({
      f <- fila_parcialidad_actual()
      kpi_card(titulo = "Tasa de parcialidad", unidad = "%",
                valor = if (!is.null(f)) round(f$tasa_parcialidad, 1) else NA)
    })

    output$kpi_parcialidad_involuntaria <- renderUI({
      f <- fila_parcialidad_actual()
      kpi_card(titulo = "Parcialidad involuntaria", unidad = "%",
                valor = if (!is.null(f)) round(f$tasa_parcialidad_involuntaria, 1) else NA)
    })

    datos_jor_colectivo <- reactive(datos_desglose(input$desglose_jor) |> anotar_fiabilidad("ocu"))

    output$plot_parcialidad_colectivo <- plotly::renderPlotly({
      df <- datos_jor_colectivo()
      req(nrow(df) > 0)
      etiqueta_dim <- names(OPCIONES_DESGLOSE_CALIDAD)[OPCIONES_DESGLOSE_CALIDAD == input$desglose_jor]
      datos <- df |>
        dplyr::select(categoria, tasa_parcialidad, tasa_parcialidad_involuntaria) |>
        tidyr::pivot_longer(-categoria, names_to = "serie", values_to = "valor") |>
        dplyr::mutate(serie = dplyr::recode(serie,
          tasa_parcialidad = "Parcialidad total",
          tasa_parcialidad_involuntaria = "de la que involuntaria"
        ))
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = categoria, y = valor, fill = serie)) +
        ggplot2::geom_col(position = "identity", alpha = 0.85) +
        ggplot2::scale_fill_manual(values = c("Parcialidad total" = "#B7C4D6", "de la que involuntaria" = "#B25E00")) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "%", fill = NULL,
                      title = sprintf("Parcialidad por %s \u2014 %s", tolower(etiqueta_dim), formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$plot_parcialidad_historico <- plotly::renderPlotly({
      req(serie_total())
      datos <- serie_total() |>
        dplyr::select(periodo, tasa_parcialidad, tasa_parcialidad_involuntaria) |>
        tidyr::pivot_longer(-periodo, names_to = "serie", values_to = "valor") |>
        dplyr::mutate(serie = dplyr::recode(serie,
          tasa_parcialidad = "Parcialidad total",
          tasa_parcialidad_involuntaria = "de la que involuntaria"
        ))
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = valor, color = serie)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Parcialidad total" = "#0B5FA5", "de la que involuntaria" = "#B25E00")) +
        ggplot2::labs(x = NULL, y = "%", color = NULL, title = "Evoluci\u00f3n de la parcialidad") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso_jornada <- renderUI({
      df <- datos_jor_colectivo()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(categoria)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

  })
}
