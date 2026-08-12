# -----------------------------------------------------------------------------
# mod_desempleo.R
#
# Pestaña "Desempleo" + "Paro de larga duración" (Nivel 2, Oleada 1).
# Responde a: ¿quién soporta el paro? ¿cuánto tiempo llevan parados?
#   - Sexo / edad  -> epa_edad   (valor_par, tasa_par)
#   - Nacionalidad -> epa_nac
#   - Educación    -> epa_form
#   - Duración     -> epa_paro_larga (tramos vía ITBU, ya calculados)
# Mismo patrón de "Desglosar por" que mod_empleo.R para no duplicar la UI de
# ranking/evolución tres veces.
# -----------------------------------------------------------------------------

OPCIONES_DESGLOSE_DESEMPLEO <- c(
  "Edad"            = "edad",
  "Nacionalidad"    = "nac",
  "Nivel educativo" = "form"
)

ORDEN_TRAMOS_PARO_LARGA <- c("< 3 meses", "3 a 6 meses", "6 meses a 1 año", "1 a 2 años", "2 años o más")

mod_desempleo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(3, uiOutput(ns("kpi_parados"))),
      column(3, uiOutput(ns("kpi_tasa_paro"))),
      column(3, uiOutput(ns("kpi_larga_duracion")))
    ),
    br(),
    tabsetPanel(
      id = ns("subtabs"),
      tabPanel(
        "\u00bfQui\u00e9n soporta el paro?",
        br(),
        fluidRow(column(4, selectInput(ns("desglose"), "Desglosar por", choices = OPCIONES_DESGLOSE_DESEMPLEO))),
        fluidRow(
          column(6, plotly::plotlyOutput(ns("plot_ranking"), height = "420px")),
          column(6, plotly::plotlyOutput(ns("plot_evolucion_desglose"), height = "420px"))
        ),
        uiOutput(ns("aviso_desglose"))
      ),
      tabPanel(
        "Paro de larga duraci\u00f3n",
        br(),
        fluidRow(
          column(6, plotly::plotlyOutput(ns("plot_tramos"), height = "420px")),
          column(6, plotly::plotlyOutput(ns("plot_evolucion_larga"), height = "420px"))
        ),
        uiOutput(ns("aviso_larga"))
      )
    )
  )
}

#' @param id id del módulo
#' @param datos_edad,datos_nac,datos_form,datos_paro_larga reactives con las
#'   tablas estrella ya cargadas
#' @param filtros lista de reactives compartidos desde app.R
mod_desempleo_server <- function(id, datos_edad, datos_nac, datos_form, datos_paro_larga, filtros) {
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

    # --- KPIs -----------------------------------------------------------
    fila_parados <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      s <- datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), edad == "total", sexo == filtros$sexo()) |>
        variacion_interanual("valor_par") |>
        variacion_intertrimestral("valor_par") |>
        anotar_fiabilidad("par") |>
        dplyr::filter(periodo == filtros$periodo())
      if (nrow(s) != 1) return(NULL)
      s
    })

    output$kpi_parados <- renderUI({
      f <- fila_parados()
      req(f)
      kpi_card(
        titulo = "Parados", valor = round(f$valor_par),
        var_intertrim = f$var_intertrim_pct, var_interanual = f$var_interanual_pct,
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    output$kpi_tasa_paro <- renderUI({
      f <- fila_parados()
      req(f)
      kpi_card(titulo = "Tasa de paro", valor = round(f$tasa_par, 1), unidad = "%")
    })

    larga_duracion_pct <- reactive({
      req(datos_paro_larga(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      base <- datos_paro_larga() |>
        dplyr::filter(region == filtros$territorio_ref(), periodo == filtros$periodo(), sexo == filtros$sexo())
      total <- base |> dplyr::filter(duracion_paro == "Total") |> dplyr::pull(valor_par)
      larga <- base |>
        dplyr::filter(duracion_paro %in% c("1 a 2 años", "2 años o más")) |>
        dplyr::summarise(v = sum(valor_par, na.rm = TRUE)) |>
        dplyr::pull(v)
      if (length(total) != 1 || total == 0) return(NA_real_)
      100 * larga / total
    })

    output$kpi_larga_duracion <- renderUI({
      pct <- larga_duracion_pct()
      kpi_card(titulo = "Paro de larga duraci\u00f3n (\u22651 a\u00f1o)", unidad = "%",
                valor = if (!is.na(pct)) round(pct, 1) else NA)
    })

    # --- ¿Quién soporta el paro? (desglose por edad/nac/form) ---------------
    tabla_desglose <- reactive({
      switch(input$desglose,
        edad = list(df = datos_edad(), col_dim = "edad", excluir = "total"),
        nac  = list(df = datos_nac(),  col_dim = "nac",  excluir = "EX"),
        form = list(df = datos_form(), col_dim = "form", excluir = "Total")
      )
    })

    datos_desglose_actual <- reactive({
      info <- tabla_desglose()
      req(info$df, filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      col <- info$col_dim
      info$df |>
        dplyr::filter(
          region == filtros$territorio_ref(), periodo == filtros$periodo(), sexo == filtros$sexo(),
          .data[[col]] != info$excluir, !is.na(.data[[col]])
        ) |>
        dplyr::rename(categoria = dplyr::all_of(col)) |>
        anotar_fiabilidad("par")
    })

    output$plot_ranking <- plotly::renderPlotly({
      df <- datos_desglose_actual()
      req(nrow(df) > 0)
      etiqueta_dim <- names(OPCIONES_DESGLOSE_DESEMPLEO)[OPCIONES_DESGLOSE_DESEMPLEO == input$desglose]
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = reorder(categoria, tasa_par), y = tasa_par,
                     text = sprintf("%s: %.1f%%", categoria, tasa_par))
      ) +
        ggplot2::geom_col(fill = "#B3261E") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Tasa de paro (%)",
                      title = sprintf("Tasa de paro por %s \u2014 %s", tolower(etiqueta_dim), formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_evolucion_desglose <- plotly::renderPlotly({
      info <- tabla_desglose()
      req(info$df, filtros$territorio_ref(), filtros$sexo())
      col <- info$col_dim
      datos <- info$df |>
        dplyr::filter(
          region == filtros$territorio_ref(), sexo == filtros$sexo(),
          .data[[col]] != info$excluir, !is.na(.data[[col]])
        ) |>
        dplyr::rename(categoria = dplyr::all_of(col))
      etiqueta_dim <- names(OPCIONES_DESGLOSE_DESEMPLEO)[OPCIONES_DESGLOSE_DESEMPLEO == input$desglose]
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = tasa_par, color = categoria)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = "Tasa de paro (%)", color = etiqueta_dim,
                      title = sprintf("Evoluci\u00f3n de la tasa de paro por %s", tolower(etiqueta_dim))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso_desglose <- renderUI({
      df <- datos_desglose_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(categoria)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

    # --- Paro de larga duración ----------------------------------------
    tramos_actual <- reactive({
      req(datos_paro_larga(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      datos_paro_larga() |>
        dplyr::filter(
          region == filtros$territorio_ref(), periodo == filtros$periodo(), sexo == filtros$sexo(),
          duracion_paro %in% ORDEN_TRAMOS_PARO_LARGA
        ) |>
        dplyr::mutate(duracion_paro = factor(duracion_paro, levels = ORDEN_TRAMOS_PARO_LARGA)) |>
        anotar_fiabilidad("par")
    })

    output$plot_tramos <- plotly::renderPlotly({
      df <- tramos_actual()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = duracion_paro, y = valor_par,
                                              text = sprintf("%s: %s", duracion_paro,
                                                              format(round(valor_par), big.mark = ".", decimal.mark = ",")))) +
        ggplot2::geom_col(fill = "#B25E00") +
        ggplot2::labs(x = NULL, y = "Parados",
                      title = sprintf("Parados por tiempo de b\u00fasqueda \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$aviso_larga <- renderUI({
      df <- tramos_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(duracion_paro)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(style = "font-size:0.75em; color:#B3261E;",
             sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", ")))
    })

    output$plot_evolucion_larga <- plotly::renderPlotly({
      req(datos_paro_larga(), filtros$territorio_ref(), filtros$sexo())
      datos <- datos_paro_larga() |>
        dplyr::filter(region == filtros$territorio_ref(), sexo == filtros$sexo()) |>
        dplyr::group_by(periodo) |>
        dplyr::summarise(
          total = sum(valor_par[duracion_paro == "Total"], na.rm = TRUE),
          larga = sum(valor_par[duracion_paro %in% c("1 a 2 años", "2 años o más")], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(pct_larga = dplyr::if_else(total > 0, 100 * larga / total, NA_real_))
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = pct_larga)) +
        ggplot2::geom_line(color = "#B25E00") +
        ggplot2::labs(x = NULL, y = "% de parados \u2265 1 a\u00f1o buscando empleo",
                      title = "Evoluci\u00f3n del paro de larga duraci\u00f3n") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

  })
}
