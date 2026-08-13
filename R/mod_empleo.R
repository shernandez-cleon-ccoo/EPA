# -----------------------------------------------------------------------------
# mod_empleo.R
#
# Pestaña "Empleo" (Nivel 2, Oleada 1). Responde a: ¿dónde se crea empleo y
# para quién? Combina varias tablas estrella, cada una con su propia
# granularidad, seleccionables desde un único selector "Desglosar por" para
# no repetir la misma UI de gráfico 4 veces:
#   - Sexo / edad  -> epa_edad   (valor_ocu, con tasas)
#   - Sector       -> epa_sector (solo valor_ocu; ver nota en epa_helpers.R)
#   - Nacionalidad -> epa_nac    (ES / UE / no_UE, más el agregado "EX")
#   - Educación    -> epa_form   (7 niveles de epa_helpers::nforma_label())
# "Asalariados vs autónomos" es una aproximación: epa_calidad trae
# valor_asal (asalariados en sentido estricto, SITU 07/08); autónomos +
# otras situaciones profesionales se aproxima como ocupados totales menos
# asalariados. Se marca explícitamente como aproximación en la UI.
# -----------------------------------------------------------------------------

OPCIONES_DESGLOSE_EMPLEO <- c(
  "Sector de actividad" = "sector",
  "Nacionalidad"         = "nac",
  "Nivel educativo"      = "form",
  "Edad"                 = "edad"
)

mod_empleo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(3, uiOutput(ns("kpi_ocupados"))),
      column(3, uiOutput(ns("kpi_asalariados"))),
      column(3, uiOutput(ns("kpi_autonomos")))
    ),
    br(),
    fluidRow(
      column(4, selectInput(ns("desglose"), "Desglosar por", choices = OPCIONES_DESGLOSE_EMPLEO))
    ),
    fluidRow(
      column(6, plotly::plotlyOutput(ns("plot_contribucion"), height = "420px")),
      column(6, plotly::plotlyOutput(ns("plot_evolucion_desglose"), height = "420px"))
    ),
    uiOutput(ns("aviso_desglose")),
    tags$p(
      style = "font-size:0.75em; color:#888;",
      "\"Autónomos\" es una aproximación (ocupados totales menos asalariados en ",
      "sentido estricto); incluye también ayudas familiares y otras situaciones ",
      "profesionales minoritarias no separables con los microdatos estándar."
    )
  )
}

#' @param id id del módulo
#' @param datos_edad,datos_sector,datos_nac,datos_form,datos_calidad reactives
#'   con las tablas estrella ya cargadas
#' @param filtros lista de reactives compartidos desde app.R
mod_empleo_server <- function(id, datos_edad, datos_sector, datos_nac, datos_form, datos_calidad, filtros) {
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

    # --- KPIs: ocupados totales (epa_edad) + asalariados/autónomos (epa_calidad,
    # totales de edad/sector para no mezclar universos) --------------------
    fila_ocupados <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      s <- datos_edad() |>
        dplyr::filter(region == filtros$territorio_ref(), edad == "total", sexo == filtros$sexo()) |>
        variacion_interanual("valor_ocu") |>
        variacion_intertrimestral("valor_ocu") |>
        anotar_fiabilidad("ocu") |>
        dplyr::filter(periodo == filtros$periodo())
      if (nrow(s) != 1) return(NULL)
      s
    })

    fila_calidad_actual <- reactive({
      req(datos_calidad(), filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      f <- datos_calidad() |>
        dplyr::filter(
          region == filtros$territorio_ref(), periodo == filtros$periodo(),
          sexo == filtros$sexo(), edad == "total", sector == "Total"
        )
      if (nrow(f) != 1) return(NULL)
      f
    })

    output$kpi_ocupados <- renderUI({
      f <- fila_ocupados()
      req(f)
      kpi_card(
        titulo = "Ocupados", valor = round(f$valor_ocu),
        var_intertrim = f$var_intertrim_pct, var_interanual = f$var_interanual_pct,
        fiabilidad = f$fiabilidad, n_muestra = f$n_muestra
      )
    })

    output$kpi_asalariados <- renderUI({
      f <- fila_calidad_actual()
      kpi_card(titulo = "Asalariados", valor = if (!is.null(f)) round(f$valor_asal) else NA)
    })

    output$kpi_autonomos <- renderUI({
      f <- fila_calidad_actual()
      valor <- if (!is.null(f)) round(f$valor_ocu - f$valor_asal) else NA
      kpi_card(titulo = "Aut\u00f3nomos y otros (aprox.)", valor = valor)
    })

    # --- Tabla + variable de la dimensión de desglose elegida ---------------
    tabla_desglose <- reactive({
      switch(input$desglose,
        sector = list(df = datos_sector(), col_dim = "sector", excluir = "Total"),
        nac    = list(df = datos_nac(),    col_dim = "nac",    excluir = "EX"),
        form   = list(df = datos_form(),   col_dim = "form",   excluir = "Total"),
        edad   = list(df = datos_edad(),   col_dim = "edad",   excluir = "total")
      )
    })

    datos_desglose_actual <- reactive({
      info <- tabla_desglose()
      req(info$df, filtros$territorio_ref(), filtros$periodo(), filtros$sexo())
      col <- info$col_dim
      info$df |>
        dplyr::filter(
          region == filtros$territorio_ref(),
          periodo == filtros$periodo(),
          sexo == filtros$sexo(),
          .data[[col]] != info$excluir,
          !is.na(.data[[col]])
        ) |>
        dplyr::rename(categoria = dplyr::all_of(col)) |>
        anotar_fiabilidad("ocu")
    })

    output$plot_contribucion <- plotly::renderPlotly({
      df <- datos_desglose_actual()
      req(nrow(df) > 0)
      etiqueta_dim <- names(OPCIONES_DESGLOSE_EMPLEO)[OPCIONES_DESGLOSE_EMPLEO == input$desglose]
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = reorder(categoria, valor_ocu), y = valor_ocu,
                     text = sprintf("%s: %s", categoria, format(round(valor_ocu), big.mark = ".", decimal.mark = ",")))
      ) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Ocupados",
                      title = sprintf("Ocupados por %s \u2014 %s", tolower(etiqueta_dim), formato_trimestre(filtros$periodo()))) +
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
      etiqueta_dim <- names(OPCIONES_DESGLOSE_EMPLEO)[OPCIONES_DESGLOSE_EMPLEO == input$desglose]
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = valor_ocu, color = categoria)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = "Ocupados", color = etiqueta_dim,
                      title = sprintf("Evoluci\u00f3n por %s", tolower(etiqueta_dim))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$aviso_desglose <- renderUI({
      df <- datos_desglose_actual()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(categoria)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(
        style = "font-size:0.75em; color:#B3261E;",
        sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", "))
      )
    })

  })
}
