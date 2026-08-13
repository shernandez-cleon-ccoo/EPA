# -----------------------------------------------------------------------------
# mod_mercado_laboral.R
#
# Pestaña "Mercado laboral" (Nivel 2, Oleada 1). Responde a: ¿se reduce el
# paro por más empleo o por menos activos? ¿cómo envejece la fuerza de
# trabajo? Usa solo epa_edad (region x edad x sexo), que ya trae
# valor_act/ocu/par/ina y la banda de edad necesaria para todo lo de aquí --
# no hace falta ninguna tabla estrella nueva.
#
# Nota de universo: la EPA solo cubre población de 16+ años (ver la
# limitación ya documentada en epa_helpers.R/build_epa_tablas.R), así que
# toda la lectura de "estructura demográfica activa" es sobre población
# activa/ocupada 16+, no sobre población total del territorio.
# -----------------------------------------------------------------------------

# Orden de las bandas de edad tal y como las etiqueta edad_banda() en
# epa_helpers.R, para que las barras y líneas no salgan en orden alfabético.
ORDEN_BANDAS_EDAD <- c("16-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+")

mod_mercado_laboral_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("resumen_seleccion")),
    br(),
    fluidRow(
      column(4, uiOutput(ns("kpi_ratio_reemplazo"))),
      column(4, uiOutput(ns("kpi_peso_55mas")))
    ),
    br(),
    tabsetPanel(
      id = ns("subtabs"),
      tabPanel(
        "\u00bfPor qu\u00e9 sube o baja el paro?",
        br(),
        radioButtons(ns("horizonte"), NULL,
                     choices = c("Interanual (vs. mismo trimestre a\u00f1o anterior)" = "interanual",
                                 "Intertrimestral (vs. trimestre anterior)" = "intertrimestral"),
                     selected = "interanual", inline = TRUE),
        plotly::plotlyOutput(ns("plot_descomposicion"), height = "420px"),
        tags$p(
          style = "font-size:0.8em; color:#666;",
          "Identidad contable: parados = activos \u2212 ocupados. El efecto \"Activos\" ",
          "positivo significa que entra m\u00e1s gente al mercado laboral (sube el paro, ",
          "a igualdad de empleo); el efecto \"Ocupados\" positivo significa que se ",
          "crea empleo suficiente para absorber a esos activos (baja el paro)."
        ),
        hr(),
        tags$h4("De d\u00f3nde vienen los parados actuales (dato real, no estimado)"),
        tags$p(
          style = "font-size:0.8em; color:#666;",
          "Fuente: Estad\u00edstica de Flujos de la Poblaci\u00f3n Activa (EFPA) del INE, no ",
          "reconstruida por este dashboard \u2014 el INE la calcula enlazando individuos ",
          "reales entre trimestres consecutivos (panel rotante de la EPA) y la publica ",
          "ya hecha. A diferencia del gr\u00e1fico de arriba (una identidad contable sobre ",
          "agregados), esto s\u00ed distingue si el aumento del paro viene de gente que ",
          "realmente perdi\u00f3 su empleo el trimestre pasado o de gente que entra desde ",
          "la inactividad. \u26A0 Sin desglose por sexo (la tabla del INE por CCAA no lo trae)."
        ),
        fluidRow(column(3, uiOutput(ns("kpi_flujo_desde_ocupados")))),
        br(),
        plotly::plotlyOutput(ns("plot_flujos_origen"), height = "380px")
      ),
      tabPanel(
        "Evoluci\u00f3n del mercado laboral",
        br(),
        plotly::plotlyOutput(ns("plot_evolucion"), height = "440px")
      ),
      tabPanel(
        "Estructura de edad de la fuerza de trabajo",
        br(),
        fluidRow(
          column(6, plotly::plotlyOutput(ns("plot_estructura_edad"), height = "420px")),
          column(6, plotly::plotlyOutput(ns("plot_envejecimiento_tiempo"), height = "420px"))
        ),
        uiOutput(ns("aviso_estructura"))
      )
    )
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param datos_flujos reactive() que devuelve la tabla epa_flujos (EFPA) ya cargada
#' @param filtros lista de reactives compartidos desde app.R
mod_mercado_laboral_server <- function(id, datos_edad, datos_flujos, filtros) {
  moduleServer(id, function(input, output, session) {

    # --- Serie completa (todos los periodos) del territorio de referencia,
    # totales de edad/sexo -- base para descomposición y evolución ----------
    serie_total <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      datos_edad() |>
        dplyr::filter(
          region == filtros$territorio_ref(),
          edad == "total",
          sexo == filtros$sexo()
        ) |>
        dplyr::arrange(periodo)
    })

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

    # --- Descomposición del cambio en parados -------------------------------
    descomposicion_df <- reactive({
      req(serie_total())
      n_lag <- if (identical(input$horizonte, "intertrimestral")) 1 else 4
      descomposicion_paro(serie_total(), n_lag = n_lag)
    })

    fila_descomposicion <- reactive({
      req(descomposicion_df(), filtros$periodo())
      f <- descomposicion_df() |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1 || is.na(f$var_parados_total)) return(NULL)
      f
    })

    output$plot_descomposicion <- plotly::renderPlotly({
      f <- fila_descomposicion()
      shiny::validate(shiny::need(
        !is.null(f),
        "No hay trimestre de comparación suficiente para descomponer el cambio (falta histórico)."
      ))
      etiqueta_horizonte <- if (identical(input$horizonte, "intertrimestral")) "trimestre anterior" else "mismo trimestre año anterior"
      datos <- tibble::tibble(
        concepto = factor(
          c("Efecto activos", "Efecto ocupados", "Cambio total en parados"),
          levels = c("Efecto activos", "Efecto ocupados", "Cambio total en parados")
        ),
        valor = c(f$efecto_activos, f$efecto_ocupados, f$var_parados_total)
      )
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = concepto, y = valor, fill = valor >= 0,
                                                 text = sprintf("%s: %+.0f", concepto, valor))) +
        ggplot2::geom_col(width = 0.55) +
        ggplot2::geom_hline(yintercept = 0, color = "#888888") +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#B3261E", "FALSE" = "#1B7A3D"), guide = "none") +
        ggplot2::labs(x = NULL, y = "Personas",
                      title = sprintf("Descomposici\u00f3n del cambio en parados \u2014 %s (%s)",
                                       formato_trimestre(filtros$periodo()), etiqueta_horizonte)) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    # --- De dónde vienen los parados actuales (EFPA real, no estimada) ------
    ORDEN_ORIGEN_PARO <- c("Ocupados", "Parados", "Inactivos", "No consta")

    fila_flujos_actual <- reactive({
      req(datos_flujos(), filtros$territorio_ref(), filtros$periodo())
      datos_flujos() |>
        dplyr::filter(
          region == filtros$territorio_ref(), periodo == filtros$periodo(),
          situacion_actual == "Parados", situacion_anterior %in% ORDEN_ORIGEN_PARO
        )
    })

    output$plot_flujos_origen <- plotly::renderPlotly({
      df <- fila_flujos_actual()
      shiny::validate(shiny::need(
        nrow(df) > 0,
        "No disponible para este trimestre/territorio en la EFPA (la serie empieza en 2014, o la muestra es insuficiente para este cruce)."
      ))
      df <- df |> dplyr::mutate(situacion_anterior = factor(situacion_anterior, levels = ORDEN_ORIGEN_PARO))
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = reorder(situacion_anterior, valor), y = valor,
        text = sprintf("%s el trimestre anterior: %s", situacion_anterior, format(round(valor), big.mark = ".", decimal.mark = ","))
      )) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Parados actuales",
                      title = sprintf("Parados de %s seg\u00fan su situaci\u00f3n el trimestre anterior \u2014 %s",
                                       filtros$territorio_ref(), formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$kpi_flujo_desde_ocupados <- renderUI({
      df <- fila_flujos_actual()
      total <- sum(df$valor, na.rm = TRUE)
      desde_ocu <- df$valor[df$situacion_anterior == "Ocupados"]
      pct <- if (length(desde_ocu) == 1 && total > 0) round(100 * desde_ocu / total, 1) else NA
      kpi_card(titulo = "Parados que eran ocupados hace un trimestre", unidad = "%", valor = pct)
    })


    output$plot_evolucion <- plotly::renderPlotly({
      req(serie_total())
      datos <- serie_total() |>
        dplyr::select(periodo, valor_act, valor_ocu, valor_par, valor_ina) |>
        tidyr::pivot_longer(-periodo, names_to = "serie", values_to = "valor") |>
        dplyr::mutate(serie = dplyr::recode(serie,
          valor_act = "Activos", valor_ocu = "Ocupados",
          valor_par = "Parados", valor_ina = "Inactivos"
        ))
      p <- ggplot2::ggplot(datos, ggplot2::aes(x = periodo, y = valor, color = serie)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c(
          "Activos" = "#0B5FA5", "Ocupados" = "#1B7A3D",
          "Parados" = "#B3261E", "Inactivos" = "#888888"
        )) +
        ggplot2::labs(x = NULL, y = "Personas", color = NULL,
                      title = sprintf("Activos, ocupados, parados e inactivos \u2014 %s", filtros$territorio_ref())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    # --- Estructura de edad de la fuerza de trabajo (activos), trimestre
    # seleccionado, con aviso de fiabilidad por banda --------------------
    estructura_edad_df <- reactive({
      req(datos_edad(), filtros$periodo(), filtros$sexo(), filtros$territorio_ref())
      datos_edad() |>
        dplyr::filter(
          region == filtros$territorio_ref(),
          periodo == filtros$periodo(),
          sexo == filtros$sexo(),
          edad %in% ORDEN_BANDAS_EDAD
        ) |>
        dplyr::mutate(edad = factor(edad, levels = ORDEN_BANDAS_EDAD)) |>
        anotar_fiabilidad("act")
    })

    output$plot_estructura_edad <- plotly::renderPlotly({
      df <- estructura_edad_df()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = edad, y = valor_act,
                                              text = sprintf("%s: %s activos", edad,
                                                              format(round(valor_act), big.mark = ".", decimal.mark = ",")))) +
        ggplot2::geom_col(fill = "#0B5FA5") +
        ggplot2::labs(x = NULL, y = "Activos",
                      title = sprintf("Estructura de edad de los activos \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$aviso_estructura <- renderUI({
      df <- estructura_edad_df()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(edad)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(
        style = "font-size:0.75em; color:#B3261E;",
        sprintf("\u26D4 Muestra insuficiente este trimestre en: %s.", paste(insuficientes, collapse = ", "))
      )
    })

    # --- Ratio de reemplazo y peso de 55+ EN EL TIEMPO, para el territorio
    # de referencia (el ranking entre CCAA en un trimestre dado ya vive en
    # mod_territorio.R; aquí es la evolución de un único territorio) --------
    envejecimiento_tiempo_df <- reactive({
      req(datos_edad(), filtros$territorio_ref(), filtros$sexo())
      base <- datos_edad() |>
        dplyr::filter(
          region == filtros$territorio_ref(),
          sexo == filtros$sexo(),
          edad %in% ORDEN_BANDAS_EDAD
        )
      totales_periodo <- base |>
        dplyr::group_by(periodo) |>
        dplyr::summarise(valor_act_total = sum(valor_act, na.rm = TRUE), .groups = "drop")

      base |>
        dplyr::mutate(grupo = dplyr::case_when(
          edad %in% c("16-19", "20-24") ~ "entra",
          edad == "55-64"               ~ "sale_5564",
          edad == "65+"                 ~ "sale_65mas",
          TRUE ~ NA_character_
        )) |>
        dplyr::filter(!is.na(grupo)) |>
        dplyr::group_by(periodo, grupo) |>
        dplyr::summarise(valor_act = sum(valor_act, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = grupo, values_from = valor_act, values_fill = 0) |>
        dplyr::left_join(totales_periodo, by = "periodo") |>
        dplyr::mutate(
          ratio_reemplazo = dplyr::if_else(entra > 0, sale_5564 / entra, NA_real_),
          peso_55mas_pct  = dplyr::if_else(valor_act_total > 0, 100 * (sale_5564 + sale_65mas) / valor_act_total, NA_real_)
        ) |>
        dplyr::arrange(periodo)
    })

    output$plot_envejecimiento_tiempo <- plotly::renderPlotly({
      df <- envejecimiento_tiempo_df()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = periodo, y = ratio_reemplazo)) +
        ggplot2::geom_line(color = "#0B5FA5") +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#888888") +
        ggplot2::labs(x = NULL, y = "Activos 55-64 / Activos 16-24",
                      title = sprintf("Ratio de reemplazo generacional \u2014 %s", filtros$territorio_ref())) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    fila_envejecimiento_actual <- reactive({
      df <- envejecimiento_tiempo_df()
      req(nrow(df) > 0, filtros$periodo())
      f <- df |> dplyr::filter(periodo == filtros$periodo())
      if (nrow(f) != 1) return(NULL)
      f
    })

    output$kpi_ratio_reemplazo <- renderUI({
      f <- fila_envejecimiento_actual()
      kpi_card(
        titulo = "Ratio de reemplazo (55-64 / 16-24)",
        valor = if (!is.null(f) && !is.na(f$ratio_reemplazo)) round(f$ratio_reemplazo, 2) else NA
      )
    })

    output$kpi_peso_55mas <- renderUI({
      f <- fila_envejecimiento_actual()
      kpi_card(
        titulo = "Peso de 55+ sobre activos totales", unidad = "%",
        valor = if (!is.null(f) && !is.na(f$peso_55mas_pct)) round(f$peso_55mas_pct, 1) else NA
      )
    })

  })
}
