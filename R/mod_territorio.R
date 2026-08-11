# -----------------------------------------------------------------------------
# mod_territorio.R
#
# Pestaña "Territorio" (Nivel 2, Oleada 1). Responde a: ¿qué diferencias hay
# entre provincias/CCAA? ¿quién envejece más rápido?
#
# Usa DOS tablas estrella distintas, con distinta granularidad territorial:
#   - datos_prov (epa_prov): provincia x sexo -> mapa coroplético, ranking
#     provincial y evolución de una provincia.
#   - datos_edad (epa_edad): region (CCAA/España) x edad x sexo -> ranking de
#     envejecimiento activo entre CCAA (necesita el desglose por edad, que
#     epa_prov no tiene).
#
# El filtro global `territorio_ref`/`territorio_comp` (§app.R) trabaja a
# nivel CCAA/España, no de provincia -- por eso esta pestaña añade un filtro
# LOCAL "provincia a analizar" (principio ya fijado en la arquitectura: los
# filtros avanzados/específicos de una pestaña no viven en app.R). El
# territorio de comparación global SÍ se reutiliza tal cual en la evolución
# provincial cuando resuelve a "España" (que epa_prov sí tiene, como
# pseudo-provincia) o a un nombre que casualmente sea una provincia; si no,
# la evolución se reduce a una sola serie (mismo principio que el resto del
# dashboard: nunca un gráfico roto por comparación ausente).
# -----------------------------------------------------------------------------

# Opciones de indicador compartidas entre mapa, ranking y evolución, para no
# duplicar la lista de choices/etiquetas en tres sitios distintos.
OPCIONES_INDICADOR_TERRITORIO <- c(
  "Tasa de paro (%)"      = "tasa_par",
  "Tasa de empleo (%)"    = "tasa_emp",
  "Tasa de actividad (%)" = "tasa_act",
  "Ocupados"               = "valor_ocu",
  "Parados"                = "valor_par",
  "Activos"                = "valor_act"
)

etiqueta_indicador_territorio <- function(var_sel) {
  nombres <- names(OPCIONES_INDICADOR_TERRITORIO)
  nombres[OPCIONES_INDICADOR_TERRITORIO == var_sel][1]
}

mod_territorio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("indicador"), "Indicador a mapear / rankear",
                             choices = OPCIONES_INDICADOR_TERRITORIO)),
      column(4, uiOutput(ns("selector_provincia_foco")))
    ),
    tabsetPanel(
      id = ns("subtabs"),
      tabPanel(
        "Mapa provincial",
        br(),
        plotOutput(ns("mapa"), height = "520px"),
        tags$p(
          style = "font-size:0.75em; color:#888;",
          "El mapa no representa el aviso de fiabilidad muestral por celda ",
          "(el color solo depende del valor); consulta el ranking o la tabla ",
          "de la pestaña Calidad del empleo para el detalle de fiabilidad ",
          "por provincia."
        )
      ),
      tabPanel(
        "Ranking provincial",
        br(),
        plotly::plotlyOutput(ns("ranking"), height = "620px"),
        uiOutput(ns("ranking_aviso"))
      ),
      tabPanel(
        "Evolución provincial",
        br(),
        plotly::plotlyOutput(ns("evolucion_prov"))
      ),
      tabPanel(
        "Envejecimiento activo",
        br(),
        tags$p(
          "Ratio entre activos de 55-64 años y activos de 16-24 años, por ",
          "territorio: cuanta más generación sale del mercado laboral por ",
          "cada una que entra, mayor el riesgo de sustitución generacional. ",
          "La línea discontinua marca el punto de equilibrio (ratio = 1)."
        ),
        plotly::plotlyOutput(ns("envejecimiento"), height = "620px"),
        uiOutput(ns("envejecimiento_aviso"))
      )
    )
  )
}

#' @param id id del módulo
#' @param datos_edad reactive() que devuelve la tabla epa_edad ya cargada
#' @param datos_prov reactive() que devuelve la tabla epa_prov ya cargada
#' @param filtros lista de reactives compartidos desde app.R: territorio_ref,
#'   territorio_comp (puede resolver a NULL), periodo, sexo
mod_territorio_server <- function(id, datos_edad, datos_prov, filtros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Código INE de provincia <- nombre (inverso de prov_labels, ya cargado
    # globalmente desde epa_helpers.R), para poder unir con el sf de
    # mapSpain sin depender de que los nombres coincidan carácter a carácter.
    cod_prov_por_nombre <- setNames(names(prov_labels), unname(prov_labels))

    # --- Provincias disponibles y filtro local de "provincia a analizar" ---
    provincias_disponibles <- reactive({
      req(datos_prov())
      sort(setdiff(unique(datos_prov()$provincia), "España"))
    })

    output$selector_provincia_foco <- renderUI({
      provs <- provincias_disponibles()
      req(length(provs) > 0)
      seleccion_previa <- input$provincia_foco
      sel_default <- if (!is.null(seleccion_previa) && seleccion_previa %in% provs) {
        seleccion_previa
      } else if (filtros$territorio_ref() %in% provs) {
        filtros$territorio_ref()
      } else {
        provs[1]
      }
      selectInput(ns("provincia_foco"), "Provincia a analizar (evolución)",
                  choices = provs, selected = sel_default)
    })

    # --- Datos del trimestre seleccionado, todas las provincias, con aviso
    # de fiabilidad muestral ya anotado (universo "act": activos) ----------
    prov_trimestre <- reactive({
      req(datos_prov(), filtros$periodo(), filtros$sexo())
      datos_prov() |>
        dplyr::filter(
          periodo == filtros$periodo(),
          sexo == filtros$sexo(),
          provincia != "España"
        ) |>
        anotar_fiabilidad("act")
    })

    # --- Geometría provincial (mapSpain), calculada una sola vez por sesión
    mapa_sf <- reactive({
      shiny::validate(shiny::need(
        requireNamespace("mapSpain", quietly = TRUE) && requireNamespace("sf", quietly = TRUE),
        "Los paquetes mapSpain/sf no están disponibles."
      ))
      sf_prov <- mapSpain::esp_get_prov(moveCAN = TRUE)
      # cpro es el código INE de provincia; se normaliza a 2 dígitos texto
      # para que case exactamente con los nombres de prov_labels (epa_helpers.R).
      sf_prov$cod_prov <- sprintf("%02d", as.integer(as.character(sf_prov$cpro)))
      sf_prov
    })

    # --- Mapa coroplético ---------------------------------------------------
    output$mapa <- renderPlot({
      req(prov_trimestre())
      var_sel <- input$indicador
      etiqueta <- etiqueta_indicador_territorio(var_sel)

      datos_mapa <- prov_trimestre() |>
        dplyr::mutate(cod_prov = unname(cod_prov_por_nombre[provincia])) |>
        dplyr::filter(!is.na(cod_prov))

      sf_datos <- dplyr::left_join(mapa_sf(), datos_mapa, by = "cod_prov")

      ggplot2::ggplot(sf_datos) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[var_sel]]), color = "white", linewidth = 0.2) +
        ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey85", name = etiqueta) +
        ggplot2::labs(title = sprintf("%s \u2014 %s", etiqueta, formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "right", plot.title = ggplot2::element_text(size = 13))
    })

    # --- Ranking provincial (barras horizontales, provincia local destacada)
    output$ranking <- plotly::renderPlotly({
      req(prov_trimestre())
      var_sel <- input$indicador
      etiqueta <- etiqueta_indicador_territorio(var_sel)
      foco <- input$provincia_foco

      datos <- prov_trimestre() |>
        dplyr::mutate(
          valor_sel = .data[[var_sel]],
          destacada = !is.null(foco) && provincia == foco
        )

      p <- ggplot2::ggplot(
        datos,
        ggplot2::aes(x = reorder(provincia, valor_sel), y = valor_sel, fill = destacada,
                     text = sprintf("%s: %s%s", provincia,
                                     format(round(valor_sel, 1), big.mark = ".", decimal.mark = ","),
                                     ifelse(grepl("tasa", var_sel), "%", "")))
      ) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#0B5FA5", "FALSE" = "#B7C4D6"), guide = "none") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = etiqueta,
                      title = sprintf("Ranking provincial \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p, tooltip = "text")
    })

    output$ranking_aviso <- renderUI({
      req(prov_trimestre())
      insuficientes <- prov_trimestre() |>
        dplyr::filter(fiabilidad == "insuficiente") |>
        dplyr::pull(provincia)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(
        style = "font-size:0.75em; color:#B3261E;",
        sprintf(
          "\u26D4 Muestra insuficiente este trimestre en: %s. Interpretar con máxima cautela.",
          paste(insuficientes, collapse = ", ")
        )
      )
    })

    # --- Evolución de la provincia local vs territorio de comparación global
    # (solo si territorio_comp resuelve a una provincia real de epa_prov,
    # p.ej. "España"; si no, se reduce a una sola serie sin romper el gráfico)
    output$evolucion_prov <- plotly::renderPlotly({
      req(datos_prov(), input$provincia_foco)
      var_sel <- input$indicador
      etiqueta <- etiqueta_indicador_territorio(var_sel)

      serie_foco <- datos_prov() |>
        dplyr::filter(provincia == input$provincia_foco, sexo == filtros$sexo()) |>
        dplyr::mutate(serie_id = input$provincia_foco)

      tc <- filtros$territorio_comp()
      provincias_validas <- unique(datos_prov()$provincia)
      comp_valida <- !is.null(tc) && tc %in% provincias_validas && tc != input$provincia_foco

      datos_plot <- if (comp_valida) {
        serie_comp <- datos_prov() |>
          dplyr::filter(provincia == tc, sexo == filtros$sexo()) |>
          dplyr::mutate(serie_id = tc)
        dplyr::bind_rows(serie_foco, serie_comp)
      } else {
        serie_foco
      }

      p <- ggplot2::ggplot(datos_plot, ggplot2::aes(x = periodo, y = .data[[var_sel]], color = serie_id)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = NULL, y = etiqueta, color = NULL,
                      title = sprintf("Evoluci\u00f3n \u2014 %s", etiqueta)) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })

    # --- Ranking de envejecimiento activo entre CCAA -----------------------
    # No usa ratio_reemplazo() de R/indicators.R tal cual porque esa función
    # espera una banda de edad exacta por lado; aquí "entra" combina dos
    # bandas quinquenales (16-19 + 20-24) que hay que sumar primero.
    envejecimiento_df <- reactive({
      req(datos_edad(), filtros$periodo(), filtros$sexo())
      datos_edad() |>
        dplyr::filter(
          periodo == filtros$periodo(),
          sexo == filtros$sexo(),
          edad %in% c("16-19", "20-24", "55-64"),
          region != "España"
        ) |>
        dplyr::mutate(grupo = ifelse(edad == "55-64", "sale", "entra")) |>
        dplyr::group_by(region, grupo) |>
        dplyr::summarise(
          valor_act = sum(valor_act, na.rm = TRUE),
          n_act = sum(n_act, na.rm = TRUE),
          .groups = "drop"
        ) |>
        tidyr::pivot_wider(
          names_from = grupo, values_from = c(valor_act, n_act),
          names_glue = "{.value}_{grupo}"
        ) |>
        dplyr::mutate(
          ratio_reemplazo = dplyr::if_else(valor_act_entra > 0, valor_act_sale / valor_act_entra, NA_real_),
          n_muestra = pmin(n_act_sale, n_act_entra),
          fiabilidad = clasificar_fiabilidad(n_muestra)
        )
    })

    output$envejecimiento <- plotly::renderPlotly({
      df <- envejecimiento_df()
      req(nrow(df) > 0)
      df <- df |> dplyr::mutate(destacada = region == filtros$territorio_ref())

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = reorder(region, ratio_reemplazo), y = ratio_reemplazo, fill = destacada,
                     text = sprintf("%s: %.2f", region, ratio_reemplazo))
      ) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("TRUE" = "#0B5FA5", "FALSE" = "#B7C4D6"), guide = "none") +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#888888") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Activos 55-64 / Activos 16-24",
                      title = sprintf("Ratio de reemplazo generacional \u2014 %s", formato_trimestre(filtros$periodo()))) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p, tooltip = "text")
    })

    output$envejecimiento_aviso <- renderUI({
      df <- envejecimiento_df()
      req(nrow(df) > 0)
      insuficientes <- df |> dplyr::filter(fiabilidad == "insuficiente") |> dplyr::pull(region)
      if (length(insuficientes) == 0) return(NULL)
      tags$p(
        style = "font-size:0.75em; color:#B3261E;",
        sprintf(
          "\u26D4 Muestra insuficiente este trimestre en: %s. Interpretar con máxima cautela.",
          paste(insuficientes, collapse = ", ")
        )
      )
    })
  })
}
