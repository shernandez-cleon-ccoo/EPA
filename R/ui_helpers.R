# -----------------------------------------------------------------------------
# ui_helpers.R
#
# Piezas de UI reutilizables entre módulos: KPI cards, badge de fiabilidad
# estadística (envuelve stat_quality.R para que cada módulo no reimplemente
# el aviso), resumen de filtros activos.
# -----------------------------------------------------------------------------

library(shiny)
library(htmltools)

#' Badge visual de fiabilidad, a insertar junto a cualquier KPI o celda de
#' tabla. Silencioso (NULL) si la celda es fiable, para no ensuciar la UI
#' en el caso normal — solo se ve cuando hay algo que advertir.
badge_calidad <- function(fiabilidad, n_muestra) {
  aviso <- texto_aviso_fiabilidad(fiabilidad, n_muestra)
  if (is.null(aviso)) return(NULL)
  color <- if (identical(fiabilidad, "insuficiente")) "#B3261E" else "#B25E00"
  icono <- if (identical(fiabilidad, "insuficiente")) "\u26D4" else "\u26A0"
  tags$span(
    title = aviso,
    style = sprintf(
      "color:%s; font-size:0.75em; cursor:help; margin-left:4px;", color
    ),
    icono
  )
}

#' Tarjeta KPI estándar: valor actual, unidad, variación trimestral,
#' interanual y diferencia vs territorio de comparación. `variacion_*` y
#' `diferencia` pueden venir NA (p.ej. sin comparación seleccionada o sin
#' trimestre anterior disponible) y la tarjeta los omite en vez de mostrar
#' "NA".
kpi_card <- function(titulo, valor, unidad = "",
                      var_intertrim = NA_real_, var_interanual = NA_real_,
                      diferencia = NA_real_, etiqueta_comparacion = NULL,
                      fiabilidad = NA, n_muestra = NA_integer_) {

  fmt_var <- function(x, sufijo = " pp") {
    if (is.na(x)) return(NULL)
    signo <- if (x >= 0) "+" else ""
    color <- if (x >= 0) "#1B7A3D" else "#B3261E"
    tags$span(style = sprintf("color:%s;", color), sprintf("%s%.1f%s", signo, x, sufijo))
  }

  div(
    class = "kpi-card",
    style = "border:1px solid #E0E0E0; border-radius:8px; padding:12px 16px;",
    div(
      style = "font-size:0.8em; color:#555; display:flex; align-items:center;",
      titulo, badge_calidad(fiabilidad, n_muestra)
    ),
    div(
      style = "font-size:1.6em; font-weight:600;",
      sprintf("%s%s", format(valor, big.mark = ".", decimal.mark = ",", trim = TRUE), unidad)
    ),
    div(
      style = "font-size:0.75em; color:#777; display:flex; gap:8px; flex-wrap:wrap;",
      if (!is.na(var_intertrim)) tagList("Trim. ant.: ", fmt_var(var_intertrim)),
      if (!is.na(var_interanual)) tagList("Interanual: ", fmt_var(var_interanual)),
      if (!is.na(diferencia) && !is.null(etiqueta_comparacion))
        tagList(sprintf("vs %s: ", etiqueta_comparacion), fmt_var(diferencia))
    )
  )
}

#' Resumen de la selección de filtros activa, tipo:
#' "Castilla y León · T2 2026 · Ambos sexos · 16+ años"
resumen_filtros <- function(territorio, periodo_label, sexo = "Ambos sexos", extra = NULL) {
  partes <- c(territorio, periodo_label, sexo, "16+ años", extra)
  paste(partes[!vapply(partes, is.null, logical(1))], collapse = " \u00b7 ")
}
