# -----------------------------------------------------------------------------
# indicators.R
#
# Funciones puras (data.frame -> data.frame / vector -> vector), sin ningún
# conocimiento de Shiny, para que sean testeables con testthat sin levantar
# la app. Todo lo que sea "variación", "diferencia" o "brecha" reutilizable
# entre pestañas vive aquí, no repetido dentro de cada módulo.
# -----------------------------------------------------------------------------

library(dplyr)

#' Variación interanual (mismo trimestre, año anterior) de una serie
#' periodo-ordenada. df debe tener 1 fila por periodo (ya filtrado a la
#' combinación de dimensiones deseada) y estar ordenado por periodo.
variacion_interanual <- function(df, col_valor, col_periodo = "periodo") {
  df |>
    arrange(.data[[col_periodo]]) |>
    mutate(
      valor_hace_1a = dplyr::lag(.data[[col_valor]], n = 4),
      var_interanual_abs = .data[[col_valor]] - valor_hace_1a,
      var_interanual_pct = 100 * var_interanual_abs / valor_hace_1a
    )
}

#' Variación intertrimestral (trimestre anterior inmediato).
variacion_intertrimestral <- function(df, col_valor, col_periodo = "periodo") {
  df |>
    arrange(.data[[col_periodo]]) |>
    mutate(
      valor_trim_anterior = dplyr::lag(.data[[col_valor]], n = 1),
      var_intertrim_abs = .data[[col_valor]] - valor_trim_anterior,
      var_intertrim_pct = 100 * var_intertrim_abs / valor_trim_anterior
    )
}

#' Diferencia frente al periodo de referencia fijo (p.ej. 2019T4), en
#' términos absolutos y porcentuales.
variacion_desde <- function(df, col_valor, periodo_referencia, col_periodo = "periodo") {
  valor_ref <- df[[col_valor]][df[[col_periodo]] == periodo_referencia]
  valor_ref <- if (length(valor_ref) == 1) valor_ref else NA_real_
  df |>
    mutate(
      valor_referencia = valor_ref,
      var_desde_abs = .data[[col_valor]] - valor_referencia,
      var_desde_pct = 100 * var_desde_abs / valor_referencia
    )
}

#' Une un data.frame de territorio de referencia con uno de territorio de
#' comparación (mismo periodo/dimensiones) y calcula la diferencia en
#' puntos porcentuales o unidades. territorio_comparacion puede ser NULL:
#' en ese caso se devuelve df_ref sin columnas de comparación, para que
#' cada función de plot_helpers.R decida cómo renderizar la versión
#' reducida (ver arquitectura: "sin territorio de comparación").
con_comparacion <- function(df_ref, df_comp, col_valor, by_vars) {
  if (is.null(df_comp)) {
    return(df_ref |> mutate(valor_comparacion = NA_real_, diferencia = NA_real_))
  }
  df_comp_ren <- df_comp |>
    select(all_of(by_vars), valor_comparacion = all_of(col_valor))
  df_ref |>
    left_join(df_comp_ren, by = by_vars) |>
    mutate(diferencia = .data[[col_valor]] - valor_comparacion)
}

#' Brecha de género (mujeres - hombres) en puntos porcentuales/unidades,
#' para cualquier indicador ya calculado por sexo. df debe tener una fila
#' por sexo (Hombres/Mujeres) para cada combinación del resto de
#' dimensiones (by_vars).
brecha_genero <- function(df, col_valor, by_vars, col_sexo = "sexo") {
  anchote <- df |>
    select(all_of(by_vars), all_of(col_sexo), all_of(col_valor)) |>
    tidyr::pivot_wider(names_from = all_of(col_sexo), values_from = all_of(col_valor))
  if (!all(c("Mujeres", "Hombres") %in% names(anchote))) {
    anchote$brecha <- NA_real_
    return(anchote)
  }
  anchote |> mutate(brecha = Mujeres - Hombres)
}

#' Descompone el cambio en parados en dos efectos, usando la identidad
#' parados = activos - ocupados:
#'   Δparados = Δactivos − Δocupados
#' efecto_activos > 0 significa que sube el paro porque entra más gente al
#' mercado laboral (más activos); efecto_ocupados > 0 significa que baja el
#' paro porque se crea empleo (se resta Δocupados, por eso el signo se
#' invierte respecto a la variación bruta de ocupados). Es el argumento
#' sindical típico: "el paro baja pero es porque la gente deja de buscar
#' trabajo, no porque haya más empleo" se ve directamente comparando el
#' signo/magnitud de los dos efectos.
#'
#' @param df tabla con 1 fila por periodo (ya filtrada a territorio/sexo/edad
#'   deseados) y columnas valor_act, valor_ocu, valor_par
#' @param n_lag 4 para interanual (mismo trimestre año anterior), 1 para
#'   intertrimestral
#' @return df con columnas nuevas: var_activos, efecto_activos (= var_activos),
#'   var_ocupados, efecto_ocupados (= -var_ocupados), var_parados_total
#'   (= efecto_activos + efecto_ocupados, debe coincidir con Δparados real)
descomposicion_paro <- function(df, col_periodo = "periodo", n_lag = 4) {
  df |>
    arrange(.data[[col_periodo]]) |>
    mutate(
      var_activos       = valor_act - dplyr::lag(valor_act, n = n_lag),
      var_ocupados      = valor_ocu - dplyr::lag(valor_ocu, n = n_lag),
      var_parados_real  = valor_par - dplyr::lag(valor_par, n = n_lag),
      efecto_activos    = var_activos,
      efecto_ocupados   = -var_ocupados,
      var_parados_total = efecto_activos + efecto_ocupados
    )
}

#' Ratio de reemplazo generacional: activos/ocupados de una banda "sale"
#' (p.ej. 55-64) frente a una banda "entra" (p.ej. 16-24), para el bloque
#' de estructura demográfica activa. df debe tener 1 fila por banda de
#' edad ya filtrada al resto de dimensiones deseadas.
ratio_reemplazo <- function(df, col_valor, col_edad = "edad", banda_sale, banda_entra) {
  v_sale  <- df[[col_valor]][df[[col_edad]] == banda_sale]
  v_entra <- df[[col_valor]][df[[col_edad]] == banda_entra]
  if (length(v_sale) != 1 || length(v_entra) != 1 || v_entra == 0) return(NA_real_)
  v_sale / v_entra
}
