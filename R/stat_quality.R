# -----------------------------------------------------------------------------
# stat_quality.R
#
# Control de calidad estadística de las estimaciones EPA. Se apoya en el
# tamaño muestral REAL (n_pob, n_act, n_ocu, n_par... nº de encuestas, no
# personas estimadas) que epa_helpers.R ya calcula en agregar_dato_base() /
# agregar_ocupados_base() / agregar_parados_base() junto a cada valor_*.
#
# IMPORTANTE — qué es y qué NO es este cálculo:
# La EPA es una encuesta con muestreo complejo (estratificado, por
# conglomerados, con pesos de elevación). El error de muestreo "correcto"
# requiere el diseño muestral completo (estratos, unidades primarias) para
# aplicar bootstrap replicado o linealización, que el INE no publica en el
# fichero de microdatos estándar. Lo que este módulo ofrece es una
# aproximación por tamaño muestral simple (nº de encuestas sin ponderar
# detrás de cada celda), que es lo habitual en cuadros de mando de este
# tipo y lo que permite el dato disponible. Es un proxy razonable para
# "esta celda tiene muy poca gente detrás, interprétala con cautela", NO
# un intervalo de confianza estadísticamente exacto. Así se documenta
# siempre en la UI (ver badge_calidad()) y en el diccionario metodológico.
#
# Regla de umbral: el propio INE deja de publicar/advierte fuertemente
# sobre estimaciones basadas en menos de 100 observaciones muestrales, y
# marca con advertencia especial las basadas en menos de 50. Replicamos
# esa misma convención aquí en vez de inventar un umbral propio.
# -----------------------------------------------------------------------------

library(dplyr)

UMBRAL_INSUFICIENTE <- 50   # n < 50 -> no fiable, mostrar aviso fuerte
UMBRAL_CAUTELA <- 100       # 50 <= n < 100 -> fiabilidad limitada, aviso moderado

#' Clasifica la fiabilidad de una celda según su tamaño muestral real.
#'
#' @param n vector de tamaños muestrales (nº de encuestas sin ponderar)
#' @return factor ordenado: "insuficiente" | "cautela" | "fiable" | NA
clasificar_fiabilidad <- function(n) {
  dplyr::case_when(
    is.na(n)              ~ NA_character_,
    n < UMBRAL_INSUFICIENTE  ~ "insuficiente",
    n < UMBRAL_CAUTELA       ~ "cautela",
    TRUE                     ~ "fiable"
  ) |>
    factor(levels = c("insuficiente", "cautela", "fiable"), ordered = TRUE)
}

#' Coeficiente de variación aproximado de una estimación de proporción/tasa
#' bajo muestreo aleatorio simple, como proxy del error real de diseño
#' complejo (ver nota de cabecera: subestima el verdadero CV de la EPA,
#' que tiene efecto de diseño > 1; se usa solo como señal relativa, no
#' como intervalo de confianza exacto).
#'
#' @param p proporción estimada (0-1)
#' @param n tamaño muestral (encuestas sin ponderar)
#' @return CV en porcentaje
cv_proporcion <- function(p, n) {
  se <- sqrt(pmax(p * (1 - p), 0) / pmax(n, 1))
  ifelse(n > 0 & p > 0, 100 * se / p, NA_real_)
}

#' Añade columnas de fiabilidad a una tabla agregada que ya trae valor_* y
#' n_* (salida directa de con_totales()/calcular_tablas_trimestre()).
#'
#' @param df tabla con columnas valor_<universo> y n_<universo>
#' @param universo sufijo del universo a evaluar: "pob","act","ocu","par","ina"
#' @return df con columnas nuevas: n_muestra, fiabilidad
anotar_fiabilidad <- function(df, universo) {
  col_n <- paste0("n_", universo)
  if (!col_n %in% names(df)) {
    df$n_muestra <- NA_integer_
    df$fiabilidad <- NA
    return(df)
  }
  df |>
    mutate(
      n_muestra  = .data[[col_n]],
      fiabilidad = clasificar_fiabilidad(n_muestra)
    )
}

#' Texto de aviso a mostrar junto a un dato con fiabilidad limitada o
#' insuficiente. Devuelve NULL si la celda es fiable (para no ensuciar la UI
#' con avisos en el caso normal).
texto_aviso_fiabilidad <- function(fiabilidad, n_muestra) {
  if (is.na(fiabilidad)) return(NULL)
  if (fiabilidad == "insuficiente") {
    return(sprintf(
      "Estimación con muestra insuficiente (%d encuestas). No se recomienda su uso ni difusión.",
      n_muestra
    ))
  }
  if (fiabilidad == "cautela") {
    return(sprintf(
      "Estimación con elevada incertidumbre estadística (%d encuestas). Interpretar con cautela.",
      n_muestra
    ))
  }
  NULL
}
