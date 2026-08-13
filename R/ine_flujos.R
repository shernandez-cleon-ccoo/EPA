# -----------------------------------------------------------------------------
# ine_flujos.R
#
# Ingesta de la Estadística de Flujos de la Población Activa (EFPA) del INE
# -- la fuente OFICIAL de transiciones trimestre a trimestre (ocupado ->
# parado, parado -> inactivo, etc.), calculada por el propio INE explotando
# el panel rotante de la EPA (1/6 de la muestra se renueva cada trimestre;
# el resto se sigue entre trimestres consecutivos, incluyendo el tratamiento
# correcto de casos "no consta" -- gente que cumple 16 años entre trimestres
# o que no residía en España el trimestre anterior). NO se reconstruye desde
# microdatos crudos emparejando individuos a mano: sería reinventar, peor,
# algo que el INE ya publica y valida. Solo se usa para ESTE módulo
# (Mercado laboral); el resto del pipeline sigue calculándose desde
# microdatos como siempre.
#
# Fuente: tabla Tempus3 nº 66266 ("Población de 16+ por CCAA y relación con
# la actividad en el trimestre actual según relación con la actividad en el
# trimestre anterior"), vía la API JSON documentada en
# https://www.ine.es/dyngs/DAB/index.htm?cid=1100. La estructura de parseo
# de abajo está verificada contra un volcado real de esa tabla (no
# adivinada): cada combinación de dimensiones llega como una "serie"
# independiente con su propio array MetaData (T3_Variable/Nombre/Codigo por
# dimensión) en vez de columnas categóricas planas -- se reconstruyen aquí
# leyendo MetaData, NUNCA parseando el campo "Nombre" de texto libre (frágil
# ante cambios de formato).
#
# IMPORTANTE -- mapeo de códigos de CCAA: la EFPA usa códigos 01-19 para las
# 19 CCAA+ciudades autónomas (18=Ceuta, 19=Melilla), MIENTRAS QUE el resto
# del pipeline (microdatos EPA, ccaa_labels) usa 01-17 + 51=Ceuta + 52=Melilla.
# Los códigos 01-17 coinciden (verificado); 18/19 se remapean explícitamente
# a 51/52 antes de aplicar ccaa_labels(), para que "region" case EXACTAMENTE
# con el string que usa el resto de tablas del dashboard.
#
# LIMITACIÓN CONOCIDA: la tabla 66266 solo trae "Ambos sexos" (sin desglose
# por sexo); el desglose por sexo de la EFPA vive en otra tabla, nacional
# únicamente (sin CCAA). Como este dashboard es territorial por diseño, se
# usa la tabla por CCAA y se documenta la columna `sexo` siempre como
# "total" en epa_flujos -- el filtro global de sexo NO aplica a este panel,
# y así se avisa en la UI (mod_mercado_laboral.R).
# -----------------------------------------------------------------------------

library(jsonlite)

EFPA_URL_BASE <- "https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA"

# Códigos de CCAA de la EFPA que no coinciden con los del resto del pipeline.
EFPA_CCAA_REMAP <- c("18" = "51", "19" = "52")  # Ceuta, Melilla

#' Aplana una respuesta ya parseada (lista R) de una tabla Tempus3 "cubo" de
#' la EFPA. Separada de la descarga HTTP a propósito, para poder testear el
#' parseo contra un JSON real guardado en disco sin depender de la red.
#'
#' @param datos lista R ya parseada con jsonlite::fromJSON(..., simplifyVector = FALSE)
#' @return tibble con columnas periodo/region/sexo/situacion_actual/
#'   situacion_anterior/valor/tipo_dato
parsear_efpa_json <- function(datos) {
  if (is.null(datos) || length(datos) == 0) return(tibble())

  purrr::map_dfr(datos, function(serie) {
    # Busca por SUBCADENA ASCII (sin tildes) en vez de identical() sobre el
    # nombre completo con acentos: bajo un locale no-UTF8 (confirmado en
    # pruebas), la comparación exacta de "RELACIÓN..."/"Comunidades y
    # Ciudades Autónomas" puede no casar a nivel de bytes aunque el
    # contenido sea el mismo. "ACTIVIDAD", "Comunidades", "anterior", etc.
    # son ASCII puro y no dependen de la codificación del entorno donde
    # corra esto (GitHub Actions u otro).
    buscar_meta <- function(patron, excluir = NULL) {
      for (m in serie$MetaData) {
        var <- m$T3_Variable
        if (grepl(patron, var, ignore.case = TRUE, useBytes = TRUE) &&
            (is.null(excluir) || !grepl(excluir, var, ignore.case = TRUE, useBytes = TRUE))) {
          return(m)
        }
      }
      NULL
    }

    m_nacional <- buscar_meta("^Total Nacional$")
    m_ccaa     <- buscar_meta("Comunidades")
    m_sexo     <- buscar_meta("^Sexo$")
    m_anterior <- buscar_meta("anterior")
    m_actual   <- buscar_meta("activ", excluir = "anterior")

    region <- if (!is.null(m_nacional)) {
      "España"
    } else if (!is.null(m_ccaa)) {
      codigo <- m_ccaa$Codigo
      codigo <- if (codigo %in% names(EFPA_CCAA_REMAP)) EFPA_CCAA_REMAP[[codigo]] else codigo
      unname(ccaa_labels[codigo])
    } else {
      NA_character_
    }

    sexo <- if (is.null(m_sexo)) "total" else dplyr::case_when(
      grepl("^Ambos", m_sexo$Nombre, ignore.case = TRUE) ~ "total",
      grepl("^Hombres", m_sexo$Nombre, ignore.case = TRUE) ~ "Hombres",
      grepl("^Mujeres", m_sexo$Nombre, ignore.case = TRUE) ~ "Mujeres",
      TRUE ~ NA_character_
    )

    situacion_actual   <- if (is.null(m_actual))   NA_character_ else stringr::str_remove(m_actual$Nombre,   " \\(trimestre actual\\)")
    situacion_anterior <- if (is.null(m_anterior)) NA_character_ else stringr::str_remove(m_anterior$Nombre, " \\(trimestre anterior\\)")

    if (is.null(serie$Data) || length(serie$Data) == 0 || is.na(region) || is.na(situacion_actual) || is.na(situacion_anterior)) {
      return(tibble())
    }

    escala <- if (identical(serie$T3_Escala, "Miles")) 1000 else 1

    purrr::map_dfr(serie$Data, function(d) {
      trim <- suppressWarnings(as.integer(stringr::str_remove(d$T3_Periodo, "T")))
      if (is.null(d$Anyo) || is.na(trim) || is.null(d$Valor)) return(tibble())
      tibble::tibble(
        periodo            = periodo_date(d$Anyo, trim),
        region             = region,
        sexo               = sexo,
        situacion_actual   = situacion_actual,
        situacion_anterior = situacion_anterior,
        valor              = as.numeric(d$Valor) * escala,
        tipo_dato          = if (is.null(d$T3_TipoDato)) NA_character_ else d$T3_TipoDato
      )
    })
  })
}

#' Descarga y aplana una tabla Tempus3 "cubo" de la EFPA.
#'
#' @param id_tabla identificador Tempus3 de la tabla (p.ej. 66266)
#' @param nult si se especifica, limita a los N últimos periodos por serie;
#'   NULL (por defecto) trae toda la serie histórica disponible (la tabla es
#'   pequeña -- unos cientos de series x ~50 trimestres -- no hace falta
#'   incrementalidad como en build_epa_tablas.R para los microdatos)
#' @return tibble con columnas periodo/region/sexo/situacion_actual/
#'   situacion_anterior/valor/tipo_dato, o tibble() vacío si falla la
#'   descarga o el parseo (nunca lanza error hacia arriba, mismo criterio
#'   que descargar_tabla_remota() en app.R -- quien llame decide qué hacer
#'   con una tabla vacía, p.ej. conservar la versión anterior).
descargar_efpa_tabla <- function(id_tabla, nult = NULL) {
  url <- paste0(EFPA_URL_BASE, "/", id_tabla, "?tip=AM")
  if (!is.null(nult)) url <- paste0(url, "&nult=", nult)

  resp <- tryCatch(httr::GET(url, httr::timeout(60)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message(sprintf("No se pudo descargar la tabla EFPA %s del INE (¿caída o cambio de URL?).", id_tabla))
    return(tibble())
  }

  datos <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) {
      message(sprintf("JSON de la tabla EFPA %s no parseable: %s", id_tabla, conditionMessage(e)))
      NULL
    }
  )
  parsear_efpa_json(datos)
}

#' Wrapper específico para la tabla por CCAA (66266), que es la que usa el
#' dashboard (territorial por diseño -- ver limitación de sexo en la
#' cabecera del fichero).
descargar_flujos_ccaa <- function(nult = NULL) {
  descargar_efpa_tabla(66266, nult = nult)
}
