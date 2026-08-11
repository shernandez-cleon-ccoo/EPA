# =============================================================================
# epa_helpers.R
# =============================================================================
# Funciones y diccionarios COMPARTIDOS entre:
#   - app.R                : app Shiny (ventana móvil reciente, exploración libre)
#   - build_epa_tablas.R   : job por lotes que agrega TODO el histórico desde
#                            2005 en las tablas "estrella" (epa_edad, epa_form,
#                            epa_nac, epa_sector, epa_prov) que antes venían de
#                            las tablas resumen del INE.
#
# Todo lo que dependa de la estructura/códigos de los microdatos EPA vive aquí,
# para no tener que mantenerlo dos veces. Los códigos de EDAD/NFORMA/NAC/ACT se
# han verificado contra los "Diseño de registro y valores válidos" oficiales
# del INE para los 3 periodos de diseño:
#   dr_EPA_2005 (T1/2005 - T4/2020), dr_EPA_2021 (T1/2021 - T4/2025),
#   dr_EPA_2026 (T1/2026 en adelante)
# Los códigos de EDAD5/EDAD1 (T5EDAD), NAC1 (TNACIO), NFORMA (TNFORMA),
# EXREGNA1 (TREGNAP) y ACT1/ACT09 (TACTIV/T09ACTI) son IDÉNTICOS en los 3
# ficheros, así que un único mapeo sirve para toda la serie histórica.
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(httr)
library(tibble)
library(tidyr)

# -----------------------------------------------------------------------------
# CONFIGURACION
# -----------------------------------------------------------------------------

CACHE_DIR <- "epa_cache"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# Primer trimestre disponible en el FTP con este esquema de nombres.
ANIO_MIN <- 2005

# -----------------------------------------------------------------------------
# ETIQUETAS DE TERRITORIO (idénticas a las de tu app.R)
# -----------------------------------------------------------------------------

prov_labels <- c(
  "01"="Araba/Álava","02"="Albacete","03"="Alicante/Alacant","04"="Almería","05"="Ávila",
  "06"="Badajoz","07"="Balears, Illes","08"="Barcelona","09"="Burgos","10"="Cáceres",
  "11"="Cádiz","12"="Castellón/Castelló","13"="Ciudad Real","14"="Córdoba","15"="Coruña, A",
  "16"="Cuenca","17"="Girona","18"="Granada","19"="Guadalajara","20"="Gipuzkoa",
  "21"="Huelva","22"="Huesca","23"="Jaén","24"="León","25"="Lleida",
  "26"="Rioja, La","27"="Lugo","28"="Madrid","29"="Málaga","30"="Murcia",
  "31"="Navarra","32"="Ourense","33"="Asturias","34"="Palencia","35"="Palmas, Las",
  "36"="Pontevedra","37"="Salamanca","38"="Santa Cruz de Tenerife","39"="Cantabria","40"="Segovia",
  "41"="Sevilla","42"="Soria","43"="Tarragona","44"="Teruel","45"="Toledo",
  "46"="Valencia/València","47"="Valladolid","48"="Bizkaia","49"="Zamora","50"="Zaragoza",
  "51"="Ceuta","52"="Melilla"
)

ccaa_labels <- c(
  "01"="Andalucía","02"="Aragón","03"="Asturias","04"="Illes Balears",
  "05"="Canarias","06"="Cantabria","07"="Castilla y León","08"="Castilla-La Mancha",
  "09"="Cataluña","10"="Comunitat Valenciana","11"="Extremadura","12"="Galicia",
  "13"="Comunidad de Madrid","14"="Región de Murcia","15"="Navarra","16"="País Vasco","17"="La Rioja",
  "51"="Ceuta","52"="Melilla"
)

# -----------------------------------------------------------------------------
# DICCIONARIOS DE CATEGORÍAS (verificados contra dr_EPA_2005/2021/2026.xlsx)
# -----------------------------------------------------------------------------

# EDAD5 / EDAD1 (T5EDAD): quinquenios "00".."65". Los agrupamos en las mismas
# bandas que usaba tu dashboard viejo (epa_edad$edad). NOTA: la banda "0-16"
# nunca aparecerá poblada en la práctica porque cargar_trimestre() ya filtra
# NIVEL == "1" (solo 16 años y más, el universo del cuestionario de actividad
# económica); se deja mapeada por coherencia de etiqueta con el selector, no
# porque vaya a tener datos.
edad_banda <- function(edad_ep) {
  dplyr::case_when(
    edad_ep %in% c("00", "05", "10") ~ "0-16",
    edad_ep == "16"                  ~ "16-19",
    edad_ep == "20"                  ~ "20-24",
    edad_ep %in% c("25", "30")       ~ "25-34",
    edad_ep %in% c("35", "40")       ~ "35-44",
    edad_ep %in% c("45", "50")       ~ "45-54",
    edad_ep %in% c("55", "60")       ~ "55-64",
    edad_ep == "65"                  ~ "65+",
    TRUE ~ NA_character_
  )
}

# NFORMA (TNFORMA): códigos AN/P1/P2/S1/SG/SP/SU, estables 2005-2026.
# Se mapean a las mismas claves que usaba tu vector `formacion` original.
nforma_label <- function(nforma) {
  dplyr::case_when(
    nforma == "AN" ~ "analf",
    nforma == "P1" ~ "prim_inic",
    nforma == "P2" ~ "prim",
    nforma == "S1" ~ "sec_1",
    nforma == "SG" ~ "sec_2_gen",
    nforma == "SP" ~ "sec_2_voc",
    nforma == "SU" ~ "ed_sup",
    TRUE ~ NA_character_
  )
}

# NAC1 (TNACIO): 1=Española, 2=Española y doble nacionalidad, 3=Extranjera.
# EXREGNA1 (TREGNAP): región del país de la nacionalidad extranjera.
#   115/125/128 = UE (UE-15 / resto UE-25 / resto UE-27->28) -> Unión Europea
#   cualquier otro código (100, 200, 300, 310, 350, 400, 410, 420, 500, 999) -> no UE
# Igual que en tu dashboard viejo, "Española" (nacionalidad == "1") incluye la
# doble nacionalidad (NAC1 %in% c("1","2")).
nacionalidad_grupo <- function(nac1, exregna1) {
  dplyr::case_when(
    nac1 %in% c("1", "2") ~ "ES",
    nac1 == "3" & exregna1 %in% c("115", "125", "128") ~ "UE",
    nac1 == "3" ~ "no_UE",
    TRUE ~ NA_character_
  )
}

# ACT1 (hasta CICLO<=213, es decir <=T1-2026 según tu propio corte) / ACT09
# (TACTIV / T09ACTI): códigos 0-9, estables en su agrupación gruesa por sector
# a lo largo de las 3 revisiones CNAE (93/09/25) que ya harmoniza el INE.
sector_grupo <- function(actividad) {
  dplyr::case_when(
    actividad == "0" ~ "agricultura",
    actividad == "4" ~ "construcción",
    actividad %in% c("1", "2", "3") ~ "industria",
    actividad %in% c("5", "6", "7", "8", "9") ~ "servicios",
    TRUE ~ NA_character_
  )
}

# OCUP1 (TOCUP): 10 grandes grupos CNO (0-9), estables 2005-2026 (CNO-1994 /
# CNO-2011 armonizados por el propio INE en el mismo código agregado). Solo
# está informada para quienes trabajaron o tenían empleo la semana de
# referencia (ocupados), igual que ACT1/ACT09.
ocupacion_label <- function(ocup1) {
  dplyr::case_when(
    ocup1 == "0" ~ "Militares",
    ocup1 == "1" ~ "Directores y gerentes",
    ocup1 == "2" ~ "Técnicos y profesionales científicos e intelectuales",
    ocup1 == "3" ~ "Técnicos y profesionales de apoyo",
    ocup1 == "4" ~ "Empleados contables, administrativos y de oficina",
    ocup1 == "5" ~ "Trabajadores de servicios y comercio",
    ocup1 == "6" ~ "Trabajadores cualificados agrario, ganadero, forestal y pesquero",
    ocup1 == "7" ~ "Artesanos y trabajadores cualificados de industria y construcción",
    ocup1 == "8" ~ "Operadores de instalaciones y maquinaria, montadores",
    ocup1 == "9" ~ "Ocupaciones elementales",
    TRUE ~ NA_character_
  )
}

# ITBU (TITBU): tiempo que lleva buscando empleo, 8 tramos, estable
# 2005-2026. Solo informada para quienes buscan empleo o lo han encontrado
# para empezar más adelante (aprox. parados, AOI %in% c("05","06")).
# Se reagrupa en los tramos que pide el briefing de paro de larga duración
# (<3m / 3-6m / 6-12m / 1-2a / >2a); la frontera de "larga duración" (>=1
# año) queda en el corte entre "06 meses a <1 año" y "1 año a <1 año y
# medio".
duracion_paro_banda <- function(itbu) {
  dplyr::case_when(
    itbu %in% c("01", "02") ~ "< 3 meses",
    itbu == "03"            ~ "3 a 6 meses",
    itbu == "04"            ~ "6 meses a 1 año",
    itbu %in% c("05", "06") ~ "1 a 2 años",
    itbu %in% c("07", "08") ~ "2 años o más",
    TRUE ~ NA_character_
  )
}

sexo_grupo <- function(sexo1) {
  dplyr::case_when(
    sexo1 == "1" ~ "hombres",
    sexo1 == "6" ~ "mujeres",
    TRUE ~ NA_character_
  )
}

# Columnas que descargamos de los microdatos. Se añade EXREGNA1 (necesaria
# para el desglose UE / no UE en epa_nac) respecto a la lista original de tu
# app.R, que no la traía porque de momento no distinguía UE / no UE.
cols_keep <- c(
  "CICLO", "CCAA", "PROV", "NVIVI", "NIVEL", "EDAD1", "EDAD5", "SEXO1", "NAC1",
  "EXREGNA1", "NFORMA",
  "ACT1", "ACT09", "OCUP1", "SITU", "SP", "DUCON1", "DUCON2", "DUCON3", "PARCO1", "PARCO2",
  "EXTRA", "EXTPAG", "EXTNPG", "AOI", "ITBU", "FACTOREL"
)

# -----------------------------------------------------------------------------
# HELPERS DE TEXTO / PARSEO
# -----------------------------------------------------------------------------

parse_factor_mixto <- function(x) {
  s <- trimws(as.character(x))
  dplyr::case_when(
    s == "" ~ NA_real_,
    stringr::str_detect(s, ",") ~ readr::parse_number(
      s, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    stringr::str_detect(s, "\\.") ~ readr::parse_number(
      s, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    TRUE ~ suppressWarnings(as.numeric(s))
  )
}

normalizar_nombres <- function(nms) {
  nms <- trimws(nms)
  nms <- toupper(nms)
  nms <- gsub('"', "", nms, fixed = TRUE)
  nms <- gsub("\\s+", "", nms, perl = TRUE)
  nms
}

leer_tab_generico <- function(path) {
  df <- read_delim(
    file = path,
    delim = "\t",
    locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE,
    progress = FALSE,
    quote = "\"",
    escape_double = TRUE,
    trim_ws = TRUE,
    col_types = cols(.default = col_character())
  )
  names(df) <- normalizar_nombres(names(df))
  df
}

# -----------------------------------------------------------------------------
# DESCARGA + CACHE (idéntico a tu app.R)
# -----------------------------------------------------------------------------

url_micro <- function(anio, trim) {
  yy <- sprintf("%02d", anio %% 100)
  sprintf("https://www.ine.es/ftp/microdatos/epa/datos_%dt%s.zip", trim, yy)
}

url_anexo_candidatos <- function(anio, trim) {
  yy <- sprintf("%02d", anio %% 100)
  # Confirmado por el usuario: el anexo sigue el mismo nombre que el fichero
  # de microdatos normal (datos_<trim>t<yy>.zip) con el sufijo "_a" antes de
  # la extensión. Se deja "datos_..a.zip" (sin guion bajo) como variante de
  # respaldo por si en algún trimestre concreto el guion bajo faltara.
  c(
    sprintf("https://www.ine.es/ftp/microdatos/epa/datos_%dt%s_a.zip", trim, yy),
    sprintf("https://www.ine.es/ftp/microdatos/epa/datos_%dt%sa.zip", trim, yy)
  )
}

descargar_zip <- function(url, timeout_s = 60) {
  destino <- tempfile(fileext = ".zip")
  ok <- tryCatch({
    resp <- httr::GET(
      url,
      httr::write_disk(destino, overwrite = TRUE),
      httr::timeout(timeout_s)
    )
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)

  if (!ok || !file.exists(destino) || file.info(destino)$size < 1000) {
    unlink(destino)
    return(NULL)
  }
  destino
}

extraer_fichero_datos <- function(zip_path, es_anexo = FALSE) {
  dir_extract <- tempfile("epa_extract_")
  dir.create(dir_extract)
  unzip(zip_path, exdir = dir_extract)

  todos <- list.files(dir_extract, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  candidatos <- todos[str_detect(toupper(todos), "\\.(TAB|CSV)$")]

  en_csv <- candidatos[str_detect(toupper(candidatos), "[/\\\\]CSV[/\\\\]")]
  if (length(en_csv) > 0) candidatos <- en_csv

  if (es_anexo) {
    elegido <- candidatos[str_detect(toupper(candidatos), "ANEXO")]
  } else {
    elegido <- candidatos[!str_detect(toupper(candidatos), "ANEXO")]
  }

  if (length(elegido) == 0) return(NULL)
  elegido[[1]]
}

leer_anexo_df <- function(path) {
  df <- leer_tab_generico(path)
  req_cols <- c("NVIVI", "FACB2021")
  faltan <- setdiff(req_cols, names(df))
  if (length(faltan) > 0) {
    stop("ANEXO invalido: faltan columnas ", paste(faltan, collapse = ", "))
  }
  fac_raw <- as.character(df$FACB2021)
  fac_num <- dplyr::case_when(
    stringr::str_detect(fac_raw, ",") ~ readr::parse_number(
      fac_raw, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    stringr::str_detect(fac_raw, "\\.") ~ readr::parse_number(
      fac_raw, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    TRUE ~ suppressWarnings(as.numeric(fac_raw))
  )
  tibble(NVIVI = as.character(df$NVIVI), FACB2021 = fac_num) |>
    filter(!is.na(NVIVI), NVIVI != "") |>
    group_by(NVIVI) |>
    summarise(FACB2021 = dplyr::first(FACB2021), .groups = "drop")
}

# Descarga (o recupera de cache) y procesa UN trimestre. Devuelve un data.frame
# ya con FACTOR_USAR, ACTIVIDAD, EDAD_EP calculados, o list(error=...).
cargar_trimestre <- function(anio, trim) {
  etiqueta <- sprintf("%dT%d", anio, trim)
  cache_file <- file.path(CACHE_DIR, sprintf("epa_%s.rds", etiqueta))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  zip_micro <- descargar_zip(url_micro(anio, trim))
  if (is.null(zip_micro)) {
    return(list(error = sprintf(
      "No se pudo descargar el trimestre %s (%s). ¿Aún no está publicado?",
      etiqueta, url_micro(anio, trim)
    )))
  }

  path_micro <- extraer_fichero_datos(zip_micro, es_anexo = FALSE)
  if (is.null(path_micro)) {
    return(list(error = sprintf("No se encontró el fichero de microdatos dentro del zip de %s", etiqueta)))
  }

  df <- leer_tab_generico(path_micro)

  requeridas <- c("CICLO", "CCAA", "PROV", "NVIVI", "AOI", "FACTOREL", "NIVEL")
  faltan <- setdiff(requeridas, names(df))
  if (length(faltan) > 0) {
    return(list(error = sprintf(
      "Trimestre %s: faltan columnas %s", etiqueta, paste(faltan, collapse = ", ")
    )))
  }

  df <- df |>
    select(any_of(cols_keep)) |>
    filter(NIVEL == "1") |>
    mutate(
      CICLO = as.character(CICLO),
      CCAA  = str_pad(as.character(CCAA), 2, pad = "0"),
      PROV  = str_pad(as.character(PROV), 2, pad = "0"),
      NVIVI = as.character(NVIVI),
      AOI   = str_pad(as.character(AOI), 2, pad = "0"),
      FACTOREL = parse_factor_mixto(FACTOREL)
    )

  df <- if (as.integer(df$CICLO[1]) <= 213) {
    df |> mutate(ACTIVIDAD = as.character(ACT1))
  } else {
    df |> mutate(ACTIVIDAD = as.character(ACT09))
  }

  edad5 <- if ("EDAD5" %in% names(df)) as.character(df$EDAD5) else NA_character_
  edad1 <- if ("EDAD1" %in% names(df)) as.character(df$EDAD1) else NA_character_
  df$EDAD_EP <- dplyr::coalesce(edad5, edad1)

  # Cruce con anexo (2021-2023): base poblacional 2021 (FACB2021) en vez de
  # la base 2011 (FACTOREL) del fichero de microdatos de esos trimestres.
  if (anio >= 2021 && anio <= 2023) {
    zip_anexo <- NULL
    for (u in url_anexo_candidatos(anio, trim)) {
      zip_anexo <- descargar_zip(u)
      if (!is.null(zip_anexo)) break
    }
    if (is.null(zip_anexo)) {
      return(list(error = sprintf(
        "Trimestre %s requiere anexo (factor base 2021) y no se pudo descargar con ninguno de los patrones de URL probados.",
        etiqueta
      )))
    }
    path_anexo <- extraer_fichero_datos(zip_anexo, es_anexo = TRUE)
    if (is.null(path_anexo)) {
      return(list(error = sprintf("No se encontró el fichero de anexo dentro del zip de %s", etiqueta)))
    }
    anexo_df <- leer_anexo_df(path_anexo)
    df <- df |>
      left_join(anexo_df, by = "NVIVI", relationship = "many-to-one") |>
      mutate(FACTOR_USAR = FACB2021)

    n_na <- sum(is.na(df$FACTOR_USAR))
    if (n_na > 0) {
      return(list(error = sprintf(
        "Trimestre %s: %d registros sin FACB2021 tras el cruce con el anexo", etiqueta, n_na
      )))
    }
  } else {
    df <- df |> mutate(FACTOR_USAR = FACTOREL)
  }

  df$TRIMESTRE_ANIO <- anio
  df$TRIMESTRE_NUM  <- trim
  df$TRIMESTRE_LABEL <- etiqueta

  saveRDS(df, cache_file)
  df
}

detectar_ultimo_trimestre <- function(max_intentos = 6) {
  hoy <- Sys.Date()
  anio <- as.integer(format(hoy, "%Y"))
  mes  <- as.integer(format(hoy, "%m"))
  trim_natural <- ceiling(mes / 3) - 1
  if (trim_natural < 1) { trim_natural <- 4; anio <- anio - 1 }

  candidato_anio <- anio
  candidato_trim <- trim_natural

  for (i in seq_len(max_intentos)) {
    url <- url_micro(candidato_anio, candidato_trim)
    disponible <- tryCatch({
      resp <- httr::HEAD(url, httr::timeout(15))
      httr::status_code(resp) == 200
    }, error = function(e) FALSE)

    if (disponible) return(list(anio = candidato_anio, trim = candidato_trim))

    candidato_trim <- candidato_trim - 1
    if (candidato_trim < 1) { candidato_trim <- 4; candidato_anio <- candidato_anio - 1 }
  }
  list(anio = anio, trim = trim_natural)
}

ventana_4_trimestres <- function(anio_fin, trim_fin) {
  pares <- vector("list", 4)
  a <- anio_fin; t <- trim_fin
  for (i in 4:1) {
    pares[[i]] <- c(anio = a, trim = t)
    t <- t - 1
    if (t < 1) { t <- 4; a <- a - 1 }
  }
  pares
}

cargar_ventana <- function(anio_fin, trim_fin) {
  pares <- ventana_4_trimestres(anio_fin, trim_fin)
  resultados <- map(pares, ~ cargar_trimestre(.x[["anio"]], .x[["trim"]]))

  errores <- map_chr(resultados, ~ if (!is.null(.x$error)) .x$error else NA_character_)
  errores <- errores[!is.na(errores)]

  dfs <- keep(resultados, ~ is.null(.x$error))
  df <- if (length(dfs) > 0) bind_rows(dfs) else tibble()

  list(df = df, errores = errores)
}

# Todos los trimestres desde ANIO_MIN hasta el último publicado, en orden
# cronológico. Usada por build_epa_tablas.R para el histórico completo.
todos_los_trimestres <- function(ultimo) {
  pares <- list()
  for (a in ANIO_MIN:ultimo$anio) {
    trims <- if (a == ultimo$anio) 1:ultimo$trim else 1:4
    for (t in trims) pares[[length(pares) + 1]] <- c(anio = a, trim = t)
  }
  pares
}

generar_opciones_trimestre <- function(ultimo) {
  anios <- ANIO_MIN:ultimo$anio
  opciones <- c()
  for (a in rev(anios)) {
    trims <- if (a == ultimo$anio) ultimo$trim:1 else 4:1
    for (t in trims) {
      if (a == ultimo$anio && t > ultimo$trim) next
      opciones <- c(opciones, sprintf("%dT%d", a, t))
    }
  }
  opciones
}

parse_trimestre_label <- function(label) {
  m <- str_match(label, "^(\\d{4})T(\\d)$")
  list(anio = as.integer(m[, 2]), trim = as.integer(m[, 3]))
}

trimestre_anterior <- function(anio, trim) {
  t <- trim - 1; a <- anio
  if (t < 1) { t <- 4; a <- a - 1 }
  list(anio = a, trim = t)
}

swap_choices <- function(x) {
  v <- setNames(names(x), unname(x))
  v[order(names(v))]
}

error_de <- function(res) if (is.data.frame(res)) NULL else res[["error"]]
df_de    <- function(res) if (is.data.frame(res)) res else tibble()

# ---- Filtros reutilizables --------------------------------------------------

filtrar_zona <- function(df, ambito, zona) {
  if (nrow(df) == 0) return(df)
  if (ambito == "ccaa") {
    if (is.null(zona) || zona == "") return(df[0, ])
    df <- df |> filter(CCAA == zona)
  } else if (ambito == "prov") {
    if (is.null(zona) || zona == "") return(df[0, ])
    df <- df |> filter(PROV == zona)
  }
  df
}

filtrar_sexo <- function(df, sexo) {
  if (is.null(sexo) || sexo == "ambos") return(df)
  if (nrow(df) == 0) return(df)
  df |> filter(as.character(SEXO1) == sexo)
}

filtrar_nacionalidad <- function(df, nacionalidad) {
  if (is.null(nacionalidad) || nacionalidad == "ambas") return(df)
  if (nrow(df) == 0) return(df)
  if (nacionalidad == "1") {
    df |> filter(as.character(NAC1) %in% c("1", "2"))
  } else {
    df |> filter(as.character(NAC1) == nacionalidad)
  }
}

# -----------------------------------------------------------------------------
# AGREGACIÓN (compartida entre build_epa_tablas.R y el fallback "al vuelo" de
# app.R para cuando hay un trimestre publicado más nuevo que el cacheado en
# data_agregada/). Ver cabecera de build_epa_tablas.R para el detalle del
# esquema de columnas resultante.
# -----------------------------------------------------------------------------

periodo_date <- function(anio, trim) {
  # OJO: "fecha + months(3)" requiere que months() esté redefinida por
  # lubridate (su versión crea un objeto Period); la months() de R base es
  # genérica y solo tiene método para objetos Date/POSIXt, así que
  # months(3) con un número suelto revienta con "no applicable method".
  # epa_helpers.R no carga lubridate, así que usamos seq.Date(), que sí
  # soporta aritmética de meses en R base sin depender de ningún paquete.
  inicio <- as.Date(sprintf("%d-%s-01", anio, c("01", "04", "07", "10")[trim]))
  siguiente_trimestre <- seq(inicio, by = "3 months", length.out = 2)[2]
  siguiente_trimestre - 1
}

agregar_dato_base <- function(df, group_vars) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(if_all(all_of(group_vars), ~ !is.na(.x))) |>
    group_by(across(all_of(c("periodo", group_vars)))) |>
    summarise(
      valor_pob = sum(FACTOR_USAR, na.rm = TRUE),
      valor_act = sum(FACTOR_USAR[AOI %in% c("03", "04", "05", "06")], na.rm = TRUE),
      valor_ocu = sum(FACTOR_USAR[AOI %in% c("03", "04")], na.rm = TRUE),
      valor_par = sum(FACTOR_USAR[AOI %in% c("05", "06")], na.rm = TRUE),
      valor_ina = sum(FACTOR_USAR[AOI %in% c("07", "08", "09")], na.rm = TRUE),
      # Recuentos SIN ponderar (nº de encuestas reales detrás de cada celda),
      # necesarios para stat_quality.R (CV, aviso de muestra insuficiente).
      # valor_* es la estimación poblacional; n_* es el tamaño muestral real.
      n_pob = dplyr::n(),
      n_act = sum(AOI %in% c("03", "04", "05", "06")),
      n_ocu = sum(AOI %in% c("03", "04")),
      n_par = sum(AOI %in% c("05", "06")),
      n_ina = sum(AOI %in% c("07", "08", "09")),
      .groups = "drop"
    ) |>
    mutate(
      tasa_par = 100 * valor_par / valor_act,
      tasa_act = 100 * valor_act / valor_pob,
      tasa_emp = 100 * valor_ocu / valor_pob
    )
}

agregar_ocupados_base <- function(df, group_vars) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(AOI %in% c("03", "04")) |>
    filter(if_all(all_of(group_vars), ~ !is.na(.x))) |>
    group_by(across(all_of(c("periodo", group_vars)))) |>
    summarise(valor_ocu = sum(FACTOR_USAR, na.rm = TRUE), n_ocu = dplyr::n(), .groups = "drop")
}

# Análogo a agregar_ocupados_base() pero para dimensiones que solo están
# informadas para parados (p.ej. duracion_paro, vía ITBU). Usa AOI %in%
# c("05","06") en vez de "está en la tabla epa_paro_larga sumando FACTOR_USAR
# de todos los parados", porque ITBU también puede venir informado para
# alguna persona ya colocada que empieza más tarde; nos ceñimos a los
# parados en sentido estricto (mismo universo que valor_par en las demás
# tablas) para que "% de larga duración" tenga como denominador el mismo
# total de parados que ya usas en el resto del dashboard.
agregar_parados_base <- function(df, group_vars) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(AOI %in% c("05", "06")) |>
    filter(if_all(all_of(group_vars), ~ !is.na(.x))) |>
    group_by(across(all_of(c("periodo", group_vars)))) |>
    summarise(valor_par = sum(FACTOR_USAR, na.rm = TRUE), n_par = dplyr::n(), .groups = "drop")
}

con_totales <- function(fn, df, dims, territorio_vars, etiquetas = character(0)) {
  combinaciones <- unlist(lapply(0:length(dims), combn, x = dims, simplify = FALSE), recursive = FALSE)
  resultados <- lapply(combinaciones, function(a_colapsar) {
    df2 <- df
    for (col in a_colapsar) {
      etq <- if (col %in% names(etiquetas)) etiquetas[[col]] else "Total"
      df2[[col]] <- etq
    }
    fn(df2, c(territorio_vars, dims))
  })
  bind_rows(resultados)
}

enriquecer_trimestre <- function(df, anio, trim) {
  df |>
    mutate(
      periodo       = periodo_date(anio, trim),
      sexo          = sexo_grupo(as.character(SEXO1)),
      edad          = edad_banda(EDAD_EP),
      form          = if ("NFORMA" %in% names(df)) nforma_label(as.character(NFORMA)) else NA_character_,
      nac           = if ("EXREGNA1" %in% names(df)) nacionalidad_grupo(as.character(NAC1), as.character(EXREGNA1)) else NA_character_,
      sector        = sector_grupo(ACTIVIDAD),
      ocupacion     = if ("OCUP1" %in% names(df)) ocupacion_label(as.character(OCUP1)) else NA_character_,
      duracion_paro = if ("ITBU" %in% names(df)) duracion_paro_banda(as.character(ITBU)) else NA_character_,
      region        = unname(ccaa_labels[CCAA]),
      provincia     = unname(prov_labels[PROV])
    )
}

# Calcula las 5 tablas estrella (mismo esquema que data_agregada/epa_*.rds)
# para UN trimestre ya cargado con cargar_trimestre(). La usan tanto
# build_epa_tablas.R como el fallback "al vuelo" de app.R.
calcular_tablas_trimestre <- function(res, anio, trim) {
  df <- enriquecer_trimestre(res, anio, trim)

  edad <- bind_rows(
    con_totales(agregar_dato_base, df, c("edad", "sexo"), "region", c(edad = "total", sexo = "total")),
    con_totales(agregar_dato_base, df |> mutate(region = "España"), c("edad", "sexo"), "region", c(edad = "total", sexo = "total"))
  )
  prov <- bind_rows(
    con_totales(agregar_dato_base, df, c("sexo"), "provincia", c(sexo = "total")),
    con_totales(agregar_dato_base, df |> mutate(provincia = "España"), c("sexo"), "provincia", c(sexo = "total"))
  )
  form <- bind_rows(
    con_totales(agregar_dato_base, df, c("form", "sexo"), "region", c(form = "Total", sexo = "total")),
    con_totales(agregar_dato_base, df |> mutate(region = "España"), c("form", "sexo"), "region", c(form = "Total", sexo = "total"))
  )
  nac_es_ue <- bind_rows(
    con_totales(agregar_dato_base, df, c("sexo"), c("region", "nac"), c(sexo = "total")),
    con_totales(agregar_dato_base, df |> mutate(region = "España"), c("sexo"), c("region", "nac"), c(sexo = "total"))
  )
  df_ex <- df |> filter(nac %in% c("UE", "no_UE")) |> mutate(nac = "EX")
  nac_ex <- bind_rows(
    con_totales(agregar_dato_base, df_ex, c("sexo"), c("region", "nac"), c(sexo = "total")),
    con_totales(agregar_dato_base, df_ex |> mutate(region = "España"), c("sexo"), c("region", "nac"), c(sexo = "total"))
  )
  nac <- bind_rows(nac_es_ue, nac_ex)
  sector <- bind_rows(
    con_totales(agregar_ocupados_base, df, c("sector", "edad", "sexo"), "region", c(sector = "Total", edad = "total", sexo = "total")),
    con_totales(agregar_ocupados_base, df |> mutate(region = "España"), c("sector", "edad", "sexo"), "region", c(sector = "Total", edad = "total", sexo = "total"))
  )
  ocup <- bind_rows(
    con_totales(agregar_ocupados_base, df, c("ocupacion", "sexo"), "region", c(ocupacion = "Total", sexo = "total")),
    con_totales(agregar_ocupados_base, df |> mutate(region = "España"), c("ocupacion", "sexo"), "region", c(ocupacion = "Total", sexo = "total"))
  )
  paro_larga <- bind_rows(
    con_totales(agregar_parados_base, df, c("duracion_paro", "sexo"), "region", c(duracion_paro = "Total", sexo = "total")),
    con_totales(agregar_parados_base, df |> mutate(region = "España"), c("duracion_paro", "sexo"), "region", c(duracion_paro = "Total", sexo = "total"))
  )

  list(edad = edad, prov = prov, form = form, nac = nac, sector = sector, ocup = ocup, paro_larga = paro_larga)
}
