# =============================================================================
# build_epa_tablas.R
# =============================================================================
# Job por lotes (NO es la app Shiny). Recorre todos los trimestres de la EPA
# desde ANIO_MIN (2005) hasta el último publicado, usando cargar_trimestre()
# (misma función que usa app.R, con su misma caché en epa_cache/) y agrega 5
# tablas "estrella" con el MISMO esquema de columnas que las tablas que tu
# dashboard viejo se descargaba ya hechas del INE (para poder reutilizar tal
# cual todo el código de servidor que ya filtra/pinta esas tablas). El cálculo
# en sí (enriquecer_trimestre / agregar_dato_base / con_totales /
# calcular_tablas_trimestre) vive en epa_helpers.R, compartido con el
# fallback "al vuelo" de app.R -- aquí solo se orquesta el bucle por
# trimestre y el guardado incremental.
#
#   epa_edad       : periodo, region, edad, sexo, valor_pob..tasa_emp
#   epa_prov       : periodo, provincia, sexo, valor_pob..tasa_emp
#   epa_form       : periodo, region, form, sexo, valor_pob..tasa_emp
#   epa_nac        : periodo, region, nac, sexo, valor_pob..tasa_emp   (nac
#                    incluye también "EX" = extranjeros = UE + no_UE)
#   epa_sector     : periodo, region, sector, edad, sexo, valor_ocu   (solo ocupados)
#   epa_ocup       : periodo, region, ocupacion, sexo, valor_ocu   (solo ocupados;
#                    ocupacion = 10 grandes grupos CNO vía OCUP1)
#   epa_paro_larga : periodo, region, duracion_paro, sexo, valor_par   (solo
#                    parados con ITBU informado; duracion_paro = tramos
#                    < 3 meses / 3 a 6 meses / 6 meses a 1 año / 1 a 2 años /
#                    2 años o más, vía ITBU)
#
# `periodo` es una fecha (último día del trimestre: "2024-06-30", etc.),
# EXACTAMENTE como en las tablas originales del INE, porque todo el código de
# servidor del dashboard viejo hace aritmética de fechas sobre esa columna.
#
# Cada tabla incluye, además de hombres/mujeres, una fila de "todos" en cada
# dimensión categórica (sexo="total", edad="total", form="Total",
# sector="Total"), calculada sumando de verdad los microdatos de esa
# dimensión, no promediando tasas -- reproduce el desglose "Ambos sexos" /
# "Total" que traían las tablas del INE.
#
# INCREMENTAL: si ya existe data_agregada/epa_*.rds, solo se procesan los
# trimestres que falten (comparando periodo) y se añaden al final.
#
# LIMITACIÓN CONOCIDA: los microdatos de la EPA solo traen el cuestionario de
# actividad económica para NIVEL == "1" (16 años y más). Por tanto estas
# tablas NO incluyen población real en la banda de edad "0-16" (menores),
# que sí traía la tabla 65285 del INE. Se deja mapeada por coherencia de
# etiqueta pero no tendrá filas pobladas.
# =============================================================================

source("epa_helpers.R")

OUT_DIR <- "data_agregada"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
LOG_ERRORES <- file.path(OUT_DIR, "errores_build.log")

# -----------------------------------------------------------------------------
# Determina qué trimestres faltan por procesar en cada tabla de salida,
# comparando contra lo que ya hay guardado (incremental).
# -----------------------------------------------------------------------------
trimestres_pendientes <- function(pares_todos, out_path) {
  if (!file.exists(out_path)) return(pares_todos)
  existente <- readRDS(out_path)
  periodos_existentes <- unique(existente$periodo)
  Filter(function(p) !(periodo_date(p[["anio"]], p[["trim"]]) %in% periodos_existentes), pares_todos)
}

anexar_guardar <- function(nuevo, out_path) {
  if (nrow(nuevo) == 0) return(invisible(NULL))
  cols_valor <- intersect(
    c("valor_pob", "valor_act", "valor_ocu", "valor_par", "valor_ina",
      "tasa_par", "tasa_act", "tasa_emp",
      "n_pob", "n_act", "n_ocu", "n_par", "n_ina"),
    names(nuevo)
  )
  if (file.exists(out_path)) {
    existente <- readRDS(out_path)
    combinado <- bind_rows(existente, nuevo) |>
      distinct(across(-all_of(cols_valor)), .keep_all = TRUE)
  } else {
    combinado <- nuevo
  }
  saveRDS(combinado, out_path)
  message(sprintf("  -> %s: %d filas", out_path, nrow(combinado)))
}

# -----------------------------------------------------------------------------
# MAIN
# -----------------------------------------------------------------------------

ultimo <- detectar_ultimo_trimestre()
message(sprintf("Último trimestre publicado detectado: %dT%d", ultimo$anio, ultimo$trim))

pares_todos <- todos_los_trimestres(ultimo)
message(sprintf("Trimestres en el histórico objetivo (%d-%dT%d): %d",
                 ANIO_MIN, ultimo$anio, ultimo$trim, length(pares_todos)))

out_paths <- list(
  edad       = file.path(OUT_DIR, "epa_edad.rds"),
  prov       = file.path(OUT_DIR, "epa_prov.rds"),
  form       = file.path(OUT_DIR, "epa_form.rds"),
  nac        = file.path(OUT_DIR, "epa_nac.rds"),
  sector     = file.path(OUT_DIR, "epa_sector.rds"),
  ocup       = file.path(OUT_DIR, "epa_ocup.rds"),
  paro_larga = file.path(OUT_DIR, "epa_paro_larga.rds")
)

pendientes_por_tabla <- lapply(out_paths, trimestres_pendientes, pares_todos = pares_todos)
periodos_pendientes <- unique(unlist(lapply(pendientes_por_tabla, function(ps) {
  vapply(ps, function(p) sprintf("%dT%d", p[["anio"]], p[["trim"]]), character(1))
})))

if (length(periodos_pendientes) == 0) {
  message("Nada que hacer: las 5 tablas ya están al día.")
  quit(save = "no", status = 0)
}

message(sprintf("Trimestres a procesar en esta ejecución: %s",
                 paste(sort(periodos_pendientes), collapse = ", ")))

acumulado <- list(edad = list(), prov = list(), form = list(), nac = list(), sector = list(), ocup = list(), paro_larga = list())

for (p in pares_todos) {
  etiqueta <- sprintf("%dT%d", p[["anio"]], p[["trim"]])
  if (!(etiqueta %in% periodos_pendientes)) next

  message(sprintf("Procesando %s ...", etiqueta))
  res <- cargar_trimestre(p[["anio"]], p[["trim"]])
  err <- error_de(res)
  if (!is.null(err)) {
    message(sprintf("  ! %s", err))
    cat(sprintf("[%s] %s: %s\n", Sys.time(), etiqueta, err), file = LOG_ERRORES, append = TRUE)
    next
  }

  tablas <- calcular_tablas_trimestre(res, p[["anio"]], p[["trim"]])
  acumulado$edad[[etiqueta]]       <- tablas$edad
  acumulado$prov[[etiqueta]]       <- tablas$prov
  acumulado$form[[etiqueta]]       <- tablas$form
  acumulado$nac[[etiqueta]]        <- tablas$nac
  acumulado$sector[[etiqueta]]     <- tablas$sector
  acumulado$ocup[[etiqueta]]       <- tablas$ocup
  acumulado$paro_larga[[etiqueta]] <- tablas$paro_larga
}

message("Guardando tablas agregadas...")
anexar_guardar(bind_rows(acumulado$edad),       out_paths$edad)
anexar_guardar(bind_rows(acumulado$prov),       out_paths$prov)
anexar_guardar(bind_rows(acumulado$form),       out_paths$form)
anexar_guardar(bind_rows(acumulado$nac),        out_paths$nac)
anexar_guardar(bind_rows(acumulado$sector),     out_paths$sector)
anexar_guardar(bind_rows(acumulado$ocup),       out_paths$ocup)
anexar_guardar(bind_rows(acumulado$paro_larga), out_paths$paro_larga)

message("Hecho.")
