# =============================================================================
# build_epa_tablas.R
# =============================================================================
# Job por lotes (NO es la app Shiny). Recorre todos los trimestres de la EPA
# desde ANIO_MIN (2005) hasta el último publicado, usando cargar_trimestre()
# (misma función que usa app.R, con su misma caché en epa_cache/) y agrega 5
# tablas "estrella" con la forma de las tablas que tu dashboard viejo se
# descargaba ya hechas del INE:
#
#   epa_edad   : periodo x región (CCAA) x edad x sexo
#   epa_prov   : periodo x provincia x sexo
#   epa_form   : periodo x región (CCAA) x formación x sexo
#   epa_nac    : periodo x región (CCAA) x nacionalidad (ES/UE/no_UE) x sexo
#   epa_sector : periodo x región (CCAA) x sector x edad x sexo   (solo ocupados)
#
# Cada una en formato "ancho": pob, act, ocu, par, ina, tasa_par, tasa_act,
# tasa_emp -- más consistente que el long/wide mezclado del script original,
# y así el server de la app solo tiene que hacer filter() + pivot si hace
# falta.
#
# INCREMENTAL: si ya existe data_agregada/epa_*.rds, solo se procesan los
# trimestres que falten (comparando TRIMESTRE_LABEL) y se añaden al final.
# La primera ejecución completa (2005-hoy) es la única que descarga todo el
# histórico; las siguientes (p.ej. desde GitHub Actions cada trimestre) solo
# tocan el trimestre nuevo.
#
# LIMITACIÓN CONOCIDA: los microdatos de la EPA solo traen el cuestionario de
# actividad económica para NIVEL == "1" (16 años y más). Por tanto estas
# tablas NO incluyen la banda de edad "0-15" que sí traía la tabla 65285 del
# INE (población por edad, que sí cubre menores). Si se necesita ese dato hay
# que leer también los registros de NIVEL distinto de "1" (roster del hogar),
# que no llevan las variables de actividad/formación. Se deja fuera por ahora;
# avisar si hace falta y se añade como tabla aparte.
# =============================================================================

source("epa_helpers.R")

OUT_DIR <- "data_agregada"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
LOG_ERRORES <- file.path(OUT_DIR, "errores_build.log")

# -----------------------------------------------------------------------------
# Agregación genérica: cuenta pob/act/ocu/par/ina (FACTOR_USAR ponderado según
# AOI) para un data.frame de microdatos ya cargado, agrupando por
# TRIMESTRE_LABEL/ANIO/NUM + las columnas extra que se le pasen.
# -----------------------------------------------------------------------------
agregar_dato <- function(df, extra_group_vars) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(if_all(all_of(extra_group_vars), ~ !is.na(.x))) |>
    group_by(across(all_of(c("TRIMESTRE_LABEL", "TRIMESTRE_ANIO", "TRIMESTRE_NUM", extra_group_vars)))) |>
    summarise(
      valor_pob = sum(FACTOR_USAR, na.rm = TRUE),
      valor_act = sum(FACTOR_USAR[AOI %in% c("03", "04", "05", "06")], na.rm = TRUE),
      valor_ocu = sum(FACTOR_USAR[AOI %in% c("03", "04")], na.rm = TRUE),
      valor_par = sum(FACTOR_USAR[AOI %in% c("05", "06")], na.rm = TRUE),
      valor_ina = sum(FACTOR_USAR[AOI %in% c("07", "08", "09")], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      tasa_par = 100 * valor_par / valor_act,
      tasa_act = 100 * valor_act / valor_pob,
      tasa_emp = 100 * valor_ocu / valor_pob
    )
}

# Igual que agregar_dato() pero calculado SOLO sobre ocupados (para sector,
# que solo tiene sentido para quien tiene/tuvo empleo) y sin tasas.
agregar_ocupados <- function(df, extra_group_vars) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(AOI %in% c("03", "04")) |>
    filter(if_all(all_of(extra_group_vars), ~ !is.na(.x))) |>
    group_by(across(all_of(c("TRIMESTRE_LABEL", "TRIMESTRE_ANIO", "TRIMESTRE_NUM", extra_group_vars)))) |>
    summarise(valor_ocu = sum(FACTOR_USAR, na.rm = TRUE), .groups = "drop")
}

# Enriquece un trimestre ya cargado (cargar_trimestre) con las columnas
# derivadas (SEXO, EDAD, FORM, NACGRUPO, SECTOR, REGION, PROVINCIA) que usan
# las 5 tablas.
enriquecer_trimestre <- function(df) {
  df |>
    mutate(
      SEXO      = sexo_grupo(as.character(SEXO1)),
      EDAD      = edad_banda(EDAD_EP),
      FORM      = if ("NFORMA" %in% names(df)) nforma_label(as.character(NFORMA)) else NA_character_,
      NACGRUPO  = if ("EXREGNA1" %in% names(df)) nacionalidad_grupo(as.character(NAC1), as.character(EXREGNA1)) else NA_character_,
      SECTOR    = sector_grupo(ACTIVIDAD),
      REGION    = unname(ccaa_labels[CCAA]),
      PROVINCIA = unname(prov_labels[PROV])
    )
}

# -----------------------------------------------------------------------------
# Determina qué trimestres faltan por procesar en cada tabla de salida,
# comparando contra lo que ya hay guardado (incremental).
# -----------------------------------------------------------------------------
trimestres_pendientes <- function(pares_todos, out_path) {
  if (!file.exists(out_path)) return(pares_todos)
  existente <- readRDS(out_path)
  labels_existentes <- unique(existente$TRIMESTRE_LABEL)
  Filter(function(p) !(sprintf("%dT%d", p[["anio"]], p[["trim"]]) %in% labels_existentes), pares_todos)
}

anexar_guardar <- function(nuevo, out_path) {
  if (nrow(nuevo) == 0) return(invisible(NULL))
  if (file.exists(out_path)) {
    existente <- readRDS(out_path)
    combinado <- bind_rows(existente, nuevo) |>
      distinct(across(-c(valor_pob, valor_act, valor_ocu, valor_par, valor_ina,
                          tasa_par, tasa_act, tasa_emp)), .keep_all = TRUE)
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
  edad   = file.path(OUT_DIR, "epa_edad.rds"),
  prov   = file.path(OUT_DIR, "epa_prov.rds"),
  form   = file.path(OUT_DIR, "epa_form.rds"),
  nac    = file.path(OUT_DIR, "epa_nac.rds"),
  sector = file.path(OUT_DIR, "epa_sector.rds")
)

# Unión de los trimestres que falten en CUALQUIERA de las 5 tablas: así solo
# descargamos/leemos cada trimestre una vez aunque falte en varias tablas.
pendientes_por_tabla <- lapply(out_paths, trimestres_pendientes, pares_todos = pares_todos)
labels_pendientes <- unique(unlist(lapply(pendientes_por_tabla, function(ps) {
  vapply(ps, function(p) sprintf("%dT%d", p[["anio"]], p[["trim"]]), character(1))
})))

if (length(labels_pendientes) == 0) {
  message("Nada que hacer: las 5 tablas ya están al día.")
  quit(save = "no", status = 0)
}

message(sprintf("Trimestres a procesar en esta ejecución: %s",
                 paste(sort(labels_pendientes), collapse = ", ")))

acumulado <- list(edad = list(), prov = list(), form = list(), nac = list(), sector = list())

for (p in pares_todos) {
  etiqueta <- sprintf("%dT%d", p[["anio"]], p[["trim"]])
  if (!(etiqueta %in% labels_pendientes)) next

  message(sprintf("Procesando %s ...", etiqueta))
  res <- cargar_trimestre(p[["anio"]], p[["trim"]])
  err <- error_de(res)
  if (!is.null(err)) {
    message(sprintf("  ! %s", err))
    cat(sprintf("[%s] %s: %s\n", Sys.time(), etiqueta, err), file = LOG_ERRORES, append = TRUE)
    next
  }

  df <- enriquecer_trimestre(res)

  acumulado$edad[[etiqueta]] <- bind_rows(
    agregar_dato(df, c("REGION", "EDAD", "SEXO")),
    agregar_dato(df |> mutate(REGION = "España"), c("REGION", "EDAD", "SEXO"))
  )
  acumulado$prov[[etiqueta]] <- bind_rows(
    agregar_dato(df, c("PROVINCIA", "SEXO")),
    agregar_dato(df |> mutate(PROVINCIA = "España"), c("PROVINCIA", "SEXO"))
  )
  acumulado$form[[etiqueta]] <- bind_rows(
    agregar_dato(df, c("REGION", "FORM", "SEXO")),
    agregar_dato(df |> mutate(REGION = "España"), c("REGION", "FORM", "SEXO"))
  )
  acumulado$nac[[etiqueta]] <- bind_rows(
    agregar_dato(df, c("REGION", "NACGRUPO", "SEXO")),
    agregar_dato(df |> mutate(REGION = "España"), c("REGION", "NACGRUPO", "SEXO"))
  )
  acumulado$sector[[etiqueta]] <- bind_rows(
    agregar_ocupados(df, c("REGION", "SECTOR", "EDAD", "SEXO")),
    agregar_ocupados(df |> mutate(REGION = "España"), c("REGION", "SECTOR", "EDAD", "SEXO"))
  )
}

message("Guardando tablas agregadas...")
anexar_guardar(bind_rows(acumulado$edad),   out_paths$edad)
anexar_guardar(bind_rows(acumulado$prov),   out_paths$prov)
anexar_guardar(bind_rows(acumulado$form),   out_paths$form)
anexar_guardar(bind_rows(acumulado$nac),    out_paths$nac)
anexar_guardar(bind_rows(acumulado$sector), out_paths$sector)

message("Hecho.")
