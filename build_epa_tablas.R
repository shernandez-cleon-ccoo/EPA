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
#   epa_paro_tipo  : periodo, region, tipo_busqueda, sexo, valor_par   (parados
#                    que buscan primer empleo vs. que han trabajado
#                    anteriormente, vía AOI 05/06 -- VERIFICADO contra
#                    dr_EPA_2005/2021/2026.xlsx reales, hoja Tablas1/TAOI)
#   epa_motivo_paro: periodo, region, motivo_fin, sexo, valor_par   (motivo
#                    de fin del último empleo -- despido/fin de contrato vs.
#                    jubilación vs. otros -- vía RZULT. SOLO 2021+: RZULT no
#                    existe en el diseño 2005-2020, verificado contra los 3
#                    dr_EPA reales; motivo_fin siempre NA y esta tabla no
#                    tiene filas para trimestres anteriores a T1-2021, por
#                    diseño, no por error)
#   epa_calidad    : periodo, region, edad, sexo, sector, valor_ocu/asal/
#                    temporal/indefinido/fijo_discontinuo/parcial/
#                    parcial_involuntario + tasas ya calculadas (temporalidad,
#                    fijo_discontinuo, parcialidad, parcialidad_involuntaria).
#                    Cubre las pestañas Calidad del empleo / Temporalidad /
#                    Jornada y parcialidad, vía DUCON1/DUCON2/PARCO1/PARCO2.
#   epa_flujos     : periodo, region, sexo, situacion_actual, situacion_anterior,
#                    valor, tipo_dato. NO se calcula desde microdatos como el
#                    resto de tablas: se descarga tal cual de la Estadística de
#                    Flujos de la Población Activa (EFPA) oficial del INE (tabla
#                    Tempus3 nº 66266, por CCAA) -- ver ine_flujos.R para el
#                    porqué (reconstruir el panel rotante desde microdatos
#                    públicos no es viable: falta la garantía de estabilidad de
#                    NPERS entre oleadas y la calibración exacta del INE contra
#                    proyecciones de población, confirmado tras revisar los 3
#                    diseños de registro reales y la metodología EFPA oficial).
#                    sexo siempre "total" (la tabla por CCAA no desglosa por
#                    sexo; ver limitación documentada en ine_flujos.R).
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
source("ine_flujos.R")

OUT_DIR <- "data_agregada"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# El log de errores vive FUERA de data_agregada/ a propósito: el workflow de
# GitHub Actions hace "git add data_agregada" para detectar y commitear
# cambios en las tablas, y si el log estuviera dentro (i) crecería para
# siempre en el historial de git sin rotar nunca, y (ii) un simple error de
# red transitorio (sin que ninguna tabla cambie de verdad) dispararía un
# commit "Actualizar tablas EPA" engañoso. El log solo se sube como artifact
# de la ejecución (ver workflow), no se versiona.
LOG_DIR <- "logs"
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)
LOG_ERRORES <- file.path(LOG_DIR, "errores_build.log")
# Se trunca al empezar cada ejecución para que el artifact subido refleje
# solo los errores de ESTA ejecución, no un acumulado infinito de días.
if (file.exists(LOG_ERRORES)) file.remove(LOG_ERRORES)

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
      "n_pob", "n_act", "n_ocu", "n_par", "n_ina",
      "valor_asal", "n_asal", "valor_temporal", "n_temporal",
      "valor_indefinido", "n_indefinido", "valor_fijo_discontinuo", "n_fijo_discontinuo",
      "valor_parcial", "n_parcial", "valor_parcial_involuntario", "n_parcial_involuntario",
      "tasa_temporalidad", "tasa_fijo_discontinuo", "tasa_parcialidad", "tasa_parcialidad_involuntaria"),
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
  paro_larga = file.path(OUT_DIR, "epa_paro_larga.rds"),
  paro_tipo  = file.path(OUT_DIR, "epa_paro_tipo.rds"),
  motivo_paro = file.path(OUT_DIR, "epa_motivo_paro.rds"),
  calidad    = file.path(OUT_DIR, "epa_calidad.rds")
)

pendientes_por_tabla <- lapply(out_paths, trimestres_pendientes, pares_todos = pares_todos)
periodos_pendientes <- unique(unlist(lapply(pendientes_por_tabla, function(ps) {
  vapply(ps, function(p) sprintf("%dT%d", p[["anio"]], p[["trim"]]), character(1))
})))

# -----------------------------------------------------------------------------
# epa_flujos (EFPA oficial del INE, vía API) -- se descarga SIEMPRE, antes del
# posible "nada que hacer" de más abajo. No es incremental por trimestre como
# las otras 10 tablas (esas se calculan desde microdatos que descargamos
# nosotros; esta ya viene calculada del INE), así que su cadencia de
# actualización es independiente: aunque las 10 tablas de microdatos ya
# estén al día, la EFPA puede tener un trimestre nuevo (o una revisión de uno
# anterior) que sí queremos recoger. Nunca bloquea el resto del build: si
# falla, se deja constancia en el log y se conserva la versión anterior en
# disco (si la había) tal cual estaba.
message("Descargando Estadística de Flujos de la Población Activa (EFPA) del INE...")
flujos_ccaa <- descargar_flujos_ccaa()
ruta_flujos <- file.path(OUT_DIR, "epa_flujos.rds")
if (nrow(flujos_ccaa) > 0) {
  saveRDS(flujos_ccaa, ruta_flujos)
  message(sprintf("  -> %s: %d filas (%d periodos, %s-%s)",
                   ruta_flujos, nrow(flujos_ccaa), length(unique(flujos_ccaa$periodo)),
                   format(min(flujos_ccaa$periodo)), format(max(flujos_ccaa$periodo))))
} else {
  aviso <- "No se pudo descargar epa_flujos (EFPA) del INE en este build; se conserva la versión anterior si existía."
  message(sprintf("  ! %s", aviso))
  cat(sprintf("[%s] %s\n", Sys.time(), aviso), file = LOG_ERRORES, append = TRUE)
}

if (length(periodos_pendientes) == 0) {
  message("Nada más que hacer: las 10 tablas de microdatos ya están al día.")
  quit(save = "no", status = 0)
}

message(sprintf("Trimestres a procesar en esta ejecución: %s",
                 paste(sort(periodos_pendientes), collapse = ", ")))

acumulado <- list(edad = list(), prov = list(), form = list(), nac = list(), sector = list(), ocup = list(), paro_larga = list(), paro_tipo = list(), motivo_paro = list(), calidad = list())

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
  acumulado$paro_tipo[[etiqueta]]  <- tablas$paro_tipo
  acumulado$motivo_paro[[etiqueta]] <- tablas$motivo_paro
  acumulado$calidad[[etiqueta]]    <- tablas$calidad
}

message("Guardando tablas agregadas...")
anexar_guardar(bind_rows(acumulado$edad),       out_paths$edad)
anexar_guardar(bind_rows(acumulado$prov),       out_paths$prov)
anexar_guardar(bind_rows(acumulado$form),       out_paths$form)
anexar_guardar(bind_rows(acumulado$nac),        out_paths$nac)
anexar_guardar(bind_rows(acumulado$sector),     out_paths$sector)
anexar_guardar(bind_rows(acumulado$ocup),       out_paths$ocup)
anexar_guardar(bind_rows(acumulado$paro_larga), out_paths$paro_larga)
anexar_guardar(bind_rows(acumulado$paro_tipo),  out_paths$paro_tipo)
anexar_guardar(bind_rows(acumulado$motivo_paro), out_paths$motivo_paro)
anexar_guardar(bind_rows(acumulado$calidad),    out_paths$calidad)

# -----------------------------------------------------------------------------
# Chequeo de plausibilidad de epa_paro_tipo (ver nota de verificación
# pendiente en epa_helpers.R::tipo_busqueda_grupo()): el INE publica que
# "parados que buscan primer empleo" es SIEMPRE una minoría clara del total
# de parados (nunca por debajo de ~3% ni por encima de ~30% en toda la serie
# histórica 2005-2026, ni siquiera en los peores años de paro juvenil). Si el
# cálculo con los códigos AOI 05/06 asignados en tipo_busqueda_grupo() da un
# resultado fuera de ese rango, lo más probable es que estén invertidos --
# esto avisa fuerte en el log en vez de dejar que un dato mal etiquetado se
# cuele en el dashboard en silencio.
# -----------------------------------------------------------------------------
PLAUSIBLE_PRIMER_EMPLEO_PCT <- c(min = 3, max = 30)

tabla_paro_tipo_final <- if (file.exists(out_paths$paro_tipo)) readRDS(out_paths$paro_tipo) else NULL
if (!is.null(tabla_paro_tipo_final) && nrow(tabla_paro_tipo_final) > 0) {
  ultima_fila_espana <- tabla_paro_tipo_final |>
    dplyr::filter(region == "España", sexo == "total", periodo == max(periodo))
  total_parados   <- ultima_fila_espana$valor_par[ultima_fila_espana$tipo_busqueda == "Total"]
  primer_empleo   <- ultima_fila_espana$valor_par[ultima_fila_espana$tipo_busqueda == "primer_empleo"]

  if (length(total_parados) == 1 && length(primer_empleo) == 1 && total_parados > 0) {
    pct_primer_empleo <- 100 * primer_empleo / total_parados
    fuera_de_rango <- pct_primer_empleo < PLAUSIBLE_PRIMER_EMPLEO_PCT["min"] ||
                       pct_primer_empleo > PLAUSIBLE_PRIMER_EMPLEO_PCT["max"]
    if (fuera_de_rango) {
      aviso <- sprintf(
        paste0(
          "AVISO IMPORTANTE: epa_paro_tipo da %.1f%% de parados 'primer_empleo' en el ",
          "último trimestre (%s), fuera del rango plausible [%d%%-%d%%]. Esto sugiere que ",
          "tipo_busqueda_grupo() en epa_helpers.R tiene los códigos AOI 05/06 invertidos. ",
          "Revisa el diseño de registro del INE (dr_EPA_*.xlsx, columna AOI) y si hace falta ",
          "invierte la asignación en tipo_busqueda_grupo() antes de dar por buena esta tabla."
        ),
        pct_primer_empleo, format(max(tabla_paro_tipo_final$periodo)),
        PLAUSIBLE_PRIMER_EMPLEO_PCT["min"], PLAUSIBLE_PRIMER_EMPLEO_PCT["max"]
      )
      message(sprintf("  ! %s", aviso))
      cat(sprintf("[%s] %s\n", Sys.time(), aviso), file = LOG_ERRORES, append = TRUE)
    } else {
      message(sprintf("  -> epa_paro_tipo: %.1f%% de parados son 'primer_empleo' (dentro de lo plausible).", pct_primer_empleo))
    }
  }
}

message("Hecho.")
