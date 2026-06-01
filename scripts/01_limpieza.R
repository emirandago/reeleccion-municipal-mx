# ══════════════════════════════════════════════════════════════════════════════
# Script:   01_limpieza.R
# Proyecto: El municipio y la reelección
# Autor:  Emiliano Miranda González — ITAM
# Fecha:    2026-05-25
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Limpiar y reestructurar la base electoral a nivel sección electoral (Larreguy et al.
#   2025) produciendo dos paneles listos para estimación causal:
#     (1) panel_municipal_clean.rds  — una obs por (municipio-elección), insumo
#         del análisis descriptivo y de las pruebas de balance.
#     (2) panel_rdd_stacked.rds — panel apilado ganador + runner-up (dos observaciones
#         por cada municipio-elección) con margin_signed (el ganador lleva el margen de
#         victoria positivo y el perdedor lleva ese mismo margen con signo negativo, de 
#         modo que el cutoff c = 0 del rdrobust separe ganadores de perdedores en una 
#         sola variable), insumo directo de rdrobust() y rdplot().
#
# DISEÑO DE EXCLUSIONES (decisiones metodológicas):
#   • CDMX (edon=9): EXCLUIDA. Las alcaldías operan bajo
#     una estructura político-administrativa distinta a los municipios del
#     resto de México y su adscripción a cohort_g=2021 es disputada.
#   • Nayarit (edon=18): fecha de adopción en 2024.
#   • Tlaxcala (edon=29): adopción 2024, un solo ciclo post-tratamiento en
#     la muestra; insuficiente para separar efecto de shocks idiosincráticos.
#
# GRUPO DE CONTROL (never-treated):
#   Hidalgo (edon=13) y Veracruz (edon=30). Son los únicos estados que nunca
#   adoptaron reelección municipal consecutiva. G = 2 clusters — inferencia
#   con wild bootstrap (MacKinnon, Nielsen y Webb 2023, JoE).
#
# SUPUESTO SOBRE margin_signed (RDD):
#   mun_winning_margin en Larreguy (2025) siempre es positivo — registra el
#   margen de victoria del incumbente en t-1 (ganador por definición). Para
#   el RDD completo (Lee 2008; Lucardi y Rosas 2016) se necesitan también los
#   márgenes del lado perdedor. SOLUCIÓN: panel apilado con dos obs por
#   elección, una con margin_signed > 0 (ganador) y otra con margin_signed < 0
#   (runner-up). El cutoff c=0 divide entonces ganadores de perdedores.
#
# INPUTS (data/raw/ — SOLO LECTURA; nunca modificar):
#   • all_states_almost.csv         Larreguy et al. 2026, sección electoral, 1994–2024
#   • fechas_adopcion_reeleccion.csv  Magar, cohortes de adopción por estado
#
# OUTPUTS (data/clean/):
#   • panel_municipal_clean.rds     Panel mun-elección; insumo de descriptivos
#   • panel_municipal_clean.csv     Copia auditable
#   • panel_rdd_stacked.rds         Panel apilado; insumo de rdrobust/rdplot
#
# NOTA UNICODE: readr::read_csv() falla en rutas macOS APFS con caracteres
#   acentuados (normalización NFD). Solución: abrir conexión con base::file()
#   y pasarla como argumento a read_csv(). Ver PASO 1.
# ══════════════════════════════════════════════════════════════════════════════


# ── PASO 0: Configuración y paquetes ──────────────────────────────────────────

# conflicted garantiza que cada función usada sea la que el programador eligió,
# no la que R resuelve por precedencia de namespaces. Indispensable cuando
# tidyverse, fixest y otros paquetes definen funciones con el mismo nombre.
suppressPackageStartupMessages({
  library(conflicted)
  conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, .quiet = TRUE)
  library(tidyverse)
})

options(scipen = 999, digits = 4)

# Directorios de output — se crean si no existen; idempotente.
dir.create("data/clean",  showWarnings = FALSE, recursive = TRUE)
dir.create("output",      showWarnings = FALSE, recursive = TRUE)

# Guardar sessionInfo() para reproducibilidad completa. Cualquier persona que
# replique el análisis podrá verificar qué versiones de paquetes se usaron.
writeLines(capture.output(sessionInfo()), "output/session_info_01_limpieza.txt")

cat("\n═════════════════════════════════════════════════════════════════════════\n")
cat(" 01_limpieza.R — Pipeline de replicación\n")
cat(" El municipio y la reelección. (Des)ventaja del partido incumbente en México:  evidencia de un diseño de diferencias en discontinuidades\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ── PASO 1: Cargar fuentes raw ────────────────────────────────────────────────

cat("── PASO 1: Cargando fuentes raw (SOLO LECTURA) ─────────────────────────\n")

# NOTA TÉCNICA (macOS APFS + NFD):
# readr::read_csv() falla en rutas con tildes cuando el filesystem normaliza
# en NFD (Canonical Decomposition). Abrir con base::file() — que sí maneja
# NFD — y pasar la conexión a read_csv() resuelve el problema sin cambiar rutas.

con_larreguy  <- file("data/raw/all_states_almost.csv", "rb")
d_raw         <- read_csv(con_larreguy, show_col_types = FALSE)
close(con_larreguy)

# Verificaciones de integridad mínimas. Si fallan, el problema está en
# data/raw/ o en la versión del archivo (Larreguy et al. 2026 extended version).
stopifnot(
  "La base debe tener >500,000 observaciones (nivel sección electoral)" =
    nrow(d_raw) > 500000L,
  "Faltan columnas requeridas en la base Larreguy" =
    all(c(
      "state_code", "mun_code", "year",
      "mun_winning_margin", "share_incumbent_party_valid_vote",
      "share_runnerup_party_valid_vote",
      "turnout", "incumbent_party", "runnerup_party"
    ) %in% names(d_raw))
)

cat(sprintf(
  "✓ Larreguy: %s filas (nivel sección electoral), %d–%d, %d variables\n",
  format(nrow(d_raw), big.mark = ","),
  min(d_raw$year), max(d_raw$year),
  ncol(d_raw)
))

con_adopcion <- file("data/raw/fechas_adopcion_reeleccion.csv", "rb")
adopcion     <- read_csv(con_adopcion, show_col_types = FALSE)
close(con_adopcion)

stopifnot(
  "Faltan columnas requeridas en tabla de adopción" =
    all(c("edon", "estado", "cohort_g", "grupo_tratamiento") %in% names(adopcion))
)

cat(sprintf("✓ Adopción: %d estados clasificados por cohorte\n\n", nrow(adopcion)))


# ── PASO 2: Colapsar sección electoral → municipio-año ──────────────────────────────────

cat("── PASO 2: Colapsando sección electoral → municipio-elección ───────────────\n")

# POR QUÉ colapsar: el RDD y las pruebas de balance operan a nivel municipio-año.
# Mezclar niveles (sección electoral y municipio) introduce pseudo-replicación y sobreestima
# la precisión de los estimadores. El outcome real es único por municipio-año
# (resultado final de la elección), así que colapsar con first() es exacto.
#
# REGLA CRÍTICA: NUNCA promediar shares a nivel sección electoral — eso sesga por
# tamaño diferencial entre sección electorals (sesgo de agregación ecológica de Simpson).
# En su lugar: sumar conteos y recalcular el share desde el total municipal.

d_mun <- d_raw |>
  group_by(state_code, mun_code, year) |>
  summarise(
    # Identificadores constantes dentro del municipio-año → first()
    state                              = first(state),
    mun                                = first(mun),
    incumbent_party                    = first(incumbent_party),
    incumbent_party_candidate          = first(incumbent_party_candidate),
    runnerup_party                     = first(runnerup_party),
    runnerup_party_candidate           = first(runnerup_party_candidate),
    state_incumbent_party              = first(state_incumbent_party),
    researched_incumbent_party         = first(researched_incumbent_party),
    mun_winning_margin                 = first(mun_winning_margin),

    # Conteos físicos → sum() sobre sección electorals
    incumbent_party_vote               = sum(incumbent_party_vote,       na.rm = TRUE),
    runnerup_party_vote                = sum(runnerup_party_vote,        na.rm = TRUE),
    state_incumbent_party_vote         = sum(state_incumbent_party_vote, na.rm = TRUE),
    PRI_vote                           = sum(PRI_vote,                   na.rm = TRUE),
    PRD_vote                           = sum(PRD_vote,                   na.rm = TRUE),
    PAN_vote                           = sum(PAN_vote,                   na.rm = TRUE),
    MORENA_vote                        = sum(MORENA_vote,                na.rm = TRUE),
    valid                              = sum(valid,                      na.rm = TRUE),
    total                              = sum(total,                      na.rm = TRUE),
    registered_voters                  = sum(registered_voters,          na.rm = TRUE),
    n_secciones                         = n(),
    .groups = "drop"
  ) |>
  # Recalcular shares desde conteos municipales agregados
  mutate(
    share_incumbent_party_valid_vote   = incumbent_party_vote       / valid,
    share_runnerup_party_valid_vote    = runnerup_party_vote        / valid,
    share_state_incumbent_party_valid_vote = state_incumbent_party_vote / valid,
    share_PRI_valid_vote               = PRI_vote    / valid,
    share_PRD_valid_vote               = PRD_vote    / valid,
    share_PAN_valid_vote               = PAN_vote    / valid,
    share_MORENA_valid_vote            = MORENA_vote / valid,
    turnout                            = total / registered_voters
  )

cat(sprintf("✓ Colapso: %s obs únicas municipio-año\n\n",
            format(nrow(d_mun), big.mark = ",")))


# ── PASO 3: Homologar identificadores ─────────────────────────────────────────

cat("── PASO 3: Homologando identificadores Larreguy → convención proyecto ───\n")

# state_code (Larreguy) → edon (convención Magar y del proyecto)
# mun_code   (Larreguy) → mun_id (convención del proyecto)
# SUPUESTO verificado: state_code y edon son ambos 1-32, con CDMX=9.

d_mun <- d_mun |>
  rename(edon = state_code, mun_id = mun_code)

cat(sprintf("✓ Identificadores renombrados. Estados detectados: %d\n\n",
            n_distinct(d_mun$edon)))


# ── PASO 4: Exclusiones metodológicas con flujo de atrición ──────────────────

cat("── PASO 4: Exclusiones metodológicas ───────────────────────────────────\n")

# Función auxiliar: registrar N antes de cada exclusión (para apéndice A del paper)
registrar_paso <- function(datos, paso_nm, descripcion) {
  tibble(
    paso        = paso_nm,
    descripcion = descripcion,
    n_obs       = nrow(datos),
    n_mun_uniq  = n_distinct(datos$mun_id)
  )
}

attricion <- registrar_paso(d_mun, "0. Base colapsada", "Sin exclusiones")

## 4a. Excluir CDMX (edon = 9) ─────────────────────────────────────────────────
# JUSTIFICACIÓN: Las alcaldías de la CDMX operan bajo la Ley Orgánica de
# Alcaldías de la CDMX (2018), distinta del marco jurídico-electoral de los
# municipios del resto del país. Además, la reelección de alcaldías se
# introdujo en 2021 bajo condiciones institucionales específicas de la Ciudad,
# no equiparables al mecanismo de reforma analizado. Excluir es la posición
# metodológicamente más conservadora y la que protege la validez interna del
# diseño.
d_mun     <- d_mun |> filter(edon != 9L)
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4a. Excluir CDMX (edon=9)",
                 "Alcaldías: marco jurídico distinto al municipal"))
cat(sprintf("  4a. CDMX excluida. Restan %s obs\n", format(nrow(d_mun), big.mark = ",")))

## 4b. Excluir Nayarit (edon = 18) ─────────────────────────────────────────────
# cohort_g=2024. Con datos hasta 2024, existe un solo ciclo post-tratamiento
# en la muestra. Un ciclo es insuficiente para distinguir el efecto promedio
# del tratamiento de shocks electorales idiosincráticos en ese año.
d_mun     <- d_mun |> filter(edon != 18L)
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4b. Excluir Nayarit (edon=18)",
                 "Fecha adopción incierta (Magar: yr1st='2024?')"))
cat(sprintf("  4b. Nayarit excluida. Restan %s obs\n", format(nrow(d_mun), big.mark = ",")))

## 4c. Excluir Tlaxcala (edon = 29) ────────────────────────────────────────────
# cohort_g=2024. Con datos hasta 2024, existe un solo ciclo post-tratamiento
# en la muestra. Un ciclo es insuficiente para distinguir el efecto promedio
# del tratamiento de shocks electorales idiosincráticos en ese año.
d_mun     <- d_mun |> filter(edon != 29L)
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4c. Excluir Tlaxcala (edon=29)",
                 "cohort_g=2024: solo 1 ciclo post-tratamiento disponible"))
cat(sprintf("  4c. Tlaxcala excluida. Restan %s obs\n", format(nrow(d_mun), big.mark = ",")))

## 4d. Corregir Inf en turnout (registered_voters = 0 en fuente) ───────────────
# Inf aparece cuando el denominador (lista nominal capturada) es cero en la
# fuente. No es un valor observado: se convierte a NA para preservar la fila.
n_inf <- sum(is.infinite(d_mun$turnout))
d_mun <- d_mun |>
  mutate(turnout = if_else(is.infinite(turnout), NA_real_, turnout))
cat(sprintf("  4d. Inf→NA en turnout: %d obs corregidas\n", n_inf))

## 4e. Filtrar turnout > 1 ──────────────────────────────────────────────────────
# Error de codificación en la fuente (factor de escala de 0-100 en lugar de
# 0-1). Valores imposibles en una proporción invalidan la observación.
n_antes   <- nrow(d_mun)
d_mun     <- d_mun |> filter(turnout <= 1 | is.na(turnout))
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4e. Turnout > 1",
                 paste0(n_antes - nrow(d_mun), " obs con turnout imposible")))
cat(sprintf("  4e. Turnout > 1: %d obs excluidas. Restan %s\n",
            n_antes - nrow(d_mun), format(nrow(d_mun), big.mark = ",")))

## 4f. Excluir NA en mun_winning_margin (running variable) ────────────────────
# Sin la running variable, la observación es inutilizable para el RDD:
# no puede ubicarse a ningún lado del cutoff c=0.
n_antes   <- nrow(d_mun)
d_mun     <- d_mun |> filter(!is.na(mun_winning_margin))
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4f. NA en running variable",
                 paste0(n_antes - nrow(d_mun), " obs sin mun_winning_margin")))
cat(sprintf("  4f. NA en running var: %d obs. Restan %s\n",
            n_antes - nrow(d_mun), format(nrow(d_mun), big.mark = ",")))

## 4g. Excluir sin runnerup_party ───────────────────────────────────────────────
# mun_winning_margin solo es interpretable como "margen frente al segundo
# lugar" si existe un partido rival documentado. Sin partido en segundo lugar,
# el margen sería trivialmente 1 (monopolio) y el mecanismo disciplinario
# electoral no aplica.
n_antes   <- nrow(d_mun)
d_mun     <- d_mun |> filter(!is.na(runnerup_party))
attricion <- bind_rows(attricion,
  registrar_paso(d_mun, "4g. Sin runnerup_party",
                 paste0(n_antes - nrow(d_mun), " obs sin partido en 2do lugar")))
cat(sprintf("  4g. Sin runner-up: %d obs. Restan %s\n\n",
            n_antes - nrow(d_mun), format(nrow(d_mun), big.mark = ",")))


# ── PASO 5: Merge con tabla de adopción escalonada ────────────────────────────

cat("── PASO 5: Merge con tabla de adopción (Magar 2024) ────────────────────\n")

# left_join() — no inner_join() — para preservar todas las observaciones
# del panel y detectar estados sin clasificar (edon sin cohort_g asignado).
# Un inner_join() silenciaría esos casos; aquí generan NA que el stopifnot
# siguiente captura explícitamente.
panel <- d_mun |>
  left_join(
    adopcion |> select(edon, cohort_g, grupo_tratamiento),
    by = "edon"
  ) |>
  mutate(cohort_g = as.integer(cohort_g))

# Verificación crítica: ningún estado debe quedar sin clasificar.
n_na_cohort <- sum(is.na(panel$cohort_g))
if (n_na_cohort > 0) {
  edones_problem <- panel |>
    filter(is.na(cohort_g)) |>
    distinct(edon) |>
    pull(edon)
  stop(sprintf(
    "ERROR: %d obs sin cohort_g. edones sin clasificar: %s.\nRevisar D03_adopcion_reeleccion_codebook.md.",
    n_na_cohort, paste(edones_problem, collapse = ", ")
  ))
}

cat("✓ Merge completado. Estados por cohorte:\n")
panel |>
  distinct(edon, cohort_g) |>
  count(cohort_g, name = "n_estados") |>
  print()
cat("\n")


# ── PASO 6: Variables analíticas ──────────────────────────────────────────────

cat("── PASO 6: Construyendo variables analíticas ────────────────────────────\n")

## 6a. reforma_st: dummy de tratamiento estado × año ────────────────────────────
# DECISIÓN (Ferraz y Finan 2011, AER): reforma_st se activa cuando el
# incumbente YA PUEDE postularse a reelección en su siguiente ciclo. El
# mecanismo disciplinario (accountability) requiere que la reelección sea
# factible en la próxima elección, no solo que la ley exista.
# → reforma_st = 1 si cohort_g ≠ 0 (tratado alguna vez) Y year ≥ cohort_g.
panel <- panel |>
  mutate(
    reforma_st = as.integer(cohort_g != 0L & year >= cohort_g),
    post_2018  = as.integer(year >= 2018L)
  )

## 6b. grupo_dindisc: clasificación para el DinDisc ────────────────────────────
# Tres grupos necesarios para el diseño Difference-in-Discontinuities
# (Grembi, Nannicini y Troiano 2016, AEJ Applied):
#   • never_treated  (cohort_g = 0):  Hidalgo y Veracruz — grupo de control puro.
#   • treated_pre    (cohort_g > 0 & reforma_st = 0): tratados pero antes de la
#                    reforma — grupo contrafactual de tendencia.
#   • treated_post   (cohort_g > 0 & reforma_st = 1): tratados después de la
#                    reforma — grupo de tratamiento efectivo.
panel <- panel |>
  mutate(
    grupo_dindisc = case_when(
      cohort_g == 0L                   ~ "never_treated",
      cohort_g > 0L & reforma_st == 1L ~ "treated_post",
      cohort_g > 0L & reforma_st == 0L ~ "treated_pre",
      TRUE                             ~ NA_character_
    )
  )

cat("✓ reforma_st y grupo_dindisc creados\n")

## 6c. Corrección de escala en share_incumbent_party_valid_vote ────────────────
# ANOMALÍA DOCUMENTADA: en parte del raw, share_incumbent está en escala 0-100
# (porcentual) en lugar de 0-1 (proporción). También puede ser Inf si valid=0.
# ORDEN: corregir ANTES de construir rezagos para no heredar la contaminación.
panel <- panel |>
  mutate(
    share_incumbent_party_valid_vote = case_when(
      is.infinite(share_incumbent_party_valid_vote) ~ NA_real_,
      share_incumbent_party_valid_vote > 1 & share_incumbent_party_valid_vote <= 100 ~
        share_incumbent_party_valid_vote / 100,
      share_incumbent_party_valid_vote > 1 ~ NA_real_,
      TRUE ~ share_incumbent_party_valid_vote
    )
  )

## 6d. Conteos corruptos del incumbente en edon=1/2010 (Aguascalientes) y ──────
##     edon=22/2003 (Querétaro): reconstrucción sobre VÁLIDOS e imputación NA ──
# DIAGNÓSTICO (verificado contra data/raw/all_states_almost.csv):
#   En estas dos combinaciones (estado, año) el share de sección de la fuente
#   alcanza valores imposibles (hasta ~5.4). Al reconstruir el share municipal
#   con el DENOMINADOR CORRECTO —votos válidos, conforme a la definición del
#   codebook: share_incumbent_party_valid_vote = "as a proportion of valid
#   votes"— el cociente inc/valid SIGUE excediendo 1 en la mayoría de los
#   municipios. Ello prueba que el numerador (incumbent_party_vote) está
#   inflado en la fuente (probable doble conteo de coaliciones), NO que el
#   denominador deba ser 'total'. El parche previo (inc/total) devolvía ~0.5
#   sólo porque un denominador mayor enmascaraba el numerador corrupto, e
#   introducía heterogeneidad de denominador respecto al resto del panel
#   (que es valid-based). Además, el PASO 6c reescala esos valores >1 como si
#   fueran escala 0-100, mutilándolos a ~0.01.
# DECISIÓN CONSERVADORA: reconstruir inc/valid desde CONTEOS (misma metodología
#   del PASO 2: sumar conteos y dividir, nunca promediar shares de sección);
#   conservar los municipios cuyo cociente resulta ≤ 1 (recuperables) e imputar
#   NA a los que persisten > 1 (irrecuperables con las columnas disponibles).
#   No se fuerza ningún valor con denominador inconsistente. La pérdida es
#   mínima (dos estado-años pre-tratamiento) y protege la homogeneidad de la
#   variable y la validez interna del RDD.
share_reconstruido <- d_raw |>
  filter(
    (as.integer(state_code) == 1L  & year == 2010L) |
    (as.integer(state_code) == 22L & year == 2003L)
  ) |>
  mutate(
    edon   = as.integer(state_code),
    mun_id = as.integer(mun_code)
  ) |>
  group_by(edon, mun_id, year) |>
  summarise(
    inc_sum   = sum(incumbent_party_vote, na.rm = TRUE),
    valid_sum = sum(valid,                na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    # Denominador = válidos (codebook). Si aun así el ratio > 1 o no es finito,
    # el conteo de origen es corrupto e irrecuperable → NA.
    ratio     = inc_sum / valid_sum,
    share_fix = if_else(is.finite(ratio) & ratio <= 1, ratio, NA_real_),
    celda_objetivo = TRUE
  ) |>
  select(edon, mun_id, year, share_fix, celda_objetivo)

panel <- panel |>
  left_join(share_reconstruido, by = c("edon", "mun_id", "year")) |>
  mutate(
    # OVERWRITE (no coalesce) en las celdas objetivo: hay que REEMPLAZAR el
    # valor que el PASO 6c mutiló a ~0.01; coalesce conservaría ese espurio.
    # Para celdas objetivo, el valor pasa a ser share_fix (que puede ser NA);
    # el resto del panel queda intacto.
    share_incumbent_party_valid_vote = if_else(
      coalesce(celda_objetivo, FALSE),
      share_fix,
      share_incumbent_party_valid_vote
    )
  ) |>
  select(-share_fix, -celda_objetivo)

stopifnot(
  "Aún hay Inf en share_incumbent" =
    sum(is.infinite(panel$share_incumbent_party_valid_vote), na.rm = TRUE) == 0,
  "Aún hay valores > 1 en share_incumbent" =
    sum(panel$share_incumbent_party_valid_vote > 1, na.rm = TRUE) == 0
)
cat("✓ share_incumbent_party_valid_vote: rango ∈ [0, 1], sin Inf\n")

## 6e. Rezagos y leads con control de brecha temporal ─────────────────────────
# gap_ciclos ≤ 5 preserva rezagos válidos y descarta huecos de cobertura
# (diferencias > 5 años indican municipios sin elecciones reportadas en ese
# periodo, no un periodo de mandato genuino — invalidaría el lag).
panel <- panel |>
  arrange(edon, mun_id, year) |>
  group_by(edon, mun_id) |>
  mutate(
    year_lag   = lag(year),
    gap_ciclos = year - year_lag,
    lag_valido = !is.na(gap_ciclos) & gap_ciclos <= 5L,

    share_incumbent_lag = if_else(
      lag_valido,
      lag(share_incumbent_party_valid_vote),
      NA_real_
    ),

    year_lead      = lead(year),
    gap_ciclos_fwd = year_lead - year,
    lead_valido    = !is.na(gap_ciclos_fwd) & gap_ciclos_fwd <= 5L,

    share_incumbent_lead = if_else(
      lead_valido,
      lead(share_incumbent_party_valid_vote),
      NA_real_
    ),

    share_runnerup_lead = if_else(
      lead_valido,
      lead(share_runnerup_party_valid_vote),
      NA_real_
    )
  ) |>
  ungroup()

cat("✓ Rezagos y leads construidos (gap_ciclos ≤ 5)\n")

## 6f. Outcomes binarios (mayoría relativa, no 0.5) ────────────────────────────
# México usa mayoría relativa (plurality). Usar > 0.5 clasificaría erróneamente
# como derrota a victorias legítimas por pluralidad simple.
panel <- panel |>
  mutate(
    incumbent_won = as.integer(
      share_incumbent_party_valid_vote > share_runnerup_party_valid_vote
    ),
    incumbent_won_lead = as.integer(
      share_incumbent_lead > share_runnerup_lead
    ),
    signed_margin_lead = share_incumbent_lead - share_runnerup_lead
  )

## 6g. Tipos canónicos para att_gt() y feols() ──────────────────────────────────
# att_gt() requiere idname=character, tname=integer, gname=integer.
# edon como integer para clustervars= y cluster=~edon.
panel <- panel |>
  mutate(
    mun_id     = as.character(mun_id),
    year       = as.integer(year),
    edon       = as.integer(edon),
    cohort_g   = as.integer(cohort_g),
    reforma_st = as.integer(reforma_st)
  )

cat("✓ Tipos canónicos: mun_id=character, edon/year/cohort_g/reforma_st=integer\n\n")


# ── PASO 7: Verificación final del panel municipal ────────────────────────────

cat("── PASO 7: Verificación final ──────────────────────────────────────────\n")
cat("\n─── Flujo de atrición ───\n")
print(attricion, n = Inf, width = 100)

cat("\n─── Estructura del panel municipal ───\n")
cat(sprintf("  N obs:           %s\n", format(nrow(panel), big.mark = ",")))
cat(sprintf("  N municipios:    %s\n", format(n_distinct(panel$mun_id), big.mark = ",")))
cat(sprintf("  N estados:       %d\n",  n_distinct(panel$edon)))
cat(sprintf("  Rango años:      %d–%d\n", min(panel$year), max(panel$year)))

cat("\n─── Distribución grupo_dindisc × cohort_g ───\n")
panel |>
  count(grupo_dindisc, cohort_g) |>
  arrange(cohort_g) |>
  print()

stopifnot(
  "NA en mun_winning_margin" =
    sum(is.na(panel$mun_winning_margin)) == 0L,
  "Turnout fuera de [0,1]" =
    max(panel$turnout, na.rm = TRUE) <= 1
)
cat("\n✓ stopifnot: running variable sin NA; turnout ∈ [0, 1]\n\n")


# ── PASO 8: Guardar panel_municipal_clean ─────────────────────────────────────

cat("── PASO 8: Guardando panel municipal ───────────────────────────────────\n")

saveRDS(panel, file("data/clean/panel_municipal_clean.rds", "wb"))
write_csv(panel, file("data/clean/panel_municipal_clean.csv", "wb"))

cat("✓ data/clean/panel_municipal_clean.rds\n")
cat("✓ data/clean/panel_municipal_clean.csv\n\n")


# ── PASO 9: Construir panel apilado para RDD ──────────────────────────────────

cat("── PASO 9: Construyendo panel apilado ganador + runner-up ──────────────\n")

# FUNDAMENTO (Lee 2008; Lucardi y Rosas 2016, Tabla A5 nota):
#   Para cada partido, la muestra se restringe a elecciones donde ese partido
#   terminó en primer o segundo lugar. Se genera UNA observación por papel
#   (winner / runner_up) y por elección. El cutoff c=0 en margin_signed
#   separa entonces ganadores (margin_signed > 0) de perdedores (< 0),
#   permitiendo rdrobust() con c=0 que estime el efecto de la incumbencia.
#
# ALCANCE IMPORTANTE: este panel es EXCLUSIVAMENTE para rdrobust() y rdplot().
#   No mezclarlo con el panel municipal en estimaciones DiD o TWFE.
#
# ⚠️ PRUEBA DE MANIPULACIÓN / DENSIDAD (McCrary; Cattaneo, Jansson y Ma 2020):
#   NUNCA correr rddensity() sobre margin_signed AGREGADO de este panel. Por
#   construcción, cada elección aporta un punto en +m (ganador) y otro en −m
#   (segundo lugar) con idéntico |m|: la densidad de margin_signed es SIMÉTRICA
#   POR CONSTRUCCIÓN, de modo que el test pasaría de forma mecánica y vacua —no
#   prueba ausencia de sorting.
#   El test legítimo se corre POR PARTIDO, filtrando party_focal == {PRI, PAN,
#   PRD} (con la misma regex de coaliciones de la estimación): para un partido
#   dado, los márgenes +m (cuando fue incumbente/ganador) y −m (cuando fue
#   segundo lugar) provienen de ELECCIONES DISTINTAS, por lo que su densidad
#   alrededor de c=0 sí es informativa. Por ello se añade party_focal abajo.

vars_rdd <- c(
  "mun_id", "edon", "year",
  "cohort_g", "grupo_tratamiento", "grupo_dindisc", "reforma_st",
  "mun_winning_margin",
  "incumbent_party", "runnerup_party",
  "turnout", "registered_voters",
  "share_PRI_valid_vote", "share_PAN_valid_vote",
  "share_PRD_valid_vote", "share_MORENA_valid_vote",
  "share_incumbent_lag"
)
# Conservar solo las que existen en el panel (robustez ante futuros cambios)
vars_rdd <- vars_rdd[vars_rdd %in% names(panel)]

## Perspectiva del GANADOR (margin_signed > 0) ─────────────────────────────────
panel_winner <- panel |>
  select(all_of(vars_rdd),
         share_incumbent_party_valid_vote,
         share_runnerup_party_valid_vote) |>
  mutate(
    party_role    = "winner",
    margin_signed = mun_winning_margin,     # siempre positivo
    outcome_rdd   = share_incumbent_party_valid_vote,
    # party_focal: partido al que se refieren outcome_rdd y margin_signed en
    # esta fila. En el lado ganador, es el incumbente (= ganador previo).
    # Clave para la prueba de densidad POR PARTIDO (ver advertencia arriba).
    party_focal   = incumbent_party
  ) |>
  select(-share_incumbent_party_valid_vote, -share_runnerup_party_valid_vote)

## Perspectiva del RUNNER-UP (margin_signed < 0) ───────────────────────────────
panel_loser <- panel |>
  select(all_of(vars_rdd),
         share_incumbent_party_valid_vote,
         share_runnerup_party_valid_vote) |>
  filter(!is.na(share_runnerup_party_valid_vote)) |>
  mutate(
    party_role    = "runner_up",
    margin_signed = -mun_winning_margin,    # negativo → perdedor
    outcome_rdd   = share_runnerup_party_valid_vote,
    # En el lado perdedor, party_focal es el segundo lugar. Para un partido P,
    # las filas con party_focal==P y margin<0 (P fue 2do) provienen de
    # elecciones distintas a aquéllas con party_focal==P y margin>0 (P ganó),
    # por lo que su densidad conjunta alrededor de c=0 sí es testeable.
    party_focal   = runnerup_party
  ) |>
  select(-share_incumbent_party_valid_vote, -share_runnerup_party_valid_vote)

## Apilar, limpiar y verificar ─────────────────────────────────────────────────
panel_rdd <- bind_rows(panel_winner, panel_loser) |>
  arrange(edon, mun_id, year, party_role) |>
  mutate(
    outcome_rdd = if_else(
      is.infinite(outcome_rdd) | outcome_rdd > 1,
      NA_real_,
      outcome_rdd
    )
  )

stopifnot(
  "margin_signed no tiene valores negativos — el apilado falló" =
    min(panel_rdd$margin_signed, na.rm = TRUE) < 0,
  "Ratio apilado fuera de [1, 2]" =
    between(nrow(panel_rdd) / nrow(panel), 1.0, 2.0),
  "Duplicados (party_role, mun_id, year)" =
    panel_rdd |> count(party_role, mun_id, year) |> filter(n > 1) |> nrow() == 0
)

cat(sprintf(
  "✓ Panel apilado: %s obs (%.2fx el panel municipal)\n",
  format(nrow(panel_rdd), big.mark = ","),
  nrow(panel_rdd) / nrow(panel)
))
cat(sprintf(
  "✓ Rango margin_signed: [%.4f, %.4f]\n",
  min(panel_rdd$margin_signed), max(panel_rdd$margin_signed)
))
cat(sprintf(
  "✓ NA en outcome_rdd: %.1f%%\n\n",
  100 * mean(is.na(panel_rdd$outcome_rdd))
))

saveRDS(panel_rdd, file("data/clean/panel_rdd_stacked.rds", "wb"))
cat("✓ data/clean/panel_rdd_stacked.rds\n\n")


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════\n")
cat(" 01_limpieza.R completado exitosamente.\n")
cat("  data/clean/panel_municipal_clean.rds  →  balance/descriptivos\n")
cat("  data/clean/panel_municipal_clean.csv  →  auditoría manual\n")
cat("  data/clean/panel_rdd_stacked.rds      →  RDD (rdrobust, rdplot)\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
