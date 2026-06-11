# ══════════════════════════════════════════════════════════════════════════════
# Script: 00_master.R
# Proyecto: El municipio y la reelección
# Autores:  Emiliano Miranda González
# ITAM — Primavera 2026
# Fecha:    2026-05-25
# ══════════════════════════════════════════════════════════════════════════════
#
# PROPÓSITO:
#   Ejecutar el pipeline completo de replicación en un solo jalón.
#   Orden: limpieza → estimación agregada → estimación por partido → continuidad
#   Al final imprime un resumen de todos los outputs generados.
#
# INPUTS:
#   - data/raw/all_states_almost.csv         (base casilla-nivel, 1994-2024)
#   - data/raw/fechas_adopcion_reeleccion.csv (adopción escalonada por estado)
#
# OUTPUTS (generados por los sub-scripts):
#   data/clean/
#     panel_municipal_clean.rds
#     panel_municipal_clean.csv
#     panel_rdd_stacked.rds
#   output/results/
#     dindisc_todas_rdrobust.rds
#     dindisc_todas_grembi.rds
#     dindisc_partido.rds
#     mccrary_resultados.rds
#     balance_covariables.rds
#   output/tables/
#     tabla_dindisc_todas.tex
#     tabla_dindisc_partido_PRI.tex
#     tabla_dindisc_partido_PAN.tex
#     tabla_dindisc_partido_PRD.tex
#     tabla_balance_covariables.tex
#   output/figures/
#     rdplot_todas_never_treated.pdf
#     rdplot_todas_treated_pre.pdf
#     rdplot_todas_treated_post.pdf
#     rdplot_partido_PRI_*.pdf
#     rdplot_partido_PAN_*.pdf
#     rdplot_partido_PRD_*.pdf
#     mccrary_plot_full.pdf
#     balance_placebo_coef.pdf
#   output/session_info.txt
#
# DEPENDENCIAS:
#   Ver scripts individuales. Paquetes principales:
#   tidyverse, fixest, rdrobust, rddensity, fwildclusterboot,
#   modelsummary, ggplot2, dqrng
#
# NOTAS:
#   - fwildclusterboot NO está en CRAN (archivado mayo 2024).
#     Instalar desde R-universe antes de correr:
#     install.packages("fwildclusterboot",
#                      repos = c("https://s3alfisc.r-universe.dev",
#                                "https://cloud.r-project.org"))
#   - El pipeline tarda ~10-20 minutos según specs de la máquina
#     (el cuello de botella es el wild bootstrap B=9999 en 02 y 03).
#   - Ejecutar siempre desde la RAÍZ del proyecto (eco_pol_II_proyecto/).
#     En RStudio: Session → Set Working Directory → To Project Directory
# ══════════════════════════════════════════════════════════════════════════════

# ── Entorno limpio ─────────────────────────────────────────────────────────────
# Empezar con entorno vacío para garantizar reproducibilidad.
# Si hay objetos de sesiones anteriores pueden generar efectos de memoria.
rm(list = ls()); gc()

# ── Opciones globales ──────────────────────────────────────────────────────────
options(
  scipen    = 999,      # Evitar notación científica en outputs de consola
  digits    = 4,        # Decimales en consola
  warn      = 0         # Advertencias normales (no suprimir; no convertir en errores)
)

# ── Timestamp inicio ───────────────────────────────────────────────────────────
t_inicio <- proc.time()
cat("\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("  PIPELINE REPLICACIÓN: Reelección Municipal y Ventaja del Incumbente\n")
cat("  ITAM — Emiliano Miranda González\n")
cat("  Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("══════════════════════════════════════════════════════════════════════════\n\n")

# ── Verificar directorio de trabajo ───────────────────────────────────────────
# El pipeline usa rutas relativas; asegurarse de estar en la raíz del proyecto.
wd_actual <- getwd()
if (!file.exists(file.path(wd_actual, "data", "raw", "all_states_almost.csv"))) {
  stop(
    "\n[ERROR] Directorio de trabajo incorrecto.\n",
    "  Actual: ", wd_actual, "\n",
    "  Esperado: la raíz del proyecto (eco_pol_II_proyecto/).\n",
    "  En RStudio: Session → Set Working Directory → To Project Directory\n"
  )
}
cat("✓ Directorio de trabajo verificado:", wd_actual, "\n\n")

# ── Verificar que inputs existen ──────────────────────────────────────────────
inputs_requeridos <- c(
  "data/raw/all_states_almost.csv",
  "data/raw/fechas_adopcion_reeleccion.csv"
)

cat("── Verificando inputs ─────────────────────────────────────────────────────\n")
inputs_ok <- TRUE
for (inp in inputs_requeridos) {
  if (file.exists(inp)) {
    cat("  ✓", inp, "\n")
  } else {
    cat("  ✗ FALTA:", inp, "\n")
    inputs_ok <- FALSE
  }
}
if (!inputs_ok) {
  stop("\n[ERROR] Faltan archivos de input. Verificar rutas y ejecutar de nuevo.\n")
}
cat("\n")

# ── Crear directorios de output si no existen ─────────────────────────────────
# Los sub-scripts asumen que estos directorios existen; crearlos aquí evita
# errores en caso de que sea la primera ejecución en una máquina nueva.
dirs_output <- c(
  "data/clean",
  "output/results",
  "output/tables",
  "output/figures"
)

cat("── Preparando directorios de output ───────────────────────────────────────\n")
for (d in dirs_output) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  Creado:", d, "\n")
  } else {
    cat("  Ya existe:", d, "\n")
  }
}
cat("\n")

# ── Pipeline: scripts en orden ────────────────────────────────────────────────
# Cada script es autocontenido y guarda sus propios outputs.
# El orden importa: 02, 03 y 04 dependen del output de 01.
scripts_pipeline <- c(
  "scripts/replicacion/01_limpieza.R",       # raw → panel limpio + stacked
  "scripts/replicacion/02_dindisc_todas.R",  # DinDisc agregado (todas elecciones)
  "scripts/replicacion/03_dindisc_partido.R",# DinDisc por partido (PRI, PAN, PRD)
  "scripts/replicacion/04_continuidad.R"     # McCrary + balance de covariables
)

# Registro de éxito/fallo por script
log_pipeline <- data.frame(
  script    = scripts_pipeline,
  status    = NA_character_,
  tiempo_s  = NA_real_,
  stringsAsFactors = FALSE
)

cat("══════════════════════════════════════════════════════════════════════════\n")
cat("  EJECUTANDO PIPELINE\n")
cat("══════════════════════════════════════════════════════════════════════════\n\n")

for (i in seq_along(scripts_pipeline)) {

  s <- scripts_pipeline[i]

  cat(sprintf("─── [%d/%d] %s ───\n", i, length(scripts_pipeline), s))
  cat("    Inicio:", format(Sys.time(), "%H:%M:%S"), "\n")

  t0 <- proc.time()

  tryCatch(
    expr = {
      # echo = FALSE: no imprimir cada línea del script; los scripts internamente
      # tienen sus propios cat() y mensajes; el ruido de echo aquí sería excesivo.
      source(s, echo = FALSE, encoding = "UTF-8")

      elapsed <- (proc.time() - t0)["elapsed"]
      log_pipeline$status[i]   <- "OK"
      log_pipeline$tiempo_s[i] <- round(elapsed, 1)

      cat(sprintf("    ✓ OK — %.1f segundos\n\n", elapsed))
    },
    error = function(e) {
      elapsed <- (proc.time() - t0)["elapsed"]
      log_pipeline$status[i]   <<- "ERROR"
      log_pipeline$tiempo_s[i] <<- round(elapsed, 1)

      cat("    ✗ ERROR en", s, "\n")
      cat("      Mensaje:", conditionMessage(e), "\n")
      cat("      Traceback:\n")
      traceback()
      cat("\n")

      # Detener el pipeline: si 01_limpieza falla, no tiene sentido continuar.
      # Si falla 02/03/04, los outputs parciales quedan guardados.
      stop(
        sprintf("[PIPELINE INTERRUMPIDO] Error en script %d/%d: %s\n%s",
                i, length(scripts_pipeline), s, conditionMessage(e))
      )
    }
  )
}

# ── Resumen final ─────────────────────────────────────────────────────────────
t_total <- (proc.time() - t_inicio)["elapsed"]

cat("══════════════════════════════════════════════════════════════════════════\n")
cat("  RESUMEN DEL PIPELINE\n")
cat("══════════════════════════════════════════════════════════════════════════\n\n")

cat("Scripts ejecutados:\n")
for (i in seq_len(nrow(log_pipeline))) {
  icono <- if (log_pipeline$status[i] == "OK") "✓" else "✗"
  cat(sprintf("  %s %-55s %6.1f s\n",
              icono,
              basename(log_pipeline$script[i]),
              log_pipeline$tiempo_s[i]))
}

cat(sprintf("\n  Tiempo total: %.1f segundos (%.1f minutos)\n",
            t_total, t_total / 60))
cat("\n")

# ── Inventario de outputs generados ───────────────────────────────────────────
cat("Outputs generados:\n")

outputs_esperados <- list(
  "data/clean" = c(
    "panel_municipal_clean.rds",
    "panel_municipal_clean.csv",
    "panel_rdd_stacked.rds"
  ),
  "output/results" = c(
    "dindisc_todas_rdrobust.rds",
    "dindisc_todas_grembi.rds",
    "dindisc_partido.rds",
    "mccrary_resultados.rds",
    "balance_covariables.rds"
  ),
  "output/tables" = c(
    "tabla_dindisc_todas.tex",
    "tabla_dindisc_partido_PRI.tex",
    "tabla_dindisc_partido_PAN.tex",
    "tabla_dindisc_partido_PRD.tex",
    "tabla_balance_covariables.tex"
  )
)

for (directorio in names(outputs_esperados)) {
  cat(sprintf("\n  %s/\n", directorio))
  for (archivo in outputs_esperados[[directorio]]) {
    ruta <- file.path(directorio, archivo)
    if (file.exists(ruta)) {
      size_kb <- round(file.size(ruta) / 1024, 1)
      cat(sprintf("    ✓ %-50s (%s KB)\n", archivo, size_kb))
    } else {
      cat(sprintf("    ✗ %-50s (no generado)\n", archivo))
    }
  }
}

# Figuras (listar todas las que existan en output/figures/)
cat("\n  output/figures/\n")
figs <- list.files("output/figures", pattern = "\\.pdf$", full.names = FALSE)
if (length(figs) == 0) {
  cat("    (ninguna figura .pdf encontrada)\n")
} else {
  for (f in sort(figs)) {
    size_kb <- round(file.size(file.path("output/figures", f)) / 1024, 1)
    cat(sprintf("    ✓ %-50s (%s KB)\n", f, size_kb))
  }
}

# ── sessionInfo ───────────────────────────────────────────────────────────────
# Guardar la información de sesión para garantizar reproducibilidad.
# Cualquier replicador puede verificar exactamente qué versiones se usaron.
ruta_session <- "output/session_info.txt"
writeLines(
  c(
    paste("Pipeline ejecutado:", format(Sys.time())),
    paste("Tiempo total: ", round(t_total / 60, 2), "minutos"),
    "",
    capture.output(sessionInfo())
  ),
  ruta_session
)
cat(sprintf("\n  ✓ %-50s (session info)\n", ruta_session))

# ── Cierre ────────────────────────────────────────────────────────────────────
cat("\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("  FIN DEL PIPELINE —", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("══════════════════════════════════════════════════════════════════════════\n\n")

# El objeto log_pipeline queda disponible en el entorno global por si el usuario
# quiere inspeccionarlo con View(log_pipeline) o write_csv(log_pipeline, ...)
invisible(log_pipeline)
