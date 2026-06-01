# ══════════════════════════════════════════════════════════════════════════════
# Script:   04_continuidad.R
# Proyecto: El municipio y la reelección
# Autor:    Emiliano Miranda González — ITAM
# Fecha:    2026-05-28
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Verificar los supuestos de identificación del componente RDD del diseño de
#   doble diferencial sobre cuatro discontinuidades, con dos familias de pruebas:
#
#   (A) Densidad / manipulación (McCrary; Cattaneo, Jansson y Ma 2020) —
#       VERSIÓN LEGÍTIMA, POR PARTIDO.
#       ⚠ Correr rddensity() sobre el margin_signed AGREGADO del panel apilado es
#       VACUO: por construcción, cada elección aporta un punto en +m (ganador) y
#       otro en −m (perdedor) con idéntico |m|, de modo que la densidad es
#       SIMÉTRICA por construcción y el test pasa mecánicamente. El test legítimo
#       se corre POR PARTIDO sobre el margen FIRMADO del partido focal (regla de
#       partido mayor único, igual que 03): para un partido P, las observaciones
#       con +m (P ganó en t-1) y −m (P fue segundo) provienen de ELECCIONES
#       DISTINTAS, por lo que su densidad alrededor de c=0 sí es informativa.
#
#   (B) Balance de covariables PREDETERMINADAS al cutoff (Lucardi y Rosas 2016,
#       Tabla A5; Marshall 2024, AJPS).
#       ⚠ CORRECCIÓN: turnout NO es predeterminada — es la participación de la
#       elección CORRIENTE (contemporánea al outcome), un potential outcome, no
#       una covariable de base. Usar covariables genuinamente predeterminadas:
#         • share_incumbent_lag — voto del incumbente en t-2 (anterior al margen).
#         • log_pobtot_2010, tasa_pobreza_2010, graproes_2010, pct_hli_2010 —
#           estructurales de 2010 (atemporales, predeterminadas).
#       El balance se corre POR PARTIDO sobre el margen firmado del partido focal
#       (mismo running variable que identifica el efecto), tratando cada
#       covariable como outcome ficticio: el coeficiente esperado bajo
#       continuidad es cero.
#
# INPUTS:  data/clean/panel_rdd_stacked.rds
#          data/clean/covariates_municipales.csv  (estructurales INEGI/CONEVAL)
# OUTPUTS:
#   output/results/mccrary_resultados.rds
#   output/results/balance_covariables.rds
#   output/tables/tabla_balance_covariables.tex
#   output/figures/mccrary_densidad_partidos.pdf
#   output/figures/balance_coef.pdf
# ══════════════════════════════════════════════════════════════════════════════


# ── PASO 0: Configuración ────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(conflicted)
  conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, .quiet = TRUE)
  library(tidyverse)
  library(rdrobust)
  library(rddensity)
})
options(scipen = 999, digits = 4)
dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("\n══════════════════════════════════════════════════════════════════════════\n")
cat(" 04_continuidad.R — Densidad legítima (por partido) + balance predeterminado\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ── PASO 1: Panel, celdas y coalición focal (regla de partido mayor único) ───

cat("── PASO 1: Panel + celdas + partido mayor único ─────────────────────────\n")

panel_rdd <- readRDS("data/clean/panel_rdd_stacked.rds")
panel_rdd <- panel_rdd |> filter(runnerup_party != "0" | is.na(runnerup_party))

majors       <- c("PRI", "PAN", "PRD", "MORENA")
regex_major  <- function(p) paste0("^", p, "$|^", p, "_|_", p, "$|_", p, "_")
regex_majors <- set_names(map_chr(majors, regex_major), majors)

panel_rdd <- panel_rdd |>
  mutate(
    TR   = as.integer(cohort_g > 0L),
    POST = if_else(cohort_g > 0L, as.integer(reforma_st == 1L),
                   as.integer(year >= 2018L)),
    D    = as.integer(margin_signed >= 0),
    celda = case_when(
      TR == 1L & POST == 1L ~ "treated_post",
      TR == 1L & POST == 0L ~ "treated_pre",
      TR == 0L & POST == 1L ~ "never_post",
      TR == 0L & POST == 0L ~ "never_pre"),
    coal_focal = if_else(party_role == "winner", incumbent_party, runnerup_party),
    n_major    = rowSums(sapply(regex_majors, function(rx) str_detect(coalesce(coal_focal, ""), rx)))
  )

# Muestra del partido P bajo la regla de mayor único (idéntica a 03).
party_sample <- function(panel, p) {
  panel |>
    filter(str_detect(coalesce(coal_focal, ""), regex_majors[[p]]), n_major == 1L)
}

cat(sprintf("✓ Panel: %s obs, %d estados\n\n",
            format(nrow(panel_rdd), big.mark = ","), n_distinct(panel_rdd$edon)))


# ── PASO 2: Covariables predeterminadas ──────────────────────────────────────

cat("── PASO 2: Covariables predeterminadas (NO turnout) ─────────────────────\n")

ruta_covs <- "data/clean/covariates_municipales.csv"
if (file.exists(ruta_covs)) {
  covs <- read_csv(file(ruta_covs, "rb"), show_col_types = FALSE) |>
    mutate(mun_id = as.character(mun_id),
           log_pobtot_2010 = log(pobtot_2010)) |>
    select(any_of(c("mun_id", "log_pobtot_2010", "tasa_pobreza_2010",
                    "graproes_2010", "pct_hli_2010")))
  panel_rdd <- panel_rdd |> left_join(covs, by = "mun_id")
  cat(sprintf("✓ Covariables estructurales 2010 fusionadas: %s\n",
              paste(setdiff(names(covs), "mun_id"), collapse = ", ")))
} else {
  cat("  ⚠ covariates_municipales.csv no encontrado.\n")
}

# Lista de covariables PREDETERMINADAS presentes. turnout EXCLUIDO a propósito:
# es contemporáneo al outcome (no predeterminado) y no constituye un continuity
# test válido (Marshall 2024).
covariables <- intersect(
  c("share_incumbent_lag", "log_pobtot_2010", "tasa_pobreza_2010",
    "graproes_2010", "pct_hli_2010"),
  names(panel_rdd))
cat(sprintf("  Covariables de balance: %s\n", paste(covariables, collapse = ", ")))
cat("  (turnout excluido: contemporáneo al outcome, no predeterminado)\n\n")


# ── PASO 3: Densidad de McCrary POR PARTIDO (test legítimo) ──────────────────

cat("── PASO 3: Densidad de McCrary por partido (margen firmado del focal) ───\n")

partidos_test <- c("PRI", "PAN", "PRD")
mccrary <- list()

pdf("output/figures/mccrary_densidad_partidos.pdf", width = 14, height = 5)
par(mfrow = c(1, 3))
for (p in partidos_test) {
  d <- party_sample(panel_rdd, p) |> filter(!is.na(margin_signed))
  cat(sprintf("  %s (N=%s): ", p, format(nrow(d), big.mark = ",")))
  if (nrow(d) < 100) { cat("N insuficiente.\n"); plot(1, type="n", main=sprintf("%s (insuf.)", p)); next }
  rd <- tryCatch(rddensity(X = d$margin_signed, c = 0), error = function(e) NULL)
  mccrary[[p]] <- rd
  if (!is.null(rd)) {
    pv <- rd$test$p_jk
    cat(sprintf("T=%.3f, p=%.4f %s\n", rd$test$t_jk, pv,
                if (pv < 0.05) "⚠ RECHAZA H0" else "✓ no rechaza"))
    tryCatch(rdplotdensity(rdd = rd, X = d$margin_signed, type = "both",
                           xlabel = "Margen firmado del partido focal",
                           ylabel = "Densidad", title = sprintf("%s (p=%.3f)", p, pv)),
             error = function(e) { hist(d$margin_signed, breaks = 50, main = sprintf("%s (p=%.3f)", p, pv),
                                         xlab = "Margen", col = "grey80", border = "white"); abline(v = 0, col = "red", lwd = 2) })
  } else { cat("rddensity falló.\n"); plot(1, type="n", main=sprintf("%s (error)", p)) }
}
dev.off()
saveRDS(mccrary, "output/results/mccrary_resultados.rds")
cat("  ✓ output/figures/mccrary_densidad_partidos.pdf\n")
cat("  ✓ output/results/mccrary_resultados.rds\n\n")


# ── PASO 4: Balance de covariables predeterminadas por partido ───────────────

cat("── PASO 4: Balance (covariable predeterminada como outcome ficticio) ────\n")

# Para cada partido P y cada covariable predeterminada z, se estima rdrobust con
# z como outcome sobre el margen firmado del partido focal. El coeficiente
# esperado bajo el supuesto de continuidad es CERO. Se corre sobre toda la
# submuestra del partido (las covariables son predeterminadas/estructurales).
tabla_balance <- tibble()
for (p in partidos_test) {
  dp <- party_sample(panel_rdd, p)
  for (z in covariables) {
    d <- dp |> filter(!is.na(.data[[z]]), !is.infinite(.data[[z]]), !is.na(margin_signed))
    if (nrow(d) < 60 || sum(d$margin_signed < 0) < 20 || sum(d$margin_signed > 0) < 20) next
    rd <- tryCatch(rdrobust(y = d[[z]], x = d$margin_signed, c = 0, cluster = d$edon,
                            vce = "hc2", bwselect = "mserd", kernel = "triangular"),
                   error = function(e) NULL)
    if (is.null(rd)) next
    tabla_balance <- bind_rows(tabla_balance, tibble(
      partido = p, covariable = z,
      beta = rd$coef["Bias-Corrected", 1], se = rd$se["Robust", 1],
      # n = sum(rd$N_h): observaciones efectivas dentro del bandwidth (UN escalar).
      # ANTES: n = rd$N era un vector de longitud 2 (izq, der) que, al reciclarse
      # en tibble(), DUPLICABA cada fila (partido × covariable) — error que dejaba
      # la tabla de balance con 30 filas en vez de 15 y rompía su lectura aguas abajo.
      pval = rd$pv["Robust", 1], bw = rd$bws["h", "left"], n = sum(rd$N_h)))
    cat(sprintf("  %-4s × %-20s β=%8.4f  SE=%7.4f  p=%.3f %s\n",
                p, z, rd$coef["Bias-Corrected",1], rd$se["Robust",1], rd$pv["Robust",1],
                if (rd$pv["Robust",1] < 0.05) "⚠" else "✓"))
  }
}
cat("\n")
saveRDS(list(tabla = tabla_balance,
             metadata = list(covariables = covariables,
                             nota = "turnout excluido (contemporáneo); balance por partido sobre margen firmado focal",
                             fecha = Sys.time())),
        "output/results/balance_covariables.rds")
cat("✓ output/results/balance_covariables.rds\n\n")


# ── PASO 5: Tabla LaTeX y coefplot de balance ────────────────────────────────

cat("── PASO 5: Tabla y figura de balance ────────────────────────────────────\n")

stars <- function(p) if (is.na(p)) "" else if (p<.01) "$^{***}$" else if (p<.05) "$^{**}$" else if (p<.1) "$^{*}$" else ""
etq_cov <- c(share_incumbent_lag = "Voto incumbente ($t-2$)",
             log_pobtot_2010 = "Log población 2010", tasa_pobreza_2010 = "Tasa pobreza 2010",
             graproes_2010 = "Escolaridad 2010", pct_hli_2010 = "\\% lengua indígena 2010")

if (nrow(tabla_balance) > 0) {
  tex <- c("% Balance de covariables predeterminadas (test de continuidad por partido)",
           "% Generado por 04_continuidad.R",
           "\\begin{tabular}{llccc}", "\\toprule",
           "Covariable & & PRI & PAN & PRD \\\\", "\\midrule")
  for (z in covariables) {
    etq <- if (z %in% names(etq_cov)) etq_cov[[z]] else z
    b <- s <- character(3)
    for (i in seq_along(partidos_test)) {
      r <- tabla_balance |> filter(partido == partidos_test[i], covariable == z)
      if (nrow(r) == 1) { b[i] <- sprintf("%.4f%s", r$beta, stars(r$pval)); s[i] <- sprintf("(%.4f)", r$se) }
      else { b[i] <- "---"; s[i] <- "" }
    }
    tex <- c(tex, sprintf("\\textit{%s} & $\\hat{\\beta}$ & %s & %s & %s \\\\", etq, b[1], b[2], b[3]),
             sprintf(" & (SE) & %s & %s & %s \\\\", s[1], s[2], s[3]))
  }
  tex <- c(tex, "\\bottomrule",
           "\\multicolumn{5}{p{0.9\\textwidth}}{\\footnotesize \\textit{Nota:} cada celda es el",
           "  estimador RD bias-corrected con la covariable PREDETERMINADA como outcome ficticio,",
           "  sobre el margen firmado del partido focal (regla de partido mayor único). SE robustos",
           "  (hc2, cluster-estado), $h$ MSE-óptimo. La ausencia de discontinuidad corrobora el",
           "  supuesto de continuidad (Marshall 2024, AJPS; Lucardi y Rosas 2016). turnout se excluye",
           "  por no ser predeterminado. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
           "\\end{tabular}")
  writeLines(tex, "output/tables/tabla_balance_covariables.tex")
  cat("✓ output/tables/tabla_balance_covariables.tex\n")

  p_bal <- tabla_balance |>
    mutate(ci_lo = beta - 1.96*se, ci_hi = beta + 1.96*se,
           cov_lab = recode(covariable, !!!etq_cov)) |>
    ggplot(aes(x = beta, y = cov_lab, color = partido)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(position = position_dodge(width = .6), size = 2.4) +
    geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = .2,
                   position = position_dodge(width = .6)) +
    labs(title = "Balance de covariables predeterminadas al cutoff (margen = 0)",
         subtitle = "Estimador RD por partido; efecto esperado = 0 bajo continuidad",
         x = "Coeficiente RD bias-corrected (IC 95%)", y = NULL, color = "Partido") +
    theme_classic(base_size = 11) + theme(legend.position = "bottom")
  ggsave("output/figures/balance_coef.pdf", p_bal, width = 8, height = 5, device = cairo_pdf)
  cat("✓ output/figures/balance_coef.pdf\n\n")
} else cat("  ⚠ tabla_balance vacía.\n\n")


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════\n")
cat(" Diagnóstico de validez del RDD\n")
for (p in partidos_test) if (!is.null(mccrary[[p]]))
  cat(sprintf("  McCrary %s: p = %.4f  %s\n", p, mccrary[[p]]$test$p_jk,
              if (mccrary[[p]]$test$p_jk > 0.05) "✓" else "⚠"))
n_sig <- if (nrow(tabla_balance) > 0) sum(tabla_balance$pval < 0.10, na.rm = TRUE) else 0
cat(sprintf("  Balance: %d de %d pruebas significativas al 10%% %s\n",
            n_sig, nrow(tabla_balance), if (n_sig == 0) "✓" else "⚠ ver tabla"))
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
