# ══════════════════════════════════════════════════════════════════════════════
# Script:   03b_dindisc_morena.R
# Proyecto: El municipio y la reelección
# Autor:    Emiliano Miranda González — ITAM
# Fecha:    2026-05-29
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Exploración del partido hoy dominante, MORENA, que el análisis principal
#   (03) excluyó. Este script es AUTÓNOMO: reconstruye el panel y las celdas
#   igual que 02/03, con la regla de "partido mayor único".
#
# ── ADVERTENCIA METODOLÓGICA CENTRAL (leer antes de interpretar) ─────────────
#   MORENA se registró como partido en 2014 y ganó sus primeros ayuntamientos
#   en 2015/2018; como PARTIDO INCUMBENTE (ganador del ciclo previo, que es lo
#   que define la variable de asignación, margin t-1) aparece prácticamente solo
#   desde 2018. En consecuencia, las celdas PRE-reforma (treated_pre, never_pre)
#   están VACÍAS o casi.
#
#   Implicación: el DOBLE DIFERENCIAL sobre cuatro discontinuidades
#       τ = [β(treated,post) − β(treated,pre)] − [β(never,post) − β(never,pre)]
#   es INESTIMABLE para MORENA, porque no existe un "antes de la reelegibilidad"
#   para un partido nacido después de la reforma. No es una limitación de datos
#   subsanable, sino una imposibilidad de identificación: MORENA es, por
#   construcción, una unidad post-tratamiento sin contrafactual temporal propio.
#
#   Lo que SÍ es estimable es el NIVEL de la discontinuidad de incumbencia de
#   MORENA en el periodo post-reforma: un RDD de elección cerrada (Lee 2008;
#   Lucardi y Rosas 2016) sobre el margen firmado del partido focal. Mide si,
#   en contiendas cerradas, gobernar el municipio ayuda (+) o perjudica (−) al
#   voto de MORENA en la siguiente elección. NO es el efecto causal de la
#   reforma; es la (des)ventaja del incumbente morenista, condicionada al
#   régimen ya reformado.
#
# INPUTS:  data/clean/panel_rdd_stacked.rds   (producido por 01_limpieza.R)
# OUTPUTS:
#   output/results/dindisc_morena.rds
#   output/tables/tabla_morena.tex
#   output/figures/rdplot_morena_post.pdf
# ══════════════════════════════════════════════════════════════════════════════


# ── PASO 0: Configuración ────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(conflicted)
  conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, .quiet = TRUE)
  library(tidyverse)
  library(fixest)
  library(rdrobust)
})
if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  install.packages("fwildclusterboot",
    repos = c("https://s3alfisc.r-universe.dev", "https://cloud.r-project.org"))
}
library(fwildclusterboot)

options(scipen = 999, digits = 4)
dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("\n══════════════════════════════════════════════════════════════════════════\n")
cat(" 03b_dindisc_morena.R — Exploración del partido dominante (MORENA)\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ── PASO 1: Panel, celdas y coalición focal (idéntico a 02/03) ───────────────

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

# Panel de MORENA bajo la regla de partido mayor único (igual que 03):
#   incluir la observación si MORENA está en la coalición focal y es el ÚNICO
#   partido mayor (coaliciones MORENA+menores sí; MORENA_PRD, MORENA_PAN no).
dat_mor <- panel_rdd |>
  mutate(outcome_party = share_MORENA_valid_vote) |>
  filter(str_detect(coalesce(coal_focal, ""), regex_majors[["MORENA"]]),
         n_major == 1L,
         !is.na(outcome_party), !is.infinite(outcome_party), outcome_party <= 1)

cat(sprintf("✓ Panel MORENA (mayor único): %s obs (ganador=%s, perdedor=%s)\n\n",
            format(nrow(dat_mor), big.mark = ","),
            format(sum(dat_mor$D == 1L), big.mark = ","),
            format(sum(dat_mor$D == 0L), big.mark = ",")))


# ── PASO 2: Diagnóstico de viabilidad del DinDisc ────────────────────────────

cat("── PASO 2: ¿Hay observaciones en las cuatro celdas? ─────────────────────\n")

conteo_celdas <- dat_mor |>
  count(celda, lado = if_else(D == 1L, "ganador", "perdedor")) |>
  pivot_wider(names_from = lado, values_from = n, values_fill = 0) |>
  mutate(total = rowSums(across(where(is.numeric))))
print(conteo_celdas)

# Mínimo de observaciones por celda y por lado para que el RDD de esa celda sea
# siquiera estimable (mismo umbral conservador que 02/03).
n_celda <- dat_mor |> count(celda) |> deframe()
get_n <- function(x) if (x %in% names(n_celda)) n_celda[[x]] else 0L
pre_factible <- get_n("treated_pre") >= 50 && get_n("never_pre") >= 50

cat(sprintf(
  "\n  treated_pre = %d | treated_post = %d | never_pre = %d | never_post = %d\n",
  get_n("treated_pre"), get_n("treated_post"), get_n("never_pre"), get_n("never_post")))

dindisc_factible <- pre_factible
if (!dindisc_factible) {
  cat("\n  ⚠ DOBLE DIFERENCIAL INESTIMABLE PARA MORENA.\n")
  cat("    Las celdas pre-reforma están vacías o casi: MORENA no existía como\n")
  cat("    partido incumbente antes de 2018, de modo que no hay un contrafactual\n")
  cat("    temporal ('antes de la reelegibilidad') para diferenciar. Se procede a\n")
  cat("    estimar solo lo identificable: el NIVEL de la discontinuidad post-reforma.\n\n")
} else {
  cat("\n  ✓ Hay observaciones pre-reforma; el DinDisc es estimable (caso no esperado).\n\n")
}


# ── PASO 3: Lo estimable — discontinuidad de incumbencia post-reforma ────────

cat("── PASO 3: RDD de nivel (discontinuidad de incumbencia, post-reforma) ───\n")

# Estimador de polinomio local (rdrobust): kernel triangular, h MSE-óptimo,
# SE bias-corrected y conglomerados por estado (mismas opciones que 02/03).
# Mide la (des)ventaja del incumbente morenista en contiendas cerradas; NO es
# el efecto causal de la reforma.
rd_nivel <- function(dat, etq) {
  d <- dat |> filter(!is.na(outcome_party), !is.na(margin_signed))
  if (nrow(d) < 50 || sum(d$margin_signed < 0) < 20 || sum(d$margin_signed > 0) < 20) {
    message(sprintf("  ⚠ %s: obs insuficientes (N=%d, izq=%d, der=%d).",
                    etq, nrow(d), sum(d$margin_signed < 0), sum(d$margin_signed > 0)))
    return(NULL)
  }
  rd <- tryCatch(
    rdrobust(y = d$outcome_party, x = d$margin_signed, c = 0, cluster = d$edon,
             vce = "hc2", bwselect = "mserd", kernel = "triangular"),
    error = function(e) { message(sprintf("  ✗ rdrobust %s: %s", etq, conditionMessage(e))); NULL })
  if (!is.null(rd)) {
    cat(sprintf("  %-22s β_BC=%7.4f  SE_rob=%6.4f  p=%.3f  IC95%%=[%.4f, %.4f]  h=%.4f  N=%d\n",
                etq, rd$coef["Bias-Corrected", 1], rd$se["Robust", 1], rd$pv["Robust", 1],
                rd$ci["Robust", 1], rd$ci["Robust", 2], rd$bws["h", "left"], rd$N))
  }
  rd
}

# (a) Toda la muestra MORENA mayor-único (esencialmente post-2018).
rd_full <- rd_nivel(dat_mor, "MORENA (todo, mayor único)")
# (b) Solo la celda treated_post (estados con reforma vigente): la lectura limpia.
rd_post <- rd_nivel(dat_mor |> filter(celda == "treated_post"), "MORENA (treated_post)")
# (c) Robustez: MORENA puro (sin coalición alguna).
dat_mor_puro <- panel_rdd |>
  mutate(outcome_party = share_MORENA_valid_vote) |>
  filter(coal_focal == "MORENA",
         !is.na(outcome_party), !is.infinite(outcome_party), outcome_party <= 1)
rd_puro <- rd_nivel(dat_mor_puro, "MORENA puro (sin coalición)")
cat("\n")


# ── PASO 4: Inferencia por wild cluster bootstrap (pocos clusters) ───────────

cat("── PASO 4: Wild cluster bootstrap del salto post-reforma ────────────────\n")

# Para el salto de nivel se corre una regresión local-lineal dentro del h
# MSE-óptimo y se aplica wild cluster bootstrap (Rademacher, B=9999) sobre el
# coeficiente de D (el salto), conglomerando por estado. Coherente con la
# inferencia de pocos clusters usada en todo el proyecto (MacKinnon et al. 2023).
boot_post <- NULL
if (!is.null(rd_post)) {
  h <- rd_post$bws["h", "left"]
  d <- dat_mor |>
    filter(celda == "treated_post", !is.na(outcome_party), !is.na(margin_signed),
           abs(margin_signed) <= h) |>
    mutate(m = margin_signed, w = pmax(0, 1 - abs(margin_signed) / h))
  n_cl <- n_distinct(d$edon)
  mod <- tryCatch(feols(outcome_party ~ D * m, data = d, weights = ~w,
                        cluster = ~edon, notes = FALSE), error = function(e) NULL)
  if (!is.null(mod) && "D" %in% names(coef(mod))) {
    cat(sprintf("  Local-lineal en |m|<=%.4f: β_D=%.4f, clusters=%d\n",
                h, coef(mod)["D"], n_cl))
    boot_post <- tryCatch({
      dqrng::dqset.seed(20260529)
      bt <- boottest(mod, param = "D", clustid = "edon",
                     B = 9999L, type = "rademacher", impose_null = TRUE)
      cat(sprintf("  ✓ wild bootstrap: p = %.4f   IC95%% = [%.4f, %.4f]\n",
                  bt$p_val, bt$conf_int[1], bt$conf_int[2]))
      bt
    }, error = function(e) { cat(sprintf("  ⚠ bootstrap falló: %s\n", conditionMessage(e))); NULL })
  }
}
cat("\n")


# ── PASO 5: rdplot post-reforma ──────────────────────────────────────────────

cat("── PASO 5: rdplot ───────────────────────────────────────────────────────\n")
d_plot <- dat_mor |> filter(celda == "treated_post", !is.na(outcome_party), !is.na(margin_signed))
if (nrow(d_plot) >= 50) {
  tryCatch({
    pdf("output/figures/rdplot_morena_post.pdf", width = 7, height = 4.5)
    rdplot(y = d_plot$outcome_party, x = d_plot$margin_signed, c = 0, nbins = c(20, 20),
           title = "MORENA — discontinuidad de incumbencia (post-reforma)",
           x.label = "Margen de victoria (t-1)", y.label = "Voto MORENA (fracción válida)",
           col.dots = "black", col.lines = "#A6093D")
    dev.off()
    cat("  ✓ output/figures/rdplot_morena_post.pdf\n")
  }, error = function(e) { message(sprintf("  ✗ rdplot: %s", conditionMessage(e))); try(dev.off(), silent = TRUE) })
} else cat("  ⚠ insuficientes obs para rdplot.\n")
cat("\n")


# ── PASO 6: Guardar resultados y tabla ───────────────────────────────────────

beta_of <- function(rd) if (is.null(rd)) NA_real_ else rd$coef["Bias-Corrected", 1]
se_of   <- function(rd) if (is.null(rd)) NA_real_ else rd$se["Robust", 1]
p_of    <- function(rd) if (is.null(rd)) NA_real_ else rd$pv["Robust", 1]
n_of    <- function(rd) if (is.null(rd)) NA_integer_ else rd$N

saveRDS(
  list(
    dindisc_factible = dindisc_factible,
    conteo_celdas    = conteo_celdas,
    rd_full = rd_full, rd_post = rd_post, rd_puro = rd_puro,
    boot_post = boot_post,
    metadata = list(
      nota = paste("DinDisc inestimable para MORENA (sin celdas pre-reforma);",
                   "se reporta el NIVEL de la discontinuidad de incumbencia post-reforma,",
                   "no el efecto causal de la reforma."),
      outcome = "share_MORENA_valid_vote", fecha = Sys.time())),
  "output/results/dindisc_morena.rds")
cat("✓ output/results/dindisc_morena.rds\n")

stars <- function(p) if (is.na(p)) "" else if (p<.01) "$^{***}$" else if (p<.05) "$^{**}$" else if (p<.1) "$^{*}$" else ""
p_post_disp <- if (!is.null(boot_post)) boot_post$p_val else p_of(rd_post)
tex <- c(
  "% MORENA — discontinuidad de incumbencia post-reforma (NO efecto causal de la reforma)",
  "% Generado por 03b_dindisc_morena.R",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Especificación & $\\hat{\\beta}$ (salto) & (SE) & $N$ \\\\",
  "\\midrule",
  sprintf("MORENA, mayor único (todo) & %.4f%s & (%.4f) & %s \\\\",
          beta_of(rd_full), stars(p_of(rd_full)), se_of(rd_full),
          ifelse(is.na(n_of(rd_full)), "---", format(n_of(rd_full), big.mark = ","))),
  sprintf("\\quad celda \\textit{treated\\_post} & %.4f%s & (%.4f) & %s \\\\",
          beta_of(rd_post), stars(p_post_disp), se_of(rd_post),
          ifelse(is.na(n_of(rd_post)), "---", format(n_of(rd_post), big.mark = ","))),
  sprintf("MORENA puro (sin coalición) & %.4f%s & (%.4f) & %s \\\\",
          beta_of(rd_puro), stars(p_of(rd_puro)), se_of(rd_puro),
          ifelse(is.na(n_of(rd_puro)), "---", format(n_of(rd_puro), big.mark = ","))),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.9\\textwidth}}{\\footnotesize \\textit{Nota:} discontinuidad de",
  "  incumbencia (RDD local-lineal, kernel triangular, $h$ MSE-óptimo, SE bias-corrected",
  "  conglomerados por estado). \\textbf{No} es el efecto causal de la reforma: el doble",
  "  diferencial es inestimable para MORENA por carecer de observaciones pre-reforma.",
  "  $p$ de la celda \\textit{treated\\_post} por wild cluster bootstrap Rademacher.",
  "  $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}")
writeLines(tex, "output/tables/tabla_morena.tex")
cat("✓ output/tables/tabla_morena.tex\n\n")


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════\n")
cat(" 03b completado.\n")
cat(sprintf("  DinDisc factible para MORENA: %s\n", ifelse(dindisc_factible, "SÍ", "NO (sin celdas pre)")))
if (!is.null(rd_post))
  cat(sprintf("  Discontinuidad post-reforma (treated_post): β=%.4f [SE %.4f], N=%d\n",
              beta_of(rd_post), se_of(rd_post), n_of(rd_post)))
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
