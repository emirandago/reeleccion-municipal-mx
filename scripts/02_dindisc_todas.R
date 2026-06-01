# ══════════════════════════════════════════════════════════════════════════════
# Script:   02_dindisc_todas.R
# Proyecto: El municipio y la reelección
# Autor:    Emiliano Miranda González — ITAM
# Fecha:    2026-05-28
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Estimar el efecto de la reforma de reelección municipal (2014) sobre la
#   ventaja partidista del incumbente con un diseño Difference-in-Discontinuities
#   (DinDisc) en su forma CANÓNICA: el DOBLE DIFERENCIAL sobre CUATRO
#   discontinuidades (Grembi, Nannicini y Troiano 2016, AEJ Applied; Picchetti,
#   Pinto y Shinoki 2026).
#
# ESTRATEGIA DE IDENTIFICACIÓN (la del trabajo, de aquí en adelante):
#   La maldición del incumbente es una DISCONTINUIDAD en el umbral margin = 0
#   (brinco entre el voto del ganador marginal y el del perdedor marginal en t-1).
#   El estimador NO es el contraste transversal treated_post vs never_treated
#   —que confunde el efecto de la reforma con el realineamiento de 2018 (efecto
#   AMLO/MORENA)—, sino el doble diferencial:
#
#     τ = [β(treated, post) − β(treated, pre)]   ← 1er diferencial (Grembi básico):
#                                                   barre el confusor INVARIANTE
#                                                   en el tiempo (maldición basal).
#         − [β(never, post) − β(never, pre)]      ← 2º diferencial (vs. control):
#                                                   barre el confusor VARIABLE en
#                                                   el tiempo común (choque de 2018).
#
#   donde β(g) es la discontinuidad de la ventaja del incumbente en la celda g.
#   Las CUATRO celdas surgen de cruzar TR (estado tratado) × POST (régimen
#   post-reforma):
#     • treated_pre   (TR=1, POST=0): estados que adoptarán, antes de la reforma.
#     • treated_post  (TR=1, POST=1): estados con reelección ya vigente.
#     • never_pre     (TR=0, POST=0): Hidalgo+Veracruz, antes de 2018.
#     • never_post    (TR=0, POST=1): Hidalgo+Veracruz, desde 2018.
#
# DEFINICIÓN DE POST:
#   • Estados tratados: POST = reforma_st (1 si la reelección ya rige para esa
#     elección; respeta la fecha de adopción de cada estado a nivel observación,
#     SIN modelar dinámica escalonada).
#   • Estados never-treated: POST = 1{year ≥ 2018}, el quiebre calendario de la
#     cohorte dominante y de la elección crítica de 2018. Su primer/segundo
#     diferencial capta el choque común que el doble diferencial elimina.
#
# ESTIMADOR E INFERENCIA (como lo harían Simpser o Lucardi):
#   τ se estima con UNA SOLA regresión local-lineal SATURADA dentro del ancho de
#   banda MSE-óptimo, con kernel triangular y pesos. τ es el coeficiente de la
#   triple interacción D×POST×TR. Esto da inferencia conjunta VÁLIDA: una sola
#   matriz de varianzas que captura la correlación dentro de estado entre celdas
#   pre/post (lo que el viejo SE = sqrt(se²+se²) ignoraba). SE conglomerados a
#   nivel estado + wild cluster bootstrap Rademacher B=9999 (obligatorio con ~29
#   clusters; MacKinnon, Nielsen y Webb 2023, JoE).
#
# NOTA: este trabajo NO incorpora DiD escalonado ni TWFE. El doble diferencial
#   local-lineal es el estimador único de cabecera.
#
# INPUTS:  data/clean/panel_rdd_stacked.rds   (producido por 01_limpieza.R)
# OUTPUTS:
#   output/results/dindisc_todas.rds            Objeto con τ, celdas, robustez
#   output/tables/tabla_dindisc_todas.tex       Tabla LaTeX
#   output/figures/rdplot_todas_*.pdf           rdplots por celda
#   output/figures/rdplot_dindisc_panel.pdf     Panel 2×2
# ══════════════════════════════════════════════════════════════════════════════


# ── PASO 0: Configuración ────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(conflicted)
  conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, .quiet = TRUE)
  library(tidyverse)
  library(fixest)     # feols(): regresión local-lineal saturada del DinDisc
  library(rdrobust)   # rdrobust()/rdplot(): bandwidth MSE-óptimo y discont. por celda
})

# fwildclusterboot no está en CRAN (archivado 2024-05-29).
if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  message("Instalando fwildclusterboot desde R-universe (solo primera vez)...")
  install.packages(
    "fwildclusterboot",
    repos = c("https://s3alfisc.r-universe.dev", "https://cloud.r-project.org")
  )
}
library(fwildclusterboot)

options(scipen = 999, digits = 4)

dir.create("output/results", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("\n══════════════════════════════════════════════════════════════════════════\n")
cat(" 02_dindisc_todas.R — Doble diferencial sobre cuatro discontinuidades\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ── PASO 1: Cargar panel y construir las cuatro celdas ───────────────────────

cat("── PASO 1: Panel apilado + construcción de TR, POST y celdas ────────────\n")

panel_rdd <- readRDS("data/clean/panel_rdd_stacked.rds")

stopifnot(
  "margin_signed debe tener valores negativos (runner-ups)" =
    min(panel_rdd$margin_signed, na.rm = TRUE) < 0,
  "outcome_rdd no existe en el panel" = "outcome_rdd" %in% names(panel_rdd),
  "edon no existe (variable de clustering)" = "edon" %in% names(panel_rdd),
  "cohort_g no existe" = "cohort_g" %in% names(panel_rdd),
  "reforma_st no existe" = "reforma_st" %in% names(panel_rdd)
)

# Excluir elecciones sin oposición real (runnerup_party == "0"): el margen no
# tiene contenido político interpretable cuando no hay segundo lugar.
panel_rdd <- panel_rdd |> filter(runnerup_party != "0" | is.na(runnerup_party))

# TR: estado tratado (adoptó la reforma en alguna cohorte).
# POST: para tratados, reforma_st (reforma vigente para ESA elección, respetando
#       la fecha de adopción de cada estado); para never-treated, quiebre
#       calendario en 2018 (cohorte dominante + elección crítica).
panel_rdd <- panel_rdd |>
  mutate(
    TR   = as.integer(cohort_g > 0L),
    POST = if_else(cohort_g > 0L,
                   as.integer(reforma_st == 1L),
                   as.integer(year >= 2018L)),
    D    = as.integer(margin_signed >= 0),   # 1 = el partido focal ganó en t-1
    celda = case_when(
      TR == 1L & POST == 1L ~ "treated_post",
      TR == 1L & POST == 0L ~ "treated_pre",
      TR == 0L & POST == 1L ~ "never_post",
      TR == 0L & POST == 0L ~ "never_pre",
      TRUE                  ~ NA_character_
    )
  )

stopifnot("Hay observaciones sin celda asignada" = sum(is.na(panel_rdd$celda)) == 0)

cat(sprintf("✓ Panel: %s obs, %d estados, años %d–%d\n",
            format(nrow(panel_rdd), big.mark = ","),
            n_distinct(panel_rdd$edon), min(panel_rdd$year), max(panel_rdd$year)))
cat("\n─── Obs por celda (× lado del umbral) ───\n")
panel_rdd |> count(celda, party_role = if_else(D == 1L, "ganador", "perdedor")) |>
  pivot_wider(names_from = party_role, values_from = n) |> print()
cat("\n")


# ── PASO 2: Descriptivos por celda ───────────────────────────────────────────

cat("── PASO 2: Descriptivos por celda ───────────────────────────────────────\n")
panel_rdd |>
  group_by(celda) |>
  summarise(
    N_obs     = n(),
    N_mun     = n_distinct(mun_id),
    N_estados = n_distinct(edon),
    y_mean    = mean(outcome_rdd, na.rm = TRUE),
    .groups   = "drop"
  ) |> print(width = Inf)
cat("\n")


# ── PASO 3: Discontinuidad por celda (descriptivo) + bandwidth común ─────────

cat("── PASO 3: rdrobust por celda y bandwidth MSE-óptimo común ──────────────\n")

# Discontinuidad por celda con rdrobust (kernel triangular, h MSE-óptimo, SE
# bias-corrected, clustered por estado). Sirve para describir cada β(celda) y
# para los rdplots; la INFERENCIA de τ proviene de la regresión saturada (PASO 4).
estimar_celda <- function(dat, etiqueta) {
  d <- dat |> filter(!is.na(outcome_rdd), !is.na(margin_signed))
  if (nrow(d) < 50 || sum(d$margin_signed < 0) < 20 || sum(d$margin_signed > 0) < 20) {
    message(sprintf("  ⚠ %s: obs insuficientes (N=%d). NULL.", etiqueta, nrow(d)))
    return(NULL)
  }
  rd <- tryCatch(
    rdrobust(y = d$outcome_rdd, x = d$margin_signed, c = 0,
             cluster = d$edon, vce = "hc2", bwselect = "mserd", kernel = "triangular"),
    error = function(e) { message(sprintf("  ✗ rdrobust falló (%s): %s", etiqueta, conditionMessage(e))); NULL }
  )
  if (!is.null(rd)) {
    cat(sprintf("  %-13s β_BC=%7.4f  SE_rob=%6.4f  p=%.3f  h=%.4f  N=%d\n",
                etiqueta, rd$coef["Bias-Corrected", 1], rd$se["Robust", 1],
                rd$pv["Robust", 1], rd$bws["h", "left"], rd$N))
  }
  rd
}

celdas <- c("treated_pre", "treated_post", "never_pre", "never_post")
rd_celdas <- set_names(
  map(celdas, ~ estimar_celda(panel_rdd |> filter(celda == .x), .x)),
  celdas
)

# Bandwidth común MSE-óptimo sobre el pool de las cuatro celdas. Es el ancho de
# banda de referencia para la regresión saturada; se reporta sensibilidad en P5.
rd_pool <- tryCatch(
  with(panel_rdd |> filter(!is.na(outcome_rdd), !is.na(margin_signed)),
       rdrobust(y = outcome_rdd, x = margin_signed, c = 0, vce = "hc2",
                bwselect = "mserd", kernel = "triangular")),
  error = function(e) NULL
)
h_ref <- if (!is.null(rd_pool)) rd_pool$bws["h", "left"] else 0.15
cat(sprintf("\n  Bandwidth MSE-óptimo común (pool): h = %.4f\n\n", h_ref))


# ── PASO 4: ESTIMADOR DE CABECERA — doble diferencial local-lineal saturado ──

cat("── PASO 4: Doble diferencial (regresión local-lineal saturada) ──────────\n")

# Regresión saturada dentro de |m| ≤ h con kernel triangular:
#   y = (D + m + D·m) × POST × TR
# El brinco (efecto de D) en cada celda es una combinación lineal de coeficientes:
#   never_pre   : β_D
#   never_post  : β_D + β_{D:POST}
#   treated_pre : β_D + β_{D:TR}
#   treated_post: β_D + β_{D:POST} + β_{D:TR} + β_{D:POST:TR}
# De donde:
#   1er diferencial (treated, post−pre) = β_{D:POST} + β_{D:POST:TR}
#   2º  diferencial (never,   post−pre) = β_{D:POST}
#   DOBLE DIFERENCIAL  τ                = β_{D:POST:TR}   ← coeficiente de interés
#
# Una sola regresión ⇒ inferencia conjunta válida: la matriz de varianzas
# clustered por estado captura la correlación dentro de estado entre celdas
# pre/post, que el estimador "informal" sqrt(se²+se²) ignoraba por completo.

ajustar_dd <- function(dat, h, kernel = c("triangular", "uniform")) {
  kernel <- match.arg(kernel)
  d <- dat |>
    filter(!is.na(outcome_rdd), !is.na(margin_signed), abs(margin_signed) <= h) |>
    mutate(
      m = margin_signed,
      w = if (kernel == "triangular") pmax(0, 1 - abs(m) / h) else 1
    )
  mod <- feols(outcome_rdd ~ D * m * POST * TR,
               data = d, weights = ~w, cluster = ~edon, notes = FALSE)
  list(mod = mod, dat = d)
}

fit <- ajustar_dd(panel_rdd, h_ref, "triangular")
mod_dd <- fit$mod
dat_bw <- fit$dat

cf  <- coef(mod_dd)
V   <- vcov(mod_dd, cluster = ~edon)             # VCOV clustered (robusto a versión)

# Localizar coeficientes por sus COMPONENTES (no por nombre literal): fixest puede
# ordenar la interacción como "D:POST:TR" o "D:TR:POST", etc. setequal() es robusto.
find_coef <- function(mod, parts) {
  nms <- names(coef(mod))
  hit <- nms[vapply(strsplit(nms, ":"), function(z) setequal(z, parts), logical(1))]
  if (length(hit) == 1) hit else NA_character_
}
nm_tau   <- find_coef(mod_dd, c("D", "POST", "TR"))   # τ = doble diferencial
nm_dpost <- find_coef(mod_dd, c("D", "POST"))         # 2º diferencial (control)
stopifnot("No se identificó el coeficiente D:POST:TR (¿colinealidad en alguna celda?)" =
            !is.na(nm_tau))

get <- function(nm) if (!is.na(nm) && nm %in% names(cf)) unname(cf[nm]) else NA_real_
tau        <- get(nm_tau)                         # doble diferencial
fd_treated <- get(nm_dpost) + get(nm_tau)         # 1er dif. (tratados)
fd_never   <- get(nm_dpost)                       # 2º  dif. (control)

# SE clustered de combinaciones lineales vía g'Vg (g = selector de coeficientes).
se_combo <- function(nms) {
  nms <- nms[!is.na(nms)]
  g <- as.numeric(names(cf) %in% nms)
  sqrt(drop(t(g) %*% V %*% g))
}
se_tau        <- se_combo(nm_tau)
se_fd_treated <- se_combo(c(nm_dpost, nm_tau))
se_fd_never   <- se_combo(nm_dpost)

cat(sprintf("  N (|m| ≤ h=%.4f): %s  |  clusters (estados): %d\n",
            h_ref, format(nrow(dat_bw), big.mark = ","), n_distinct(dat_bw$edon)))
cat(sprintf("  1er diferencial  (treated, post−pre): %7.4f  [SE %.4f]\n", fd_treated, se_fd_treated))
cat(sprintf("  2º  diferencial  (never,   post−pre): %7.4f  [SE %.4f]\n", fd_never,   se_fd_never))
cat(sprintf("  ★ τ DOBLE DIFERENCIAL (D:POST:TR):    %7.4f  [SE %.4f]\n", tau, se_tau))
cat(sprintf("    z = %.3f   p (normal) = %.4f\n", tau / se_tau, 2 * pnorm(-abs(tau / se_tau))))

# ── Wild cluster bootstrap (Rademacher, B=9999) sobre τ = D:POST:TR ──────────
# Inferencia preferida con ~29 clusters. impose_null = TRUE (recomendado).
cat("\n  Wild cluster bootstrap (B=9999, Rademacher) sobre D:POST:TR...\n")
boot_tau <- tryCatch({
  dqrng::dqset.seed(20260528)
  bt <- boottest(mod_dd, param = nm_tau, clustid = "edon",
                 B = 9999L, type = "rademacher", impose_null = TRUE)
  cat(sprintf("  ✓ p = %.4f   IC 95%% = [%.4f, %.4f]\n",
              bt$p_val, bt$conf_int[1], bt$conf_int[2]))
  bt
}, error = function(e) {
  cat(sprintf("  ⚠ bootstrap falló: %s\n    Inferencia por CR (cluster-robust) únicamente.\n",
              conditionMessage(e)))
  NULL
})
cat("\n")


# ── PASO 5: Robustez de bandwidth y de kernel ────────────────────────────────

cat("── PASO 5: Robustez — τ a través de bandwidths y kernel ─────────────────\n")

robustez <- map_dfr(c(0.5, 0.75, 1.0, 1.25, 1.5, 2.0), function(mult) {
  h_m <- h_ref * mult
  out <- tryCatch({
    m_tri <- ajustar_dd(panel_rdd, h_m, "triangular")$mod
    m_uni <- ajustar_dd(panel_rdd, h_m, "uniform")$mod
    nt <- find_coef(m_tri, c("D", "POST", "TR"))
    nu <- find_coef(m_uni, c("D", "POST", "TR"))
    tibble(
      mult    = mult, h = h_m,
      tau_tri = unname(coef(m_tri)[nt]),
      se_tri  = sqrt(vcov(m_tri, cluster = ~edon)[nt, nt]),
      tau_uni = unname(coef(m_uni)[nu]),
      se_uni  = sqrt(vcov(m_uni, cluster = ~edon)[nu, nu]),
      n       = m_tri$nobs
    )
  }, error = function(e) tibble(mult = mult, h = h_m, tau_tri = NA, se_tri = NA,
                                tau_uni = NA, se_uni = NA, n = NA))
  out
})
print(robustez, digits = 4)
cat("\n")


# ── PASO 6: Guardar resultados ────────────────────────────────────────────────

cat("── PASO 6: Guardando resultados ─────────────────────────────────────────\n")
saveRDS(
  list(
    modelo        = mod_dd,
    tau           = tau,
    se_tau        = se_tau,
    boot_tau      = boot_tau,
    diferenciales = tibble(
      cantidad = c("1er dif (treated)", "2o dif (never)", "tau (doble dif)"),
      estimado = c(fd_treated, fd_never, tau),
      se       = c(se_fd_treated, se_fd_never, se_tau)
    ),
    rd_celdas     = rd_celdas,
    h_ref         = h_ref,
    robustez      = robustez,
    metadata = list(
      estimador = "Doble diferencial sobre 4 discontinuidades (Grembi 2016; Picchetti 2026)",
      tau_param = "D:POST:TR (regresión local-lineal saturada, kernel triangular)",
      inferencia = "CR cluster=edon + wild cluster bootstrap Rademacher B=9999",
      POST      = "tratados: reforma_st; never-treated: 1{year>=2018}",
      nota      = "Sin DiD escalonado ni TWFE, por decisión de diseño.",
      fecha     = Sys.time()
    )
  ),
  "output/results/dindisc_todas.rds"
)
cat("✓ output/results/dindisc_todas.rds\n\n")


# ── PASO 7: Tabla LaTeX ───────────────────────────────────────────────────────

cat("── PASO 7: Tabla LaTeX ──────────────────────────────────────────────────\n")

p_tau   <- if (!is.null(boot_tau)) boot_tau$p_val else 2 * pnorm(-abs(tau / se_tau))
stars   <- function(p) if (is.na(p)) "" else if (p < .01) "$^{***}$" else if (p < .05) "$^{**}$" else if (p < .1) "$^{*}$" else ""

beta_celda <- function(rd) if (is.null(rd)) "---" else sprintf("%.4f", rd$coef["Bias-Corrected", 1])
se_celda   <- function(rd) if (is.null(rd)) "" else sprintf("(%.4f)", rd$se["Robust", 1])

tex <- c(
  "% Tabla DinDisc (doble diferencial, 4 discontinuidades) — todas las elecciones",
  "% Generado por 02_dindisc_todas.R",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Pre-reforma & Post-reforma \\\\",
  "\\midrule",
  sprintf("Tratados ($\\hat{\\beta}$)        & %s & %s \\\\",
          beta_celda(rd_celdas$treated_pre), beta_celda(rd_celdas$treated_post)),
  sprintf("                                  & %s & %s \\\\",
          se_celda(rd_celdas$treated_pre), se_celda(rd_celdas$treated_post)),
  sprintf("Control HGO+VER ($\\hat{\\beta}$) & %s & %s \\\\",
          beta_celda(rd_celdas$never_pre), beta_celda(rd_celdas$never_post)),
  sprintf("                                  & %s & %s \\\\",
          se_celda(rd_celdas$never_pre), se_celda(rd_celdas$never_post)),
  "\\midrule",
  sprintf("1er diferencial (tratados)        & \\multicolumn{2}{c}{%.4f \\;(%.4f)} \\\\", fd_treated, se_fd_treated),
  sprintf("2\\textsuperscript{o} diferencial (control) & \\multicolumn{2}{c}{%.4f \\;(%.4f)} \\\\", fd_never, se_fd_never),
  sprintf("\\textbf{$\\hat{\\tau}$ doble diferencial}   & \\multicolumn{2}{c}{\\textbf{%.4f%s} \\;(%.4f)} \\\\",
          tau, stars(p_tau), se_tau),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.85\\textwidth}}{\\footnotesize \\textit{Nota:} $\\hat{\\beta}$ por celda:",
  "  RDD local-lineal, kernel triangular, $h$ MSE-óptimo (Calonico, Cattaneo y Titiunik 2014),",
  "  SE bias-corrected. $\\hat{\\tau}=\\beta_{D:POST:TR}$ de la regresión local-lineal saturada",
  sprintf("  dentro de $|m|\\le %.3f$; SE clustered por estado; $p$ por wild cluster bootstrap", h_ref),
  "  Rademacher ($B=9999$). Control \\textit{never-treated}: Hidalgo y Veracruz.",
  "  $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}"
)
writeLines(tex, "output/tables/tabla_dindisc_todas.tex")
cat("✓ output/tables/tabla_dindisc_todas.tex\n\n")


# ── PASO 8: rdplots por celda + panel 2×2 ────────────────────────────────────

cat("── PASO 8: rdplots ──────────────────────────────────────────────────────\n")

rdplot_celda <- function(etq, titulo, ruta) {
  d <- panel_rdd |> filter(celda == etq, !is.na(outcome_rdd), !is.na(margin_signed))
  if (nrow(d) < 50) { message(sprintf("  ⚠ %s: insuficientes obs.", etq)); return(invisible(NULL)) }
  tryCatch({
    pdf(ruta, width = 7, height = 4.5)
    rdplot(y = d$outcome_rdd, x = d$margin_signed, c = 0, nbins = c(20, 20),
           title = titulo, x.label = "Margen de victoria (t-1)",
           y.label = "Voto del partido (fracción válida)",
           col.dots = "black", col.lines = "#00563F")
    dev.off()
  }, error = function(e) { message(sprintf("  ✗ rdplot %s: %s", etq, conditionMessage(e))); try(dev.off(), silent = TRUE) })
  cat(sprintf("  ✓ %s\n", ruta))
}
rdplot_celda("treated_pre",  "Tratados — pre-reforma",  "output/figures/rdplot_todas_treated_pre.pdf")
rdplot_celda("treated_post", "Tratados — post-reforma", "output/figures/rdplot_todas_treated_post.pdf")
rdplot_celda("never_pre",    "Control (HGO+VER) — pre-2018",  "output/figures/rdplot_todas_never_pre.pdf")
rdplot_celda("never_post",   "Control (HGO+VER) — desde 2018","output/figures/rdplot_todas_never_post.pdf")

pdf("output/figures/rdplot_dindisc_panel.pdf", width = 11, height = 9)
par(mfrow = c(2, 2))
for (cfg in list(
  list(c = "treated_pre",  t = "(a) Tratados — pre-reforma"),
  list(c = "treated_post", t = "(b) Tratados — post-reforma"),
  list(c = "never_pre",    t = "(c) Control — pre-2018"),
  list(c = "never_post",   t = "(d) Control — desde 2018"))) {
  d <- panel_rdd |> filter(celda == cfg$c, !is.na(outcome_rdd), !is.na(margin_signed))
  if (nrow(d) < 50) { plot(1, type = "n", main = paste(cfg$t, "(insuf.)")); next }
  tryCatch(rdplot(y = d$outcome_rdd, x = d$margin_signed, c = 0, nbins = c(15, 15),
                  title = cfg$t, x.label = "Margen (t-1)", y.label = "Voto del partido"),
           error = function(e) NULL)
}
dev.off()
cat("  ✓ output/figures/rdplot_dindisc_panel.pdf\n\n")


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════\n")
cat(sprintf(" 02 completado.  τ (doble diferencial) = %.4f  [SE %.4f, p = %.4f]\n",
            tau, se_tau, p_tau))
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
