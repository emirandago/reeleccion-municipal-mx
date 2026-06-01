# ══════════════════════════════════════════════════════════════════════════════
# Script:   03_dindisc_partido.R
# Proyecto: El municipio y la reelección
# Autor:    Emiliano Miranda González — ITAM
# Fecha:    2026-05-28
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Estimar el DOBLE DIFERENCIAL sobre cuatro discontinuidades (ver 02) por
#   separado para PRI, PAN y PRD, replicando la estrategia por partido de
#   Lucardi y Rosas (2016, Electoral Studies) —pero con el share del partido como
#   variable dependiente, no la probabilidad de retener— y con el componente de
#   diferencias inducido por la reforma de 2014.
#
# ESTIMADOR (idéntico a 02, por partido):
#   τ_P = [β_P(treated,post) − β_P(treated,pre)] − [β_P(never,post) − β_P(never,pre)]
#   recuperado como el coeficiente D:POST:TR de una regresión local-lineal
#   SATURADA dentro del bandwidth MSE-óptimo (kernel triangular). Inferencia:
#   SE clustered por estado + wild cluster bootstrap Rademacher B=9999.
#
# ── ATRIBUCIÓN DE COALICIONES: REGLA DE "PARTIDO MAYOR ÚNICO" ────────────────
#   PROBLEMA: con la regla previa (partido en cualquier posición del
#   identificador), una elección ganada por una coalición de dos mayores
#   —p.ej. PAN_PRI_PRD— entraba SIMULTÁNEAMENTE a los paneles del PAN, del PRI y
#   del PRD, con el MISMO margen pero distinto outcome: doble (triple) conteo y
#   atribución conceptualmente ambigua (¿qué partido "es" el incumbente y
#   disciplina al alcalde en una alianza de mayores?).
#
#   SOLUCIÓN (decisión de diseño, defendible y verificada en los datos):
#   se asigna cada elección al ÚNICO partido mayor de su coalición. Una
#   observación entra al panel del partido P si y solo si:
#       (i)  P está en la coalición focal (incumbente si ganador; segundo lugar
#            si perdedor), y
#       (ii) NINGÚN otro partido mayor {PRI,PAN,PRD,MORENA} está en esa coalición.
#   Esto hace los tres paneles MUTUAMENTE EXCLUYENTES (cero doble conteo) y
#   atribuye la incumbencia al partido mayor inequívoco. Verificación empírica
#   (01): las coaliciones con ≥2 mayores son apenas 1.5% de las elecciones, de
#   modo que el costo muestral es mínimo y recae justamente sobre los casos de
#   atribución ambigua que Magar (2026) señala (PAN_PRD, PAN_PRI_PRD, PRI_PRD).
#   La especificación "partido puro" (sin ninguna coalición) se reporta como
#   robustez adicional.
#
# MORENA: excluido del análisis principal por insuficiencia de observaciones
#   pre-reforma para estimar las celdas pre (la maldición basal). Se omite.
#
# INPUTS:  data/clean/panel_rdd_stacked.rds   (producido por 01_limpieza.R)
# OUTPUTS:
#   output/results/dindisc_partido.rds
#   output/tables/tabla_dindisc_partido.tex
#   output/figures/rdplot_panel_partidos.pdf
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
cat(" 03_dindisc_partido.R — Doble diferencial por partido (PRI, PAN, PRD)\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ── PASO 1: Panel + celdas + coalición focal y conteo de mayores ─────────────

cat("── PASO 1: Panel, celdas y regla de partido mayor único ─────────────────\n")

panel_rdd <- readRDS("data/clean/panel_rdd_stacked.rds")
panel_rdd <- panel_rdd |> filter(runnerup_party != "0" | is.na(runnerup_party))

stopifnot(
  "margin_signed sin negativos" = min(panel_rdd$margin_signed, na.rm = TRUE) < 0,
  "faltan incumbent_party/runnerup_party" =
    all(c("incumbent_party", "runnerup_party") %in% names(panel_rdd))
)

majors        <- c("PRI", "PAN", "PRD", "MORENA")
regex_major   <- function(p) paste0("^", p, "$|^", p, "_|_", p, "$|_", p, "_")
regex_majors  <- set_names(map_chr(majors, regex_major), majors)

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
      TR == 0L & POST == 0L ~ "never_pre"
    ),
    # Coalición focal: la del incumbente si esta fila es el ganador (D=1),
    # la del segundo lugar si es el perdedor (D=0).
    coal_focal = if_else(party_role == "winner", incumbent_party, runnerup_party),
    # Número de partidos MAYORES presentes en la coalición focal.
    n_major = rowSums(sapply(regex_majors, function(rx)
      str_detect(coalesce(coal_focal, ""), rx)))
  )

cat(sprintf("✓ Panel: %s obs. Coaliciones con ≥2 mayores: %s (%.1f%%) → excluidas por partido\n\n",
            format(nrow(panel_rdd), big.mark = ","),
            format(sum(panel_rdd$n_major >= 2), big.mark = ","),
            100 * mean(panel_rdd$n_major >= 2)))


# ── PASO 2: Definición de partidos ────────────────────────────────────────────

partidos <- list(
  PRI = list(nombre = "PRI", outcome = "share_PRI_valid_vote", color = "#00563F"),
  PAN = list(nombre = "PAN", outcome = "share_PAN_valid_vote", color = "#003DA5"),
  PRD = list(nombre = "PRD", outcome = "share_PRD_valid_vote", color = "#FFCC00")
)


# ── PASO 3: Funciones de panel por partido y de estimación ───────────────────

# build_party_panel(): aplica la regla de partido mayor único.
#   modo = "unico": P presente Y único mayor en la coalición focal (principal).
#   modo = "puro" : P corrió SIN coalición alguna (coal_focal == "PRI"/"PAN"/"PRD").
build_party_panel <- function(panel, partido, modo = c("unico", "puro")) {
  modo <- match.arg(modo)
  rx   <- regex_majors[[partido$nombre]]
  out  <- partido$outcome
  d <- panel |>
    mutate(outcome_party = .data[[out]]) |>
    filter(str_detect(coalesce(coal_focal, ""), rx),
           !is.na(outcome_party), !is.infinite(outcome_party), outcome_party <= 1)
  if (modo == "unico") d <- d |> filter(n_major == 1L)
  if (modo == "puro")  d <- d |> filter(coal_focal == partido$nombre)
  d
}

# Discontinuidad por celda con rdrobust (descriptivo y para rdplots).
rd_celda_partido <- function(dat, etq) {
  d <- dat |> filter(celda == etq, !is.na(outcome_party), !is.na(margin_signed))
  if (nrow(d) < 50 || sum(d$margin_signed < 0) < 15 || sum(d$margin_signed > 0) < 15) return(NULL)
  tryCatch(rdrobust(d$outcome_party, d$margin_signed, c = 0, cluster = d$edon,
                    vce = "hc2", bwselect = "mserd", kernel = "triangular"),
           error = function(e) NULL)
}

# Doble diferencial por partido: regresión local-lineal saturada + wild bootstrap.
#   Devuelve τ = coef(D:POST:TR), su SE clustered y el p de bootstrap.
dd_partido <- function(dat, etq_partido) {
  # Bandwidth MSE-óptimo del propio panel del partido (pool de celdas).
  d0 <- dat |> filter(!is.na(outcome_party), !is.na(margin_signed))
  if (nrow(d0) < 200) {
    message(sprintf("  ⚠ %s: N=%d insuficiente para DinDisc. NULL.", etq_partido, nrow(d0)))
    return(NULL)
  }
  rd0 <- tryCatch(rdrobust(d0$outcome_party, d0$margin_signed, c = 0,
                           vce = "hc2", bwselect = "mserd", kernel = "triangular"),
                  error = function(e) NULL)
  h <- if (!is.null(rd0)) rd0$bws["h", "left"] else 0.15

  d <- d0 |>
    filter(abs(margin_signed) <= h) |>
    mutate(m = margin_signed, w = pmax(0, 1 - abs(margin_signed) / h))

  # Necesitamos variación en las cuatro celdas para identificar D:POST:TR.
  if (n_distinct(d$celda) < 4 || any(table(d$celda) < 30)) {
    message(sprintf("  ⚠ %s: alguna celda con <30 obs dentro de h. τ puede ser frágil.", etq_partido))
  }

  mod <- tryCatch(
    feols(outcome_party ~ D * m * POST * TR, data = d, weights = ~w,
          cluster = ~edon, notes = FALSE),
    error = function(e) { message(sprintf("  ✗ feols %s: %s", etq_partido, conditionMessage(e))); NULL })
  if (is.null(mod)) return(NULL)

  # Localizar τ = coef(D:POST:TR) por componentes (robusto al orden de fixest).
  nms    <- names(coef(mod))
  nm_tau <- nms[vapply(strsplit(nms, ":"), function(z) setequal(z, c("D","POST","TR")), logical(1))]
  if (length(nm_tau) != 1) {
    message(sprintf("  ⚠ %s: no se identificó D:POST:TR (colinealidad en alguna celda). NULL.", etq_partido))
    return(NULL)
  }
  tau    <- unname(coef(mod)[nm_tau])
  se_tau <- sqrt(vcov(mod, cluster = ~edon)[nm_tau, nm_tau])
  boot   <- tryCatch({
    dqrng::dqset.seed(20260528)
    boottest(mod, param = nm_tau, clustid = "edon",
             B = 9999L, type = "rademacher", impose_null = TRUE)
  }, error = function(e) NULL)

  list(modelo = mod, tau = tau, se_tau = se_tau, h = h,
       p_boot = if (!is.null(boot)) boot$p_val else NA_real_,
       ci_boot = if (!is.null(boot)) boot$conf_int else c(NA, NA),
       n = mod$nobs, boot = boot)
}


# ── PASO 4: Loop principal (especificación principal: partido mayor único) ───

cat("── PASO 4: DinDisc por partido (regla de partido mayor único) ───────────\n\n")

resultados <- list(); tabla <- tibble()

for (nm in names(partidos)) {
  p <- partidos[[nm]]
  cat(sprintf("── %s ───────────────────────────────────────────────────────\n", nm))
  dat_p <- build_party_panel(panel_rdd, p, "unico")
  cat(sprintf("  Panel %s (mayor único): %s obs (ganador=%s, perdedor=%s)\n",
              nm, format(nrow(dat_p), big.mark = ","),
              format(sum(dat_p$D == 1L), big.mark = ","),
              format(sum(dat_p$D == 0L), big.mark = ",")))

  # Discontinuidad descriptiva por celda
  rd_c <- set_names(
    map(c("treated_pre","treated_post","never_pre","never_post"),
        ~ rd_celda_partido(dat_p, .x)),
    c("treated_pre","treated_post","never_pre","never_post"))
  for (cc in names(rd_c)) if (!is.null(rd_c[[cc]]))
    cat(sprintf("    %-13s β_BC=%7.4f  SE=%6.4f  N=%d\n", cc,
                rd_c[[cc]]$coef["Bias-Corrected",1], rd_c[[cc]]$se["Robust",1], rd_c[[cc]]$N))

  # Doble diferencial
  dd <- dd_partido(dat_p, nm)
  if (!is.null(dd)) {
    cat(sprintf("  ★ τ_%s (doble diferencial) = %.4f  [SE %.4f]  p_boot=%.4f  IC95%%=[%.4f, %.4f]\n\n",
                nm, dd$tau, dd$se_tau, dd$p_boot, dd$ci_boot[1], dd$ci_boot[2]))
    tabla <- bind_rows(tabla, tibble(
      partido = nm, tau = dd$tau, se = dd$se_tau, p_boot = dd$p_boot,
      ci_lo = dd$ci_boot[1], ci_hi = dd$ci_boot[2], h = dd$h, n = dd$n))
  } else cat(sprintf("  ⚠ τ_%s no estimable.\n\n", nm))

  resultados[[nm]] <- list(rd_celdas = rd_c, dd = dd)
}


# ── PASO 5: Robustez — especificación "partido puro" (sin coaliciones) ───────

cat("── PASO 5: Robustez — partido puro (sin ninguna coalición) ──────────────\n")
tabla_puro <- tibble()
for (nm in names(partidos)) {
  dat_pp <- build_party_panel(panel_rdd, partidos[[nm]], "puro")
  dd <- dd_partido(dat_pp, paste0(nm, "_puro"))
  if (!is.null(dd)) {
    cat(sprintf("  %s puro: τ = %.4f  [SE %.4f]  p_boot=%.4f  (N=%d)\n",
                nm, dd$tau, dd$se_tau, dd$p_boot, dd$n))
    tabla_puro <- bind_rows(tabla_puro, tibble(
      partido = paste0(nm, "_puro"), tau = dd$tau, se = dd$se_tau,
      p_boot = dd$p_boot, h = dd$h, n = dd$n))
  }
}
cat("\n")


# ── PASO 6: Guardar ──────────────────────────────────────────────────────────

saveRDS(
  list(por_partido = resultados, tabla_unico = tabla, tabla_puro = tabla_puro,
       metadata = list(
         estimador = "Doble diferencial 4 discontinuidades por partido (τ = D:POST:TR)",
         atribucion = "Partido mayor único (coaliciones con >=2 mayores excluidas)",
         inferencia = "CR cluster=edon + wild cluster bootstrap Rademacher B=9999",
         outcome = "share_P_valid_vote", fecha = Sys.time())),
  "output/results/dindisc_partido.rds")
cat("✓ output/results/dindisc_partido.rds\n\n")


# ── PASO 7: Tabla LaTeX (τ por partido) ──────────────────────────────────────

cat("── PASO 7: Tabla LaTeX ──────────────────────────────────────────────────\n")
stars <- function(p) if (is.na(p)) "" else if (p<.01) "$^{***}$" else if (p<.05) "$^{**}$" else if (p<.1) "$^{*}$" else ""
celda_tau <- function(nm, tb) {
  r <- tb |> filter(partido == nm)
  if (nrow(r) != 1 || is.na(r$tau)) return(c("---", ""))
  c(sprintf("%.4f%s", r$tau, stars(r$p_boot)), sprintf("(%.4f)", r$se))
}
tex <- c(
  "% Tabla DinDisc por partido (doble diferencial, 4 discontinuidades)",
  "% Generado por 03_dindisc_partido.R",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & PRI & PAN & PRD \\\\",
  "\\midrule")
filaA <- "$\\hat{\\tau}$ (mayor único)"; filaB <- ""
for (nm in c("PRI","PAN","PRD")) { v <- celda_tau(nm, tabla); filaA <- paste0(filaA, " & ", v[1]); filaB <- paste0(filaB, " & ", v[2]) }
tex <- c(tex, paste0(filaA, " \\\\"), paste0(filaB, " \\\\"), "\\midrule")
filaC <- "$\\hat{\\tau}$ (partido puro)"; filaD <- ""
for (nm in c("PRI","PAN","PRD")) { v <- celda_tau(paste0(nm,"_puro"), tabla_puro); filaC <- paste0(filaC, " & ", v[1]); filaD <- paste0(filaD, " & ", v[2]) }
tex <- c(tex, paste0(filaC, " \\\\"), paste0(filaD, " \\\\"),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.9\\textwidth}}{\\footnotesize \\textit{Nota:} $\\hat{\\tau}$ es el doble",
  "  diferencial sobre cuatro discontinuidades (coef.\\ $D{:}POST{:}TR$ de la regresión",
  "  local-lineal saturada, kernel triangular, $h$ MSE-óptimo). SE clustered por estado;",
  "  $p$ por wild cluster bootstrap Rademacher ($B=9999$). Atribución: partido mayor único",
  "  (coaliciones con $\\ge 2$ mayores excluidas; 1.5\\% de las elecciones). $^{*}p<0.10$,",
  "  $^{**}p<0.05$, $^{***}p<0.01$.}",
  "\\end{tabular}")
writeLines(tex, "output/tables/tabla_dindisc_partido.tex")
cat("✓ output/tables/tabla_dindisc_partido.tex\n\n")


# ── PASO 8: Panel rdplots 3×2 (tratados pre vs post por partido) ─────────────

cat("── PASO 8: Panel rdplots 3×2 ────────────────────────────────────────────\n")
pdf("output/figures/rdplot_panel_partidos.pdf", width = 11, height = 13)
par(mfrow = c(3, 2))
for (nm in names(partidos)) {
  p <- partidos[[nm]]; dat_p <- build_party_panel(panel_rdd, p, "unico")
  for (cfg in list(list(c="treated_pre", t="pre-reforma"), list(c="treated_post", t="post-reforma"))) {
    d <- dat_p |> filter(celda == cfg$c, !is.na(outcome_party), !is.na(margin_signed))
    if (nrow(d) < 50) { plot(1, type="n", main = sprintf("%s — %s (insuf.)", nm, cfg$t)); next }
    tryCatch(rdplot(d$outcome_party, d$margin_signed, c = 0, nbins = c(15, 15),
                    title = sprintf("%s — %s", nm, cfg$t),
                    x.label = "Margen (t-1)", y.label = sprintf("Voto %s", nm)),
             error = function(e) NULL)
  }
}
dev.off()
cat("✓ output/figures/rdplot_panel_partidos.pdf\n\n")


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════════════════\n")
cat(" Resumen τ por partido (especificación principal: mayor único)\n")
print(tabla |> select(partido, tau, se, p_boot), digits = 4)
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
