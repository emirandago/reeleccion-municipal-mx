# ══════════════════════════════════════════════════════════════════════════════
# Script:   05_apendice_figuras_tablas.R
# Proyecto: El municipio y la reelección
# Autor:    Emiliano Miranda González — ITAM
# Fecha:    2026-05-29
# ══════════════════════════════════════════════════════════════════════════════
# PROPÓSITO:
#   Generar TODAS las figuras y tablas del apéndice técnico, con los cánones
#   estéticos de AER/APSR, a partir de los objetos .rds que ya guardaron
#   01–04 y 03b. Es el único script de presentación del proyecto.
#
#   Convenciones (replicadas de los scripts de referencia del autor):
#     • Figuras: ggplot2 con tema académico (panel.border, sin grid menor),
#       rdplot reconstruido a la rdplot_gg() (bins + polinomio local partido en
#       el umbral, recta vertical en c=0); PDF vectorial + PNG.
#     • Tablas: flextable → .docx en Latin Modern Roman, bordes booktabs
#       (fp_border grueso/fino), sin cero inicial, signo menos tipográfico (−).
#
#   AUTÓNOMO y REPRODUCIBLE: corre tras 01–04 y 03b. Si falta algún .rds,
#   el bloque correspondiente se omite con aviso, sin abortar el script.
#
# INPUTS:
#   data/clean/panel_rdd_stacked.rds
#   output/results/{dindisc_todas, dindisc_partido, balance_covariables,
#                   mccrary_resultados, dindisc_morena}.rds
# OUTPUTS:
#   output/figures/figA*.pdf (+ .png)
#   output/tables/tablaA*.docx
# ══════════════════════════════════════════════════════════════════════════════


# ── PASO 0: Configuración, tema y helpers ────────────────────────────────────

if (!require("pacman")) install.packages("pacman")
suppressPackageStartupMessages(
  pacman::p_load(tidyverse, rdrobust, rddensity, patchwork,
                 flextable, officer)
)

options(scipen = 999, digits = 4)
set.seed(7)

RUTA_FIG <- "output/figures"
RUTA_TAB <- "output/tables"
dir.create(RUTA_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(RUTA_TAB, recursive = TRUE, showWarnings = FALSE)

# Tema académico (idéntico en espíritu al 00_setup.R de referencia).
tema_academico <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9, color = "grey30"),
    plot.title       = element_text(size = 10, face = "bold", hjust = 0),
    plot.subtitle    = element_text(size = 9, color = "grey40", hjust = 0),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0),
    legend.position  = "bottom",
    plot.margin      = margin(8, 8, 8, 8)
  )
theme_set(tema_academico)

# sin_cero(): elimina el cero antes del punto decimal (.2 en vez de 0.2).
sin_cero <- function(x) {
  s <- formatC(x, format = "f", digits = 1)
  ifelse(x == 0, "0", sub("\\.0$", "", sub("^(-?)0\\.", "\\1.", s)))
}

# fmt_n(): formato de tabla sin cero inicial y con signo menos tipográfico.
fmt_n <- function(x, d = 3) {
  if (is.na(x)) return("---")
  s <- sub("^0\\.", ".", formatC(abs(x), format = "f", digits = d))
  if (x < 0) paste0("−", s) else s
}
estrellas <- function(p) ifelse(is.na(p), "",
  ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))

# rdplot_gg(): reconstruye rdplot() como ggplot manipulable (de _functions.R).
rdplot_gg <- function(y, x, titulo = NULL, ytitle = "", xtitle = "Margen de victoria (t-1)",
                      xlim_plot = c(-0.5, 0.5), color_pts = "grey50", color_fit = "black",
                      p_poly = 4, ...) {
  mask <- !is.na(x) & !is.na(y) & x >= xlim_plot[1] & x <= xlim_plot[2]
  if (sum(mask) < 20) stop("Insuficientes observaciones en xlim_plot.")
  rp   <- rdplot(y = y[mask], x = x[mask], p = p_poly, hide = TRUE, ...)
  bins <- rp$vars_bins
  poly <- rp$vars_poly
  ggplot() +
    geom_point(data = bins, aes(rdplot_mean_x, rdplot_mean_y),
               color = color_pts, size = 1.4, shape = 16, alpha = 0.85) +
    geom_line(data = filter(poly, rdplot_x <= 0), aes(rdplot_x, rdplot_y),
              color = color_fit, linewidth = 0.55) +
    geom_line(data = filter(poly, rdplot_x >= 0), aes(rdplot_x, rdplot_y),
              color = color_fit, linewidth = 0.55) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
    scale_x_continuous(limits = xlim_plot,
                       breaks = seq(xlim_plot[1], xlim_plot[2], 0.25), labels = sin_cero) +
    labs(title = titulo, x = xtitle, y = ytitle) +
    theme(aspect.ratio = 1)
}

guardar_figura <- function(p, archivo, ancho = 6, alto = 4) {
  ggsave(file.path(RUTA_FIG, paste0(archivo, ".pdf")), p, width = ancho, height = alto, device = "pdf")
  tryCatch(ggsave(file.path(RUTA_FIG, paste0(archivo, ".png")), p, width = ancho, height = alto, dpi = 300),
           error = function(e) NULL)
  message("Figura: ", archivo)
}

# Bordes booktabs y guardado de tabla flextable → .docx (de 03_tabla1.R).
b_thick <- fp_border(color = "black", width = 1.5)
b_thin  <- fp_border(color = "black", width = 0.75)

tabla_docx <- function(df, titulo, header_labels = NULL, spanner = NULL,
                       footer = NULL, archivo, ancho1 = 2.4, anchoj = 1.5) {
  ft <- flextable(df)
  if (!is.null(header_labels)) ft <- set_header_labels(ft, values = header_labels)
  if (!is.null(spanner))
    ft <- add_header_row(ft, values = c("", spanner$label),
                         colwidths = c(1, spanner$span))
  ncol_df <- ncol(df)
  ft <- ft |>
    font(fontname = "Latin Modern Roman", part = "all") |>
    fontsize(size = 11, part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    align(j = 2:ncol_df, align = "center", part = "all") |>
    width(j = 1, width = ancho1) |>
    width(j = 2:ncol_df, width = anchoj) |>
    border_remove() |>
    hline_top(border = b_thick, part = "header") |>
    hline_bottom(border = b_thin, part = "header") |>
    hline_bottom(border = b_thick, part = "body")
  if (!is.null(footer)) {
    ft <- ft |> add_footer_lines(footer) |>
      font(fontname = "Latin Modern Roman", part = "footer") |>
      fontsize(size = 9, part = "footer") |> align(align = "left", part = "footer")
  }
  read_docx() |>
    body_add_par(titulo, style = "heading 2") |>
    body_add_flextable(ft) |>
    print(target = file.path(RUTA_TAB, paste0(archivo, ".docx")))
  message("Tabla: ", archivo, ".docx")
}

# Lectura segura de .rds.
leer <- function(ruta) if (file.exists(ruta)) readRDS(ruta) else { message("⚠ falta: ", ruta); NULL }


# ── PASO 1: Cargar resultados y reconstruir paneles (para rdplots) ───────────

cat("\n── PASO 1: Cargando .rds y reconstruyendo paneles ───────────────────────\n")

res_todas   <- leer("output/results/dindisc_todas.rds")
res_partido <- leer("output/results/dindisc_partido.rds")
res_balance <- leer("output/results/balance_covariables.rds")
res_mccrary <- leer("output/results/mccrary_resultados.rds")
res_morena  <- leer("output/results/dindisc_morena.rds")

panel_rdd <- leer("data/clean/panel_rdd_stacked.rds")
stopifnot("No se encontró el panel apilado" = !is.null(panel_rdd))
panel_rdd <- panel_rdd |> filter(runnerup_party != "0" | is.na(runnerup_party))

majors       <- c("PRI", "PAN", "PRD", "MORENA")
regex_major  <- function(p) paste0("^", p, "$|^", p, "_|_", p, "$|_", p, "_")
regex_majors <- set_names(map_chr(majors, regex_major), majors)

panel_rdd <- panel_rdd |>
  mutate(
    TR = as.integer(cohort_g > 0L),
    POST = if_else(cohort_g > 0L, as.integer(reforma_st == 1L), as.integer(year >= 2018L)),
    D = as.integer(margin_signed >= 0),
    celda = case_when(
      TR == 1L & POST == 1L ~ "treated_post",
      TR == 1L & POST == 0L ~ "treated_pre",
      TR == 0L & POST == 1L ~ "never_post",
      TR == 0L & POST == 0L ~ "never_pre"),
    coal_focal = if_else(party_role == "winner", incumbent_party, runnerup_party),
    n_major = rowSums(sapply(regex_majors, function(rx) str_detect(coalesce(coal_focal, ""), rx)))
  )

party_sample <- function(p) {
  panel_rdd |>
    mutate(outcome_party = .data[[paste0("share_", p, "_valid_vote")]]) |>
    filter(str_detect(coalesce(coal_focal, ""), regex_majors[[p]]), n_major == 1L,
           !is.na(outcome_party), !is.infinite(outcome_party), outcome_party <= 1)
}

etq_celda <- c(treated_pre = "Tratados, pre-reforma", treated_post = "Tratados, post-reforma",
               never_pre = "Control (HGO+VER), pre-2018", never_post = "Control (HGO+VER), desde 2018")


# ── PASO 2: FIGURAS ───────────────────────────────────────────────────────────

cat("── PASO 2: Figuras ──────────────────────────────────────────────────────\n")

## Figura A1: rdplots de las cuatro celdas (panel 2×2), outcome agregado ──────
plot_celda <- function(cel, ytitle = "Voto del partido (fracción válida)") {
  d <- panel_rdd |> filter(celda == cel, !is.na(outcome_rdd), !is.na(margin_signed))
  tryCatch(
    rdplot_gg(d$outcome_rdd, d$margin_signed, titulo = etq_celda[[cel]],
              ytitle = ytitle, binselect = "es") +
      scale_y_continuous(labels = sin_cero),
    error = function(e) { message(sprintf("  ⚠ A1 %s: %s", cel, conditionMessage(e))); NULL })
}
pls <- compact(map(c("treated_pre","treated_post","never_pre","never_post"), plot_celda))
if (length(pls) >= 1) {
  figA1 <- wrap_plots(pls, ncol = 2) +
    plot_annotation(caption = "Discontinuidad de la ventaja del incumbente por celda del doble diferencial. RDD local-lineal; bins y polinomio de cuarto orden (Calonico, Cattaneo y Titiunik 2015).")
  guardar_figura(figA1, "figA1_rdplots_celdas", ancho = 8.5, alto = 8.5)
}

## Figura A2: rdplots por partido, tratados pre vs post (panel 3×2) ──────────
pls2 <- list()
for (p in c("PRI", "PAN", "PRD")) {
  dp <- party_sample(p)
  for (cel in c("treated_pre", "treated_post")) {
    d <- dp |> filter(celda == cel, !is.na(outcome_party), !is.na(margin_signed))
    pl <- tryCatch(
      rdplot_gg(d$outcome_party, d$margin_signed,
                titulo = sprintf("%s — %s", p, ifelse(cel == "treated_pre", "pre", "post")),
                ytitle = sprintf("Voto %s", p), binselect = "es") +
        scale_y_continuous(labels = sin_cero),
      error = function(e) NULL)
    pls2 <- c(pls2, list(pl))
  }
}
pls2 <- compact(pls2)
if (length(pls2) >= 1) {
  figA2 <- wrap_plots(pls2, ncol = 2) +
    plot_annotation(caption = "Discontinuidad de incumbencia por partido (regla de partido mayor único), antes y después de la reforma en los estados tratados.")
  guardar_figura(figA2, "figA2_rdplots_partido", ancho = 8.5, alto = 11)
}

## Figura A3: densidad de McCrary por partido (test legítimo) ─────────────────
pls3 <- list()
for (p in c("PRI", "PAN", "PRD")) {
  d <- party_sample(p) |> filter(!is.na(margin_signed))
  pl <- tryCatch({
    rdd <- rddensity(d$margin_signed, c = 0)
    rp  <- rdplotdensity(rdd, d$margin_signed, plotN = 25,
                         CItype = "all", histFillShade = 0.3)
    rp$Estplot + labs(title = sprintf("%s  (p = %s)", p, fmt_n(rdd$test$p_jk)),
                      x = "Margen firmado del partido focal", y = "Densidad") +
      tema_academico + theme(aspect.ratio = 1, legend.position = "none")
  }, error = function(e) { message(sprintf("  ⚠ A3 %s: %s", p, conditionMessage(e))); NULL })
  pls3 <- c(pls3, list(pl))
}
pls3 <- compact(pls3)
if (length(pls3) >= 1) {
  figA3 <- wrap_plots(pls3, nrow = 1) +
    plot_annotation(caption = "Prueba de densidad de Cattaneo, Jansson y Ma (2020) sobre el margen firmado del partido focal. La hipótesis nula es continuidad de la densidad en el umbral.")
  guardar_figura(figA3, "figA3_mccrary_partido", ancho = 12, alto = 4.6)
}

## Figura A4: sensibilidad de τ al ancho de banda (DinDisc agregado) ──────────
if (!is.null(res_todas) && !is.null(res_todas$robustez)) {
  rob <- res_todas$robustez |>
    mutate(lo = tau_tri - 1.96 * se_tri, hi = tau_tri + 1.96 * se_tri)
  figA4 <- ggplot(rob, aes(h, tau_tri)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey85", alpha = 0.7) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.8) +
    { if (!is.null(res_todas$h_ref)) geom_vline(xintercept = res_todas$h_ref,
        linetype = "dotted", color = "#00563F") } +
    scale_y_continuous(labels = sin_cero) +
    labs(x = "Ancho de banda h", y = expression(hat(tau)~"(doble diferencial)"),
         caption = "Estimador τ del doble diferencial a través de múltiplos del ancho de banda MSE-óptimo (línea punteada), kernel triangular, IC 95% cluster-robusto. La banda incluye el cero en todo el rango.") +
    theme(aspect.ratio = 0.7)
  guardar_figura(figA4, "figA4_sensibilidad_bw", ancho = 7, alto = 5)
}

## Figura A5: balance de covariables predeterminadas (coefplot) ──────────────
if (!is.null(res_balance) && !is.null(res_balance$tabla) && nrow(res_balance$tabla) > 0) {
  etq_cov <- c(share_incumbent_lag = "Voto incumbente (t-2)", log_pobtot_2010 = "Log población 2010",
               tasa_pobreza_2010 = "Tasa pobreza 2010", graproes_2010 = "Escolaridad 2010",
               pct_hli_2010 = "% lengua indígena 2010")
  figA5 <- res_balance$tabla |>
    mutate(lo = beta - 1.96 * se, hi = beta + 1.96 * se,
           cov_lab = recode(covariable, !!!etq_cov)) |>
    ggplot(aes(beta, cov_lab, color = partido)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(position = position_dodge(width = 0.6), size = 2.2) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2,
                   position = position_dodge(width = 0.6)) +
    scale_color_grey(start = 0.1, end = 0.6) +
    labs(x = "Coeficiente RD bias-corrected (IC 95%)", y = NULL, color = "Partido",
         caption = "Balance al cutoff: cada covariable predeterminada como outcome ficticio. Efecto esperado = 0 bajo continuidad (Marshall 2024). turnout se excluye por no ser predeterminado.")
  guardar_figura(figA5, "figA5_balance", ancho = 8, alto = 5)
}

## Figura A6: rdplot de MORENA (exploratorio, post-reforma) ───────────────────
dmor <- party_sample("MORENA") |> filter(celda == "treated_post", !is.na(outcome_party), !is.na(margin_signed))
figA6 <- tryCatch(
  rdplot_gg(dmor$outcome_party, dmor$margin_signed, titulo = "MORENA — post-reforma (exploratorio)",
            ytitle = "Voto MORENA (fracción válida)", color_fit = "#A6093D", binselect = "es") +
    scale_y_continuous(labels = sin_cero) +
    labs(caption = "Discontinuidad de incumbencia de MORENA en el régimen reformado. NO es el efecto causal de la reforma: el doble diferencial es inestimable por carecer de observaciones pre-reforma."),
  error = function(e) { message(sprintf("  ⚠ A6: %s", conditionMessage(e))); NULL })
if (!is.null(figA6)) guardar_figura(figA6, "figA6_morena", ancho = 6, alto = 5.4)


# ── PASO 3: TABLAS (flextable → .docx, estilo AER/APSR) ──────────────────────

cat("── PASO 3: Tablas ───────────────────────────────────────────────────────\n")

beta_bc <- function(rd) if (is.null(rd)) NA_real_ else rd$coef["Bias-Corrected", 1]
se_rob  <- function(rd) if (is.null(rd)) NA_real_ else rd$se["Robust", 1]
p_rob   <- function(rd) if (is.null(rd)) NA_real_ else rd$pv["Robust", 1]
n_eff   <- function(rd) if (is.null(rd)) NA_integer_ else sum(rd$N_h)
bw_h    <- function(rd) if (is.null(rd)) NA_real_ else rd$bws["h", "left"]

## Tabla A1: DinDisc agregado (cuatro celdas + diferenciales + τ) ─────────────
if (!is.null(res_todas)) {
  rc <- res_todas$rd_celdas
  p_tau <- if (!is.null(res_todas$boot_tau)) res_todas$boot_tau$p_val else
    2 * pnorm(-abs(res_todas$tau / res_todas$se_tau))
  celda_cell <- function(rd) c(paste0(fmt_n(beta_bc(rd)), estrellas(p_rob(rd))), paste0("(", fmt_n(se_rob(rd)), ")"))
  df_a1 <- data.frame(
    Cantidad = c("β: tratados", "", "β: control (HGO+VER)", "",
                 "1er diferencial (tratados)", "2º diferencial (control)",
                 "τ̂ doble diferencial"),
    `Pre-reforma` = c(celda_cell(rc$treated_pre), celda_cell(rc$never_pre), "", "", ""),
    `Post-reforma` = c(celda_cell(rc$treated_post), celda_cell(rc$never_post), "", "", ""),
    check.names = FALSE, stringsAsFactors = FALSE)
  dif <- res_todas$diferenciales
  # Colocar diferenciales y τ en la 2ª columna, centrados conceptualmente
  df_a1$`Pre-reforma`[5] <- paste0(fmt_n(dif$estimado[1]), " (", fmt_n(dif$se[1]), ")")
  df_a1$`Pre-reforma`[6] <- paste0(fmt_n(dif$estimado[2]), " (", fmt_n(dif$se[2]), ")")
  df_a1$`Pre-reforma`[7] <- paste0(fmt_n(res_todas$tau), estrellas(p_tau), " (", fmt_n(res_todas$se_tau), ")")
  df_a1$`Post-reforma`[5:7] <- ""
  names(df_a1) <- c(" ", "Pre-reforma", "Post-reforma / valor")
  tabla_docx(df_a1, "Tabla A1. Doble diferencial sobre cuatro discontinuidades (agregado)",
    footer = paste(
      "Nota. β por celda: RDD local-lineal, kernel triangular, h MSE-óptimo (Calonico, Cattaneo y Titiunik 2014), SE bias-corrected.",
      sprintf("τ̂ = coeficiente D×POST×TR de la regresión local-lineal saturada en |m| ≤ %s; SE cluster-robusto (estado);", fmt_n(res_todas$h_ref)),
      "p por wild cluster bootstrap Rademacher (B = 9999). Control never-treated: Hidalgo y Veracruz.",
      "*** p < .01, ** p < .05, * p < .10."),
    archivo = "tablaA1_dindisc_agregado", ancho1 = 2.6, anchoj = 1.7)
}

## Tabla A2: DinDisc por partido (principal mayor único + robustez puro) ──────
if (!is.null(res_partido)) {
  tu <- res_partido$tabla_unico; tp <- res_partido$tabla_puro
  fila <- function(tb, nm) {
    r <- tb |> filter(partido == nm)
    if (nrow(r) != 1 || is.na(r$tau)) return(c("---", ""))
    c(paste0(fmt_n(r$tau), estrellas(r$p_boot)), paste0("(", fmt_n(r$se), ")"))
  }
  df_a2 <- data.frame(
    ` ` = c("τ̂ (mayor único)", "", "τ̂ (partido puro)", ""),
    PRI = c(fila(tu, "PRI"), fila(tp, "PRI_puro")),
    PAN = c(fila(tu, "PAN"), fila(tp, "PAN_puro")),
    PRD = c(fila(tu, "PRD"), fila(tp, "PRD_puro")),
    check.names = FALSE, stringsAsFactors = FALSE)
  tabla_docx(df_a2, "Tabla A2. Doble diferencial por partido",
    footer = paste(
      "Nota. τ̂ = doble diferencial sobre cuatro discontinuidades (coef. D×POST×TR de la regresión local-lineal saturada,",
      "kernel triangular, h MSE-óptimo). SE cluster-robusto (estado); p por wild cluster bootstrap Rademacher (B = 9999).",
      "Atribución por partido mayor único; coaliciones con ≥2 mayores excluidas. *** p < .01, ** p < .05, * p < .10."),
    archivo = "tablaA2_dindisc_partido", ancho1 = 2.4, anchoj = 1.4)
}

## Tabla A3: balance de covariables predeterminadas ──────────────────────────
if (!is.null(res_balance) && nrow(res_balance$tabla) > 0) {
  etq_cov <- c(share_incumbent_lag = "Voto incumbente (t−2)", log_pobtot_2010 = "Log población 2010",
               tasa_pobreza_2010 = "Tasa pobreza 2010", graproes_2010 = "Escolaridad 2010",
               pct_hli_2010 = "% lengua indígena 2010")
  # Deduplicar por (partido, covariable) por robustez: versiones previas de 04
  # podían guardar filas duplicadas (n = rd$N de longitud 2). distinct() deja una.
  bal <- res_balance$tabla |>
    dplyr::distinct(partido, covariable, .keep_all = TRUE)
  covs <- intersect(names(etq_cov), unique(bal$covariable))
  filas <- list()
  for (z in covs) {
    b <- s <- character(3)
    for (i in seq_along(c("PRI","PAN","PRD"))) {
      r <- bal |> filter(partido == c("PRI","PAN","PRD")[i], covariable == z)
      if (nrow(r) >= 1) {
        r <- r[1, ]
        b[i] <- paste0(fmt_n(r$beta), estrellas(r$pval)); s[i] <- paste0("(", fmt_n(r$se), ")")
      } else { b[i] <- "---"; s[i] <- "" }
    }
    filas[[length(filas)+1]] <- c(etq_cov[[z]], b)
    filas[[length(filas)+1]] <- c("", s)
  }
  df_a3 <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  names(df_a3) <- c(" ", "PRI", "PAN", "PRD")
  tabla_docx(df_a3, "Tabla A3. Balance de covariables predeterminadas al cutoff",
    footer = paste(
      "Nota. Cada celda: estimador RD bias-corrected con la covariable predeterminada como outcome ficticio,",
      "sobre el margen firmado del partido focal. SE robustos (cluster-estado), h MSE-óptimo.",
      "turnout se excluye por contemporáneo al outcome. La ausencia de discontinuidad corrobora el supuesto",
      "de continuidad (Marshall 2024; Lucardi y Rosas 2016). *** p < .01, ** p < .05, * p < .10."),
    archivo = "tablaA3_balance", ancho1 = 2.6, anchoj = 1.4)
}

## Tabla A4: pruebas de densidad de McCrary por partido ──────────────────────
if (!is.null(res_mccrary)) {
  filas <- map(c("PRI","PAN","PRD"), function(p) {
    rd <- res_mccrary[[p]]
    if (is.null(rd)) return(c(p, "---", "---"))
    c(p, fmt_n(rd$test$t_jk), fmt_n(rd$test$p_jk))
  })
  df_a4 <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  names(df_a4) <- c("Partido", "Estadístico T", "valor p")
  tabla_docx(df_a4, "Tabla A4. Prueba de densidad (manipulación) por partido",
    footer = paste(
      "Nota. Prueba de Cattaneo, Jansson y Ma (2020) sobre el margen firmado del partido focal.",
      "H0: continuidad de la densidad en el umbral (ausencia de sorting). p > .10 es consistente con no manipulación."),
    archivo = "tablaA4_mccrary", ancho1 = 1.8, anchoj = 1.8)
}

## Tabla A5: MORENA (exploratorio) ───────────────────────────────────────────
if (!is.null(res_morena)) {
  p_post <- if (!is.null(res_morena$boot_post)) res_morena$boot_post$p_val else p_rob(res_morena$rd_post)
  df_a5 <- data.frame(
    `Especificación` = c("MORENA, mayor único (todo)", "  celda treated_post", "MORENA puro (sin coalición)"),
    `β (salto)` = c(paste0(fmt_n(beta_bc(res_morena$rd_full)), estrellas(p_rob(res_morena$rd_full))),
                    paste0(fmt_n(beta_bc(res_morena$rd_post)), estrellas(p_post)),
                    paste0(fmt_n(beta_bc(res_morena$rd_puro)), estrellas(p_rob(res_morena$rd_puro)))),
    `(SE)` = c(paste0("(", fmt_n(se_rob(res_morena$rd_full)), ")"),
               paste0("(", fmt_n(se_rob(res_morena$rd_post)), ")"),
               paste0("(", fmt_n(se_rob(res_morena$rd_puro)), ")")),
    N = c(format(n_eff(res_morena$rd_full), big.mark = ","),
          format(n_eff(res_morena$rd_post), big.mark = ","),
          format(n_eff(res_morena$rd_puro), big.mark = ",")),
    check.names = FALSE, stringsAsFactors = FALSE)
  tabla_docx(df_a5, "Tabla A5. MORENA: discontinuidad de incumbencia post-reforma (exploratorio)",
    footer = paste(
      "Nota. Discontinuidad de incumbencia (RDD local-lineal, kernel triangular, h MSE-óptimo, SE bias-corrected cluster-estado).",
      "NO es el efecto causal de la reforma: el doble diferencial es inestimable para MORENA por carecer de observaciones pre-reforma.",
      "p de la celda treated_post por wild cluster bootstrap Rademacher. *** p < .01, ** p < .05, * p < .10."),
    archivo = "tablaA5_morena", ancho1 = 2.8, anchoj = 1.3)
}

## Tabla A6: composición de la muestra por celda (descriptivo) ───────────────
df_a6 <- panel_rdd |>
  group_by(celda) |>
  summarise(N_obs = n(), N_municipios = n_distinct(mun_id), N_estados = n_distinct(edon),
            .groups = "drop") |>
  mutate(celda = recode(celda, !!!etq_celda)) |>
  rename(Celda = celda) |>
  mutate(across(where(is.numeric), ~format(.x, big.mark = ",")))
tabla_docx(df_a6, "Tabla A6. Composición de la muestra por celda del diseño",
  footer = "Nota. Panel apilado (dos observaciones por elección: ganador y segundo lugar). Excluye CDMX, Nayarit y Tlaxcala; control never-treated: Hidalgo y Veracruz.",
  archivo = "tablaA6_muestra", ancho1 = 2.8, anchoj = 1.3)


# ── Cierre ────────────────────────────────────────────────────────────────────

cat("\n══════════════════════════════════════════════════════════════════════════\n")
cat(" 05 completado — apéndice técnico generado.\n")
cat("  Figuras → output/figures/figA1..figA6 (.pdf + .png)\n")
cat("  Tablas  → output/tables/tablaA1..tablaA6 (.docx, Latin Modern Roman)\n")
cat("══════════════════════════════════════════════════════════════════════════\n")
cat("Fin:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
