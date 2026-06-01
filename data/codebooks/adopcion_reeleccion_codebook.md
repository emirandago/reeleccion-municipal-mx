# D03_adopcion_reeleccion_codebook.md
# Codebook: Adopción Escalonada de Reelección Municipal Consecutiva en México
# Fuente: Eric Magar, "Calendarios electorales e instituciones de reelección
# consecutiva de México desde 1994", base de datos en http://ericmagar.com/datos/
# Archivo original: fechasEleccionesMexicoDesde1994.xlsx (3 hojas: data, fuentes, codebook)
# Procesado por: Emiliano Miranda González — Proyecto Reelección Municipal ITAM 2026

---

<fuente_original>

La base de Magar es el único repositorio sistemático de calendarios electorales
municipales mexicanos e instituciones de reelección consecutiva desde 1994. Tiene
tres hojas: (1) data — la base principal con 104 filas y 49 columnas; (2) fuentes —
nota metodológica del autor; (3) codebook — descripción de variables.

La unidad de observación es estado × tipo de elección (gob = gubernatorial,
dloc = diputados locales, ayun = ayuntamientos municipales). Para el proyecto
de reelección municipal solo son relevantes las filas donde elec = "ayun".

Contacto del autor para errores: emagar@itam.mx

</fuente_original>

---

<variables_clave>

**edon** — Código numérico del estado, compatible con INEGI (1-32). Excepción: edon=0
es el nivel federal. El código 9 corresponde a Ciudad de México (EXCLUIR del análisis
municipal: tiene alcaldías, no municipios).

**elec** — Tipo de elección. Valores: "pres" (presidencial), "dip" (diputados
federales), "sen" (senadores), "gob" (gobernador), "dloc" (diputados locales),
"ayun" (ayuntamientos municipales). Para el proyecto: usar ÚNICAMENTE filas
donde elec == "ayun".

**yr1st** — Año del primer ciclo electoral en que los incumbentes pueden contender
para reelegirse. Valores posibles:
  - Año numérico (e.g., 2018): adopción de reelección consecutiva confirmada
  - "no consecutiva": el estado permite reelección pero NO consecutiva (nTerms=1).
    Estos estados son los grupos de control permanentes (never-treated).
  - "2024?": adopción incierta en 2024 (Nayarit — verificar)

**nTerms** — Número de periodos consecutivos que puede permanecer un incumbente.
  - 2: puede reelegirse UNA vez (incumbente → reelección → fin)
  - 1: solo un periodo (equivalente a no-reelección efectiva)

**y1994 a y2025** — Día y mes de la elección municipal ese año. Formato: "DDmes"
(e.g., "1jul" = 1 de julio). "--" indica que no hubo elección ese año.

</variables_clave>

---

<clasificacion_para_did>

Esta es la clasificación operativa de cada estado para el diseño Callaway-Sant'Anna
y para la variable cohort_g (gname en el paquete `did`).

<grupo id="never_treated" cohort_g="0" descripcion="Grupo de control puro — nunca adoptaron reelección consecutiva">

  edon=13 | Hidalgo    | yr1st="no consecutiva" | nTerms=1
  edon=30 | Veracruz   | yr1st="no consecutiva" | nTerms=1

  Nota: Estos son los únicos dos estados que NUNCA adoptaron reelección consecutiva
  municipal. Son el grupo de control permanente en la especificación
  control_group="nevertreated". Sin embargo, dado que son solo 2 estados, la
  especificación principal debe usar control_group="notyettreated" para maximizar
  el grupo de comparación. Estos 2 estados siempre forman parte del control.

</grupo>

<grupo id="early_adopter_2018" cohort_g="2018" descripcion="Cohorte principal — 22 estados">

  edon=3  | Baja California Sur  | primera elección con reelección: 1jul2018
  edon=4  | Campeche             | primera elección con reelección: 1jul2018
  edon=5  | Coahuila             | primera elección con reelección: 1jul2018
  edon=6  | Colima               | primera elección con reelección: 1jul2018
  edon=7  | Chiapas              | primera elección con reelección: 1jul2018
  edon=8  | Chihuahua            | primera elección con reelección: 1jul2018
  edon=11 | Guanajuato           | primera elección con reelección: 1jul2018
  edon=12 | Guerrero             | primera elección con reelección: 1jul2018
  edon=14 | Jalisco              | primera elección con reelección: 1jul2018
  edon=15 | México               | primera elección con reelección: 1jul2018
  edon=16 | Michoacán            | primera elección con reelección: 1jul2018
  edon=17 | Morelos              | primera elección con reelección: 1jul2018
  edon=19 | Nuevo León           | primera elección con reelección: 1jul2018
  edon=20 | Oaxaca               | primera elección con reelección: 1jul2018
             ADVERTENCIA: Oaxaca tiene municipios que operan por usos y costumbres
             indígenas. Filtrar usando lista INEGI de municipios con SCAA antes de
             incluir Oaxaca en la estimación.
  edon=22 | Querétaro            | primera elección con reelección: 1jul2018
  edon=23 | Quintana Roo         | primera elección con reelección: 1jul2018
  edon=24 | San Luis Potosí      | primera elección con reelección: 1jul2018
  edon=25 | Sinaloa              | primera elección con reelección: 1jul2018
  edon=27 | Tabasco              | primera elección con reelección: 1jul2018
  edon=28 | Tamaulipas           | primera elección con reelección: 1jul2018
  edon=31 | Yucatán              | primera elección con reelección: 1jul2018
  edon=32 | Zacatecas            | primera elección con reelección: 1jul2018

</grupo>

<grupo id="early_adopter_2019" cohort_g="2019" descripcion="3 estados con ciclo electoral bienal impar">

  edon=1  | Aguascalientes | primera elección con reelección: 2jun2019
  edon=2  | Baja California| primera elección con reelección: 2jun2019
  edon=10 | Durango        | primera elección con reelección: 2jun2019

  Nota: Estos estados adoptaron la reelección junto con los de 2018 (reforma
  constitucional federal de 2014), pero su siguiente elección municipal ocurrió
  en 2019 porque sus ciclos electorales son bianuales en años impares. La fecha
  de adopción institucional es similar a 2018; yr1st=2019 refleja cuándo pudieron
  EJERCER la reelección por primera vez, no cuándo la legislaron.
  Decisión de diseño: pueden agruparse con cohort_g=2018 o tratarse como cohorte
  separada. Preferible mantenerlos separados (cohort_g=2019) y verificar robustez.

</grupo>

<grupo id="late_adopter_2021" cohort_g="2021" descripcion="2 estados (excl. CDMX)">

  edon=21 | Puebla  | primera elección con reelección: 6jun2021
  edon=26 | Sonora  | primera elección con reelección: 6jun2021

  Nota: Estos estados tuvieron elecciones en 2018 pero sin reelección (coincidió
  con la elección de AMLO), y adoptaron la reelección para 2021. Solo hay 2 estados
  en esta cohorte (excluyendo CDMX), lo cual genera problemas de inferencia con
  pocos tratados. En Callaway-Sant'Anna, este grupo tendrá estimados ATT con
  alta varianza. Verificar con Conley-Taber (2011) para esta cohorte.

</grupo>

<grupo id="very_late_adopters" cohort_g="NA_o_2024" descripcion="Tlaxcala (2024) y Nayarit (2024?)">

  edon=29 | Tlaxcala | yr1st=2024 | Solo 1 ciclo electoral en muestra
  edon=18 | Nayarit  | yr1st=2024?| Adopción incierta — Magar usa "?"

  Decisión de diseño: Excluir de la estimación principal. Pueden incluirse en
  análisis de sensibilidad o tratarse como never-treated en la especificación
  conservadora. Si se incluyen, usar cohort_g=2024 para Tlaxcala.

</grupo>

<grupo id="excluir" cohort_g="EXCLUIR" descripcion="CDMX — no tiene municipios">

  edon=9  | Ciudad de México | yr1st=2021 | EXCLUIR: tiene alcaldías, no municipios.
             Las alcaldías de CDMX no son comparables con municipios del resto del país.

</grupo>

</clasificacion_para_did>

---

<errores_en_documento_avance>

CORRECCIONES CRÍTICAS respecto al documento_avance_extendido.md:

ERROR 1 — Clasificación incorrecta de Yucatán, Tlaxcala y Nayarit:
  El avance dice: "Nayarit, Tlaxcala y Yucatán en 2021"
  Los datos de Magar muestran:
    - Yucatán (edon=31): yr1st=2018 (pertenece a la cohorte 2018, NO 2021)
    - Tlaxcala (edon=29): yr1st=2024 (very late adopter, NO 2021)
    - Nayarit (edon=18): yr1st=2024? (incierto, NO 2021)
  IMPACTO: La cohorte 2021 tiene 2 estados (Puebla y Sonora), NO 3 como
  se pensaba. La cohorte 2018 tiene 22 estados, NO 21.

ERROR 2 — Descripción de never-treated:
  El avance dice: "Hidalgo y Veracruz quedan pendientes"
  Más preciso: son never-treated en reelección CONSECUTIVA (nTerms=1).
  Sí permiten reelección no consecutiva. Esto es relevante para el mecanismo.

ERROR 3 — CDMX como unidad de análisis:
  CDMX debe excluirse explícitamente desde la limpieza (no solo mencionarse).

IMPLICACIÓN PARA EL DISEÑO:
  Con la clasificación correcta:
  - never-treated: 2 estados (Hidalgo, Veracruz)
  - cohort_g=2018: 22 estados
  - cohort_g=2019: 3 estados
  - cohort_g=2021: 2 estados (Puebla, Sonora)
  - excluidos del análisis principal: CDMX, Tlaxcala (2024), Nayarit (2024?)
  Total estados en análisis principal: 29 estados → ~29 clusters
  (Sin CDMX, sin Tlaxcala, sin Nayarit = 32 - 3 = 29)

</errores_en_documento_avance>

---

<codigo_r_para_crear_variable_tratamiento>

El siguiente bloque de código R lee el CSV adjunto y crea la variable cohort_g
correctamente para el paquete `did` de Callaway-Sant'Anna:

```r
# Leer tabla de adopción
adopcion <- read_csv("data/raw/fechas_adopcion_reeleccion.csv")

# Crear cohort_g: año de primera elección con reelección
# 0 = never-treated (para Callaway-Sant'Anna, gname=0 indica control permanente)
adopcion <- adopcion |>
  mutate(
    cohort_g = case_when(
      grupo_tratamiento == "never_treated"     ~ 0L,
      grupo_tratamiento == "early_adopter_2018" ~ 2018L,
      grupo_tratamiento == "early_adopter_2019" ~ 2019L,
      grupo_tratamiento == "late_adopter_2021"  ~ 2021L,
      TRUE                                      ~ NA_integer_  # Excluidos
    )
  )

# Merge con el panel principal
base_panel <- base_panel |>
  left_join(adopcion |> select(edon, cohort_g, grupo_tratamiento, notas),
            by = "edon") |>
  # Excluir CDMX, Tlaxcala, Nayarit, y observaciones sin cohort_g
  filter(!is.na(cohort_g))

# Verificar N por cohorte (debe coincidir con el codebook)
base_panel |>
  distinct(edon, cohort_g) |>
  count(cohort_g) |>
  print()
# Resultado esperado:
#   cohort_g   n
#   0          2   (Hidalgo, Veracruz)
#   2018       22
#   2019       3
#   2021       2
```

</codigo_r_para_crear_variable_tratamiento>

---

<preguntas_que_haran_los_evaluadores>

Magar preguntará probablemente: "¿Por qué usas yr1st como cohort_g y no el año
de la modificación constitucional local?" La respuesta: yr1st es el año en que
el tratamiento tuvo efecto electoral real — lo que nos importa para el mecanismo
disciplinario (Ferraz & Finan 2011). La reforma constitucional federal fue en 2014,
pero los incumbentes no respondían a ella hasta que su primera reelección posible
se acercaba en el ciclo electoral.

Lucardi preguntará: "¿Qué haces con los estados de 2019? ¿Los tratas igual que
2018?" Respuesta: en la especificación principal los mantenemos separados
(cohort_g=2019). En la robustez los agrupamos con 2018 como "adoptantes tempranos".

Simpser preguntará: "¿Por qué excluyes Tlaxcala?" Respuesta: con yr1st=2024, el
estado solo tiene 1 ciclo electoral con reelección en la muestra (la elección de
junio 2024). Un solo periodo post-tratamiento no nos permite distinguir el efecto
del tratamiento de shocks idiosincráticos de ese año. Lo incluimos en análisis
de sensibilidad.

</preguntas_que_haran_los_evaluadores>

---

*Archivo generado a partir de: fechasEleccionesMexicoDesde1994.xlsx (Magar)*
*Proyecto: Reelección Municipal y Ventaja del Incumbente — ITAM Primavera 2026*
