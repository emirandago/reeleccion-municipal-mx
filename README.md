# El alcalde en el umbral

**Reelección consecutiva y (des)ventaja del partido incumbente en los municipios mexicanos: evidencia de un diseño de diferencias en discontinuidades**

Emiliano Miranda González · Instituto Tecnológico Autónomo de México (ITAM), Departamento Académico de Ciencia Política · `emiliano.miranda@itam.mx`

---

## Resumen

¿Atenuó el restablecimiento de la reelección consecutiva municipal (reforma constitucional de 2014) la desventaja electoral del partido incumbente en México? Este repositorio contiene los datos procesables, el código de replicación, el documento y las diapositivas del trabajo. La estrategia de identificación es un **doble diferencial sobre cuatro discontinuidades** (*difference-in-discontinuities*; Grembi, Nannicini y Troiano 2016; Picchetti, Pinto y Shinoki 2026) que combina un RDD de elección cerrada con la adopción escalonada de la reforma, usando a Hidalgo y Veracruz como estados nunca tratados. El hallazgo central es que el efecto institucional de la reelegibilidad —neto de la autoselección del alcalde— no se identifica con precisión útil en la ventana 2018–2024, por descansar el contrafactual en solo dos estados de control.

## Estructura del repositorio

```
reeleccion-municipal-mx/
├── README.md                  Este archivo
├── LICENSE                    Licencia (MIT para el código)
├── CITATION.cff               Cómo citar el trabajo
├── reeleccion-municipal-mx.Rproj
├── scripts/                   Código de replicación en R
│   ├── 00_master.R            Orquesta todo el pipeline
│   ├── 01_limpieza.R          Construcción del panel (raw → panel apilado)
│   ├── 02_dindisc_todas.R     Doble diferencial agregado
│   ├── 03_dindisc_partido.R   Doble diferencial por partido (PRI, PAN, PRD)
│   ├── 03b_dindisc_morena.R   Exploración de MORENA
│   ├── 04_continuidad.R       Densidad (McCrary) y balance de covariables
│   └── 05_apendice_figuras_tablas.R   Figuras y tablas del apéndice
├── data/
│   ├── raw/                   Insumos (ver data/raw/README.md)
│   ├── clean/                 Paneles procesados (se regeneran con 01)
│   └── codebooks/             Codebooks de las bases
├── output/
│   ├── figures/               Figuras del apéndice (.pdf)
│   ├── tables/                Tablas del apéndice (.docx y .tex)
│   └── results/               Objetos .rds intermedios (se regeneran)
├── paper/                     Documento final (.pdf y .docx)
├── slides/                    Diapositivas (Beamer .tex y .pdf)
└── docs/
    └── REFERENCES.md          Bibliografía completa (APSR)
```

## Cómo replicar

Requisitos: **R ≥ 4.3** y los paquetes `tidyverse`, `fixest`, `rdrobust`, `rddensity`, `fwildclusterboot`, `patchwork`, `flextable`, `officer`. Nota: `fwildclusterboot` fue archivado de CRAN; se instala desde R-universe (los scripts lo hacen automáticamente en la primera corrida).

1. Obtén la base seccional `data/raw/all_states_almost.csv` 
2. Abre `reeleccion-municipal-mx.Rproj` en RStudio (fija el directorio de trabajo en la raíz del proyecto).
3. Ejecuta el pipeline completo, en orden, los scripts `01` → `02` → `03` → `03b` → `04` → `05`.

## Diseño empírico (en breve)

- **Variable dependiente:** fracción de voto válido del partido focal (`outcome_rdd`).
- **Variable de asignación:** margen de victoria municipal firmado (`margin_signed`), umbral c = 0.
- **Tratamiento:** reelegibilidad vigente (`reforma_st`), por adopción escalonada estatal.
- **Estimador:** τ = [β(tratados, post) − β(tratados, pre)] − [β(control, post) − β(control, pre)], recuperado como el coeficiente D×POST×TR de una regresión local-lineal saturada (kernel triangular, ancho de banda MSE-óptimo; Calonico, Cattaneo y Titiunik 2014).
- **Inferencia:** conglomerados por estado (~29) y *wild cluster bootstrap* Rademacher (MacKinnon, Nielsen y Webb 2023).
