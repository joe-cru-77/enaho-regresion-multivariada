# =====================================================================
#  00_config.R — configuración común del pipeline
#  Todas las páginas .qmd cargan este archivo al inicio.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)        # rutas relativas a la raíz del proyecto (funciona en cualquier PC)
})

# ---- Rutas (relativas al proyecto: no hay rutas de un computador en particular) ----
rutas <- list(
  raw       = here("datos", "raw"),         # CSV originales del INEI (no se suben a GitHub)
  interim   = here("datos", "interim"),     # base unida por año (.rds)
  processed = here("datos", "processed"),   # base_modelo para el modelo (se publica en Zenodo)
  tablas    = here("outputs", "tablas"),
  figuras   = here("outputs", "figuras"),
  modelos   = here("outputs", "modelos")
)
walk(rutas, dir.create, recursive = TRUE, showWarnings = FALSE)

# ---- Parámetros del estudio ----
anios   <- 2014:2025
llaves  <- c("conglome", "vivienda", "hogar", "codperso")
semilla <- 2026                 # misma semilla en todo el pipeline = resultados reproducibles

archivo_base_modelo <- file.path(rutas$processed, "base_modelo.csv.gz")

# Variables objetivo y predictoras del modelo
objetivos <- c("y1_ln_ingreso_mensual", "y2_horas_semanales")
predictoras <- c("mujer", "edad", "edad2", "anios_educ", "lengua_nativa", "etnia",
                 "casado_conviviente", "jefe_hogar", "tam_hogar", "urbano", "region",
                 "cat_ocupacional", "sector_publico", "tam_empresa", "contrato",
                 "afiliado_pensiones", "rama", "grupo_ocupacional", "anio")

# ---- Utilidades ----
fmt <- function(x, d = 0) format(round(x, d), big.mark = ",", scientific = FALSE, trim = TRUE)

# Guarda una tabla de resultados en outputs/tablas y la devuelve
guardar_tabla <- function(df, nombre) {
  write_csv(df, file.path(rutas$tablas, paste0(nombre, ".csv")))
  invisible(df)
}

theme_set(theme_minimal(base_size = 11))
