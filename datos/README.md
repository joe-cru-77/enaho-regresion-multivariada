# Datos

| Carpeta | Contenido | ¿Se sube a GitHub? |
|---|---|---|
| `raw/` | CSV originales del INEI (ENAHO, módulos 200, 300 y 500, 2014–2025). Copiar aquí las carpetas de cada año (`2014/`, `2015/`, …). | No |
| `interim/` | Base unida por año (`enaho_unida_AAAA.rds`), generada por `01-integracion.qmd`. | No |
| `processed/` | `base_modelo.csv.gz` y `diccionario_base_modelo.csv`, generados por `02-limpieza.qmd`. | No: se publican en **Zenodo** con DOI |

Fuente original: Instituto Nacional de Estadística e Informática (INEI), Encuesta Nacional de Hogares, portal de microdatos: https://proyectos.inei.gob.pe/microdatos/
