# Regresión multivariada con dos variables objetivo – ENAHO 2014–2025

Pipeline reproducible en **Quarto** y **R** que integra los microdatos de la Encuesta Nacional de Hogares (ENAHO) del INEI, construye una base limpia de asalariados y compara modelos predictivos para dos variables objetivo:

- **Y1:** logaritmo del ingreso mensual de la ocupación principal.
- **Y2:** horas trabajadas a la semana.

Usa 19 predictoras: personales, del hogar, de la ubicación y del empleo.

Universidad Nacional del Altiplano – Puno · Escuela Profesional de Ingeniería Estadística e Informática · Curso: Ciencia de Datos

## Estructura

```
├── _quarto.yml               configuración del proyecto
├── index.qmd                 objetivo y arquitectura del pipeline
├── 01-integracion.qmd        inventario de variables y unión de módulos 200, 300 y 500
├── 02-limpieza.qmd           faltantes, pases y construcción de base_modelo
├── 03-exploracion.qmd        análisis exploratorio
├── 04-modelado.qmd           regresión multivariada, lineal, Elastic Net y Random Forest
├── 05-validacion.qmd         RMSE, R², MAE; validación cruzada y prueba
├── 06-interpretabilidad.qmd  coeficientes, Pillai e importancia de variables
├── 07-mlops.qmd              registro y versiones de modelos (vetiver), API, monitoreo
├── 08-despliegue.qmd         carga en PostgreSQL
├── 09-disponibilidad-datos.qmd  GitHub (código) y Zenodo (datos, DOI)
├── dashboard.qmd             tablero de resultados
├── R/                        funciones del pipeline
├── datos/                    raw / interim / processed (no se suben a GitHub)
├── outputs/                  tablas, figuras y registro de modelos
├── api/                      APIs de predicción (plumber)
├── .github/workflows/        publicación automática en GitHub Pages
├── LISTA_REPRODUCIBILIDAD.md lista de actividades de reproducibilidad
└── CITATION.cff              cómo citar
```

## Cómo reproducir

Requisitos: **R ≥ 4.2**, **Quarto ≥ 1.4** y RStudio (recomendado).

```bash
git clone https://github.com/joe-cru-77/enaho-regresion-multivariada.git
cd enaho-regresion-multivariada
```

En R:

```r
install.packages("renv")
renv::restore()                 # instala las mismas versiones de paquetes (renv.lock)
# o, si aún no hay renv.lock:
source("R/instalar_paquetes.R")
```

Datos, con una de estas dos opciones:

- **Opción A (rápida):** descargar `base_modelo.csv.gz` desde Zenodo (DOI: `10.5281/zenodo.XXXXXXX`) y copiarlo en `datos/processed/`.
- **Opción B (desde cero):** descargar del portal de microdatos del INEI los módulos 200, 300 y 500 de 2014 a 2025 (CSV) y copiar las carpetas de cada año en `datos/raw/`.

Ejecutar el pipeline completo:

```bash
quarto render
```

El sitio se genera en `docs/` y se puede ver en `docs/index.html` o publicar con GitHub Pages.

### Despliegue en PostgreSQL (opcional)

Crear un archivo `.Renviron` en la raíz del proyecto (no se sube a GitHub):

```
PGHOST=localhost
PGPORT=5432
PGUSER=postgres
PGPASSWORD=tu_contraseña
PGDATABASE=enaho
```

## Disponibilidad de datos

- **Datos:** Zenodo, DOI `10.5281/zenodo.XXXXXXX` (completar al publicar).
- **Código:** este repositorio; versión archivada en Zenodo con DOI `10.5281/zenodo.YYYYYYY`.
- **Fuente original:** INEI, Encuesta Nacional de Hogares 2014–2025.

## Licencia

Código: MIT. Datos procesados: CC BY 4.0 (citar también al INEI como fuente original).
