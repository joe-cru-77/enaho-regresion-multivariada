# Lista de actividades para hacer el trabajo reproducible

Proyecto: **Regresión multivariada con dos variables objetivo – ENAHO 2014–2025**

Marcar cada actividad cuando esté hecha (`[x]`).

## 1. Estructura del proyecto (Quarto)

- [ ] Proyecto Quarto creado con `_quarto.yml` en la raíz.
- [ ] Una página `.qmd` por etapa del pipeline: integración → limpieza → exploración → modelado → validación → interpretabilidad → MLOps → despliegue → disponibilidad de datos.
- [ ] Carpetas separadas: `datos/raw`, `datos/interim`, `datos/processed`, `R/`, `outputs/`.
- [ ] Funciones reutilizables en `R/` (no se repite código entre páginas).
- [ ] Rutas relativas con `here()`: ninguna ruta de un computador en particular (`C:/Users/...`).
- [ ] `quarto render` ejecuta todo el pipeline sin errores, de punta a punta.

## 2. Datos

- [ ] Los CSV originales del INEI se guardan en `datos/raw/` y **no se modifican**.
- [ ] Cada etapa lee la salida de la anterior y guarda la suya en disco.
- [ ] Inventario de variables documentado: 2 301 variables distintas, 911 presentes en los 12 años.
- [ ] Reglas de limpieza y recodificación documentadas (pases frente a faltantes reales).
- [ ] Diccionario de `base_modelo` generado (`diccionario_base_modelo.csv`).

## 3. Entorno de software

- [ ] Paquetes listados en `R/instalar_paquetes.R`.
- [ ] Versiones fijadas con **renv**: `renv::init()` y luego `renv::snapshot()` → `renv.lock` subido a GitHub.
- [ ] Semilla fija (`semilla <- 2026`) para la partición, la validación cruzada y el Random Forest.
- [ ] `execute: freeze: auto` en `_quarto.yml`, con la carpeta `_freeze/` subida a GitHub.
- [ ] Versión de R y de los paquetes anotada (`sessionInfo()`).

## 4. Modelos y validación

- [ ] Partición entrenamiento/prueba (80/20) y validación cruzada (5 folds).
- [ ] Modelos: regresión multivariada (MCO), lineal, Elastic Net y Random Forest.
- [ ] Métricas para cada variable objetivo: RMSE, R² y MAE.
- [ ] Comparación entre validación cruzada y prueba para detectar sobreajuste.
- [ ] Interpretabilidad: coeficientes, prueba de Pillai e importancia de variables.

## 5. Control de versiones (GitHub)

- [ ] Repositorio creado: `enaho-regresion-multivariada`.
- [ ] `.gitignore` excluye los datos, `.Renviron` (contraseñas) y archivos temporales.
- [ ] Commits frecuentes, con mensajes claros.
- [ ] `README.md` explica cómo reproducir el trabajo paso a paso.
- [ ] Sitio publicado con GitHub Pages (carpeta `docs/`).
- [ ] Release `v1.0.0` creada al terminar.

## 5b. Análisis econométrico

- [ ] Base de selección (`base_seleccion.csv.gz`) construida con todas las personas de 14 años a más.
- [ ] Restricciones de exclusión justificadas (`n_menores6`, `n_mayores65`).
- [ ] Modelo bietápico de Heckman estimado y verificado con `sampleSelection::heckit()`.
- [ ] Resultados ponderados con `fac500a` y el diseño muestral (`survey`).
- [ ] Variables de contexto (departamento y empleo informal) en `base_contexto.csv.gz`.
- [ ] Efectos comparados por región, departamento (Puno), formalidad, rubro y grupos de departamentos (*k-means*).
- [ ] Modelos predictivos globales frente a modelos por segmento.

## 6. Publicación de datos (DOI)

- [ ] `base_modelo.csv.gz` y su diccionario publicados en **Zenodo** (o Mendeley Data / Dataverse).
- [ ] DOI de los datos copiado en `README.md`, `CITATION.cff` y la sección *Disponibilidad de datos*.
- [ ] Repositorio de GitHub enlazado con Zenodo para que el código también tenga DOI.
- [ ] Licencias definidas: datos CC BY 4.0 y código MIT.

## 7. MLOps

- [ ] Modelo final de cada variable objetivo guardado en un registro con versiones (`vetiver` + `pins`).
- [ ] Metadatos del modelo registrados: tipo de modelo, métricas de prueba, semilla y fecha.
- [ ] API de predicción generada con `plumber` (`api/`) y probada en `http://127.0.0.1:8000/__docs__/`.
- [ ] Monitoreo del desempeño por año (`vetiver_compute_metrics`) con un criterio para reentrenar.
- [ ] GitHub Actions publica el sitio automáticamente en cada push (`.github/workflows/publicar.yml`).

## 8. Despliegue

- [ ] Credenciales de PostgreSQL en `.Renviron` (nunca en el código).
- [ ] `base_modelo` y los resultados cargados en PostgreSQL (`10-despliegue.qmd`).
- [ ] Conteo de filas verificado entre R y PostgreSQL.
- [ ] Dashboard con los resultados (`dashboard.qmd`).

## 9. Verificación final

- [ ] Otro integrante clona el repositorio en otra computadora, descarga los datos de Zenodo, ejecuta `renv::restore()` y `quarto render`, y obtiene los mismos resultados.
