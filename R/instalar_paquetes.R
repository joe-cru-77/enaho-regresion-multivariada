# =====================================================================
#  instalar_paquetes.R — instala los paquetes del proyecto
#  Ejecutar una sola vez. Para fijar las versiones exactas se usa renv
#  (ver README: renv::init() y renv::snapshot()).
# =====================================================================

paquetes <- c(
  "tidyverse",   # readr, dplyr, tidyr, purrr, stringr, ggplot2
  "here",        # rutas relativas al proyecto
  "tidymodels",  # modelos predictivos y validación (parsnip, recipes, rsample, yardstick, tune)
  "glmnet",      # regresión regularizada (Ridge / Lasso / Elastic Net)
  "ranger",      # Random Forest
  "broom",       # coeficientes en formato tabla
  "survey",      # factores de expansión y diseño muestral de la ENAHO
  "sampleSelection", # modelo de Heckman con errores estándar corregidos
  "cluster",     # coeficiente de silueta para elegir el número de grupos
  "knitr",       # tablas en las páginas
  "vetiver", "pins", "plumber",   # MLOps: registro de modelos, versiones y API
  "lubridate",                    # fechas para el monitoreo
  "DBI", "RPostgres", "dbplyr",   # despliegue en PostgreSQL
  "renv"         # reproducibilidad del entorno
)

faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) install.packages(faltan)
message("Paquetes listos: ", paste(paquetes, collapse = ", "))
