# =====================================================================
#  03_funciones_modelado.R — modelos predictivos y métricas (tidymodels)
# =====================================================================

suppressPackageStartupMessages(library(tidymodels))
tidymodels_prefer()

# Tamaños de muestra para que el pipeline termine en un tiempo razonable
n_validacion_cruzada <- 50000    # filas del entrenamiento usadas en la validación cruzada
n_entrenamiento_rf   <- 100000   # filas para el ajuste final del Random Forest
v_folds              <- 5

# Métricas de validación (yardstick)
metricas <- metric_set(rmse, rsq, mae)

# Lee base_modelo con los tipos correctos
leer_base_modelo <- function() {
  read_csv(archivo_base_modelo, show_col_types = FALSE) %>%
    mutate(across(c(etnia, region, cat_ocupacional, tam_empresa, contrato, rama,
                    grupo_ocupacional), as.factor))
}

# Receta de preprocesamiento para un objetivo:
#  - el año se trata como categoría (efecto propio de cada año, p. ej. 2020)
#  - variables categóricas -> variables indicadoras (dummies)
#  - se quitan columnas sin variación y se estandarizan las numéricas
receta <- function(objetivo, datos) {
  recipe(as.formula(paste(objetivo, "~ .")),
         data = select(datos, all_of(c(objetivo, predictoras)))) %>%
    step_mutate(anio = as.character(anio)) %>%
    step_string2factor(anio) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
}

# Modelos que se comparan
modelos <- list(
  lineal        = linear_reg() %>% set_engine("lm"),
  elastic_net   = linear_reg(penalty = 0.001, mixture = 0.5) %>% set_engine("glmnet"),
  random_forest = rand_forest(trees = 300, min_n = 20) %>%
    set_engine("ranger", importance = "impurity", num.threads = parallel::detectCores()) %>%
    set_mode("regression")
)

# Partición entrenamiento/prueba (80/20). Con la misma semilla, el modelado
# y la etapa de MLOps obtienen exactamente la misma partición.
particionar <- function(datos) {
  set.seed(semilla)
  particion <- initial_split(datos, prop = 0.8)
  list(entrenamiento = training(particion), prueba = testing(particion))
}

# Ajusta un modelo (workflow = receta + modelo) para un objetivo con los datos de entrenamiento
ajustar_workflow <- function(objetivo, modelo, entrenamiento) {
  d_entr <- select(entrenamiento, all_of(c(objetivo, predictoras)))
  if (modelo == "random_forest" && nrow(d_entr) > n_entrenamiento_rf) {
    set.seed(semilla)
    d_entr <- slice_sample(d_entr, n = n_entrenamiento_rf)
  }
  workflow() %>%
    add_recipe(receta(objetivo, d_entr)) %>%
    add_model(modelos[[modelo]]) %>%
    fit(d_entr)
}

# Fórmula del modelo multivariado clásico: las 2 variables objetivo a la vez
formula_multivariada <- as.formula(paste(
  "cbind(", paste(objetivos, collapse = ", "), ") ~",
  paste(setdiff(predictoras, "anio"), collapse = " + "), "+ factor(anio)"))
