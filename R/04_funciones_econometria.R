# =====================================================================
#  04_funciones_econometria.R — modelo bietápico de Heckman,
#  análisis por región natural y diseño muestral de la ENAHO
# =====================================================================

suppressPackageStartupMessages({
  library(broom)
  library(survey)          # factores de expansión y diseño muestral
})
options(survey.lonely.psu = "adjust")   # estratos con un solo conglomerado

llaves_persona <- c("anio", "conglome", "vivienda", "hogar", "codperso")

archivo_base_seleccion <- file.path(rutas$processed, "base_seleccion.csv.gz")

# Tamaño de la submuestra para los errores estándar corregidos de heckit()
n_heckit <- 300000
# Tamaño máximo de entrenamiento del Random Forest de cada región
n_rf_region <- 60000

etiqueta_region <- c(costa = "Costa (sin Lima)", sierra = "Sierra", selva = "Selva",
                     lima_metropolitana = "Lima Metropolitana")

# ---------------------------------------------------------------------
# 1. Base de selección: todas las personas de 14 años a más con datos
#    del módulo de empleo, asalariadas o no
# ---------------------------------------------------------------------
llaves_numericas <- function(df) mutate(df, across(any_of(llaves_persona), as.numeric))

construir_base_seleccion <- function(unida, base_modelo) {
  unida <- llaves_numericas(unida)

  # Composición del hogar (solo miembros del hogar, p204 = 1)
  hogares <- unida %>%
    filter(p204 == 1) %>%
    group_by(anio, conglome, vivienda, hogar) %>%
    summarise(tam_hogar   = n(),
              n_menores6  = sum(p208a < 6, na.rm = TRUE),
              n_mayores65 = sum(p208a >= 65 & p208a < 99, na.rm = TRUE),
              .groups = "drop")

  en_modelo <- base_modelo %>%
    llaves_numericas() %>%
    distinct(across(all_of(llaves_persona))) %>%
    mutate(asalariado = 1L)

  unida %>%
    filter(!is.na(ocu500), !is.na(p208a), p208a >= 14, p208a != 99,
           !is.na(p301a), !p301a %in% c(12, 99),
           !is.na(p300a), !p300a %in% c(9, 99),
           !is.na(p558c), !is.na(p209)) %>%
    inner_join(hogares, by = c("anio", "conglome", "vivienda", "hogar")) %>%
    left_join(en_modelo, by = llaves_persona) %>%
    mutate(asalariado = coalesce(asalariado, 0L)) %>%
    # asalariados ocupados que no entraron a base_modelo por datos faltantes:
    # no se sabe si "trabajan como asalariados con datos completos", se excluyen
    filter(!(asalariado == 0L & ocu500 %in% 1 & p507 %in% c(3, 4, 6))) %>%
    transmute(
      across(all_of(llaves_persona)), fac500a, asalariado,
      mujer              = as.integer(p207 == 2),
      edad               = p208a,
      edad2              = p208a^2,
      anios_educ         = anios_educacion(p301a, p301b, p301c),
      lengua_nativa      = as.integer(p300a %in% c(1, 2, 3, 10:15)),
      etnia              = case_when(p558c %in% c(1, 2, 3, 9) ~ "indigena",
                                     p558c == 4 ~ "afro",
                                     p558c == 5 ~ "blanco",
                                     p558c == 6 ~ "mestizo",
                                     TRUE       ~ "otro_ns"),
      casado_conviviente = as.integer(p209 %in% c(1, 2)),
      jefe_hogar         = as.integer(p203 %in% 1),
      tam_hogar, n_menores6, n_mayores65,
      urbano             = as.integer(estrato %in% 1:5),
      region             = case_when(dominio %in% 1:3 ~ "costa",
                                     dominio %in% 4:6 ~ "sierra",
                                     dominio == 7     ~ "selva",
                                     TRUE             ~ "lima_metropolitana")
    ) %>%
    filter(!is.na(anios_educ))
}

leer_base_seleccion <- function() {
  read_csv(archivo_base_seleccion, show_col_types = FALSE) %>%
    mutate(across(c(etnia, region), as.factor))
}

# ---------------------------------------------------------------------
# 2. Modelo bietápico de Heckman
#    Etapa 1: probit de la probabilidad de ser asalariado
#    Etapa 2: regresión multivariada (ingreso y horas) + razón inversa de Mills
# ---------------------------------------------------------------------
vars_seleccion   <- c("mujer", "edad", "edad2", "anios_educ", "lengua_nativa", "etnia",
                      "urbano", "region")
# Restricciones de exclusión: afectan la decisión de trabajar como asalariado,
# pero no el ingreso ni las horas de quien ya trabaja. Siguen la especificación
# habitual de la literatura: composición del hogar y posición en él.
# Con solo n_menores6 y n_mayores65 la identificación era débil: λ quedaba casi
# colineal con la educación, el sexo y la edad, y absorbía sus efectos.
vars_exclusion   <- c("n_menores6", "n_mayores65", "casado_conviviente", "jefe_hogar", "tam_hogar")
# Por eso esas variables NO entran en la ecuación de resultados del modelo de Heckman
excluir_resultado_heckman <- c("casado_conviviente", "jefe_hogar", "tam_hogar")

# Quita las predictoras que no varían dentro de un subgrupo (p. ej. la región
# dentro de una región o la rama dentro de un rubro con una sola rama)
quitar_constantes <- function(vars, datos) {
  vars[map_lgl(vars, ~ .x %in% names(datos) && n_distinct(datos[[.x]], na.rm = TRUE) > 1)]
}

formula_seleccion <- function(datos, excluir = character()) {
  v <- quitar_constantes(setdiff(vars_seleccion, excluir), datos)
  as.formula(paste("asalariado ~", paste(c(v, vars_exclusion), collapse = " + "),
                   "+ mujer:n_menores6 + factor(anio)"))
}

formula_resultado <- function(datos, excluir = character(), extra = NULL) {
  v <- quitar_constantes(setdiff(predictoras, c("anio", excluir)), datos)
  as.formula(paste("cbind(", paste(objetivos, collapse = ", "), ") ~",
                   paste(c(v, "factor(anio)", extra), collapse = " + ")))
}

# Razón inversa de Mills, calculada en escala logarítmica para evitar errores numéricos
razon_mills <- function(xb) exp(dnorm(xb, log = TRUE) - pnorm(xb, log.p = TRUE))

# Etapa 1. Devuelve los coeficientes, los efectos marginales de las restricciones
# de exclusión y la razón inversa de Mills de cada persona
etapa1_probit <- function(seleccion, excluir = character()) {
  probit <- glm(formula_seleccion(seleccion, excluir), data = seleccion,
                family = binomial(link = "probit"), model = FALSE)
  xb <- probit$linear.predictors
  b  <- coef(probit)
  dens <- dnorm(xb)
  es_mujer <- seleccion$mujer == 1
  ame <- tibble(
    variable = c("n_menores6 (hombres)", "n_menores6 (mujeres)", "n_mayores65",
                 "casado_conviviente", "jefe_hogar", "tam_hogar"),
    efecto_marginal = unname(c(mean(dens[!es_mujer]) * b["n_menores6"],
                               mean(dens[es_mujer]) * (b["n_menores6"] + b["mujer:n_menores6"]),
                               mean(dens) * b[c("n_mayores65", "casado_conviviente",
                                                "jefe_hogar", "tam_hogar")])))
  list(
    coeficientes = tidy(probit),
    efectos_exclusion = ame,
    lambda = seleccion %>%
      select(all_of(llaves_persona)) %>%
      mutate(lambda = razon_mills(xb), prob_asalariado = pnorm(xb)),
    n = nrow(seleccion), tasa = mean(seleccion$asalariado)
  )
}

# Etapa 2. Regresión multivariada con y sin la razón inversa de Mills
etapa2_multivariada <- function(base_modelo, lambda_df, excluir = character(), pesos = NULL) {
  excluir <- union(excluir, excluir_resultado_heckman)
  d <- base_modelo %>%
    llaves_numericas() %>%
    inner_join(lambda_df, by = llaves_persona) %>%
    droplevels()                      # al estimar por región, quita categorías vacías
  if (is.null(pesos)) {
    sin_correccion <- lm(formula_resultado(d, excluir), data = d)
    heckman        <- lm(formula_resultado(d, excluir, "lambda"), data = d)
  } else {
    # el peso va como columna de los datos: lm() lo busca ahí
    d <- d %>% filter(!is.na(.data[[pesos]])) %>% mutate(.peso = .data[[pesos]])
    sin_correccion <- lm(formula_resultado(d, excluir), data = d, weights = .peso)
    heckman        <- lm(formula_resultado(d, excluir, "lambda"), data = d, weights = .peso)
  }
  list(datos = d, sin_correccion = sin_correccion, heckman = heckman,
       r2_lambda = r2_lambda(d, excluir))
}

# Diagnóstico de identificación: R² de λ explicada por las predictoras de la ecuación
# de resultados. Cerca de 1 = λ es casi colineal con ellas y los coeficientes
# corregidos no son confiables; lejos de 1 = las restricciones de exclusión aportan
# variación propia.
r2_lambda <- function(d, excluir) {
  rhs <- deparse1(formula_resultado(d, excluir)[[3]])
  summary(lm(as.formula(paste("lambda ~", rhs)), data = d))$r.squared
}

# Coeficientes de un modelo multivariado en formato largo, con intervalo de 95 %
coeficientes_mv <- function(modelo, modelo_nombre) {
  tidy(modelo) %>%
    transmute(objetivo = response, termino = term, estimacion = estimate,
              error_est = std.error, p_valor = p.value,
              ic_inf = estimate - 1.96 * std.error, ic_sup = estimate + 1.96 * std.error,
              modelo = modelo_nombre)
}

# Prueba de Pillai (tipo I, secuencial) de un modelo multivariado
pillai_mv <- function(modelo) {
  anova(modelo) %>%
    as_tibble(rownames = "termino") %>%
    filter(!termino %in% c("(Intercept)", "Residuals"))
}

# Verificación con errores estándar corregidos (sampleSelection::heckit), por objetivo,
# en una submuestra para limitar la memoria
heckit_verificacion <- function(seleccion, base_modelo) {
  if (!requireNamespace("sampleSelection", quietly = TRUE)) return(NULL)
  set.seed(semilla)
  sub <- seleccion %>%
    slice_sample(n = min(n_heckit, nrow(seleccion))) %>%
    left_join(base_modelo %>% llaves_numericas() %>%
                select(all_of(llaves_persona), all_of(objetivos), cat_ocupacional,
                       sector_publico, tam_empresa, contrato, afiliado_pensiones, rama,
                       grupo_ocupacional),
              by = llaves_persona) %>%
    mutate(across(where(is.character), as.factor), anio = factor(anio),
           asalariado = asalariado == 1L)

  # Matriz de la ecuación de resultados sin columnas colineales: por ejemplo, la
  # categoría FF.AA./PNP aparece a la vez en tam_empresa y en contrato. lm() descarta
  # esas columnas en silencio, pero heckit() falla ("system is exactly singular").
  x_res <- paste(setdiff(predictoras, c("anio", excluir_resultado_heckman)), collapse = " + ")
  sel <- sub$asalariado
  mm  <- model.matrix(as.formula(paste("~", x_res, "+ anio")), data = sub[sel, ])[, -1]
  qr_mm <- qr(mm)
  mm <- mm[, qr_mm$pivot[seq_len(qr_mm$rank)], drop = FALSE]
  colnames(mm) <- make.names(colnames(mm), unique = TRUE)
  x_ok <- matrix(NA_real_, nrow(sub), ncol(mm), dimnames = list(NULL, paste0("x_", colnames(mm))))
  x_ok[sel, ] <- mm
  sub <- bind_cols(sub, as_tibble(x_ok))

  map_dfr(set_names(objetivos), function(obj) {
    fit <- sampleSelection::heckit(
      selection = asalariado ~ mujer + edad + edad2 + anios_educ + lengua_nativa + etnia +
        casado_conviviente + jefe_hogar + tam_hogar + urbano + region +
        n_menores6 + n_mayores65 + mujer:n_menores6 + anio,
      outcome = as.formula(paste(obj, "~", paste(colnames(x_ok), collapse = " + "))),
      data = sub, method = "2step")
    b  <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    clave <- c("invMillsRatio", "rho", "sigma")
    tibble(objetivo = obj, parametro = clave,
           estimacion = unname(b[clave]), error_est = unname(se[clave]))
  }, .id = NULL)
}

# ---------------------------------------------------------------------
# 3. Diseño muestral de la ENAHO (factores de expansión)
#    Conglomerado = conglome dentro de cada año; estrato aproximado =
#    año × departamento × área (o año × región × área si no hay departamento)
# ---------------------------------------------------------------------
diseno_encuesta <- function(base_modelo) {
  con_dep <- "departamento" %in% names(base_modelo)
  d <- base_modelo %>%
    filter(!is.na(fac500a)) %>%
    mutate(ingreso_mensual = exp(y1_ln_ingreso_mensual),
           horas           = y2_horas_semanales,
           ingreso_hora    = ingreso_mensual / (horas * 52 / 12),
           jornada         = cut(horas, c(0, 34, 48, Inf),
                                 labels = c("Menos de 35 h", "35 a 48 h", "Más de 48 h")),
           psu  = paste(anio, conglome),
           estr = if (con_dep) paste(anio, departamento, urbano) else paste(anio, region, urbano))
  svydesign(ids = ~psu, strata = ~estr, weights = ~fac500a, data = d, nest = TRUE)
}

# Medias ponderadas por región, con su error estándar
medias_region <- function(diseno) {
  svyby(~ingreso_mensual + horas + ingreso_hora, ~region, diseno, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    pivot_longer(-region, names_to = "nombre", values_to = "valor") %>%
    mutate(tipo = if_else(str_starts(nombre, "se\\."), "error_est", "media"),
           variable = str_remove(nombre, "^se\\.")) %>%
    select(-nombre) %>%
    pivot_wider(names_from = tipo, values_from = valor)
}

# ---------------------------------------------------------------------
# 4. Random Forest: modelo para todo el Perú frente a un modelo por región
# ---------------------------------------------------------------------
ajustar_rf <- function(objetivo, datos, vars, n_max) {
  d <- select(datos, all_of(c(objetivo, vars)))
  if (nrow(d) > n_max) { set.seed(semilla); d <- slice_sample(d, n = n_max) }
  rec <- recipe(as.formula(paste(objetivo, "~ .")), data = d) %>%
    step_mutate(anio = as.character(anio)) %>%
    step_string2factor(anio) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  workflow() %>% add_recipe(rec) %>% add_model(modelos$random_forest) %>% fit(d)
}
