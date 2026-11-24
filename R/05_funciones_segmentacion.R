# =====================================================================
#  05_funciones_segmentacion.R — variables de contexto y segmentos:
#  departamento, empleo formal/informal, rubro económico y grupos de
#  departamentos similares (clustering)
# =====================================================================

archivo_base_contexto <- file.path(rutas$processed, "base_contexto.csv.gz")

nombres_departamento <- c(
  "01" = "Amazonas", "02" = "Áncash", "03" = "Apurímac", "04" = "Arequipa",
  "05" = "Ayacucho", "06" = "Cajamarca", "07" = "Callao", "08" = "Cusco",
  "09" = "Huancavelica", "10" = "Huánuco", "11" = "Ica", "12" = "Junín",
  "13" = "La Libertad", "14" = "Lambayeque", "15" = "Lima", "16" = "Loreto",
  "17" = "Madre de Dios", "18" = "Moquegua", "19" = "Pasco", "20" = "Piura",
  "21" = "Puno", "22" = "San Martín", "23" = "Tacna", "24" = "Tumbes", "25" = "Ucayali")

# ---------------------------------------------------------------------
# 1. Base de contexto: departamento (de ubigeo)
#    La variable oficial de informalidad del INEI (ocupinf) no está entre las 911
#    variables comunes a los 12 años, así que la informalidad se aproxima con
#    variables de base_modelo (ver informalidad_aprox()).
# ---------------------------------------------------------------------
construir_base_contexto <- function(unida) {
  if (!"ubigeo" %in% names(unida))
    stop("La base unida no tiene ubigeo. Revisa que esté entre las 911 variables comunes.")
  unida %>%
    llaves_numericas() %>%
    transmute(across(all_of(llaves_persona)),
              departamento = str_sub(str_pad(as.character(as.integer(as.numeric(ubigeo))), 6, pad = "0"), 1, 2)) %>%
    filter(departamento %in% names(nombres_departamento)) %>%
    distinct(across(all_of(llaves_persona)), .keep_all = TRUE)
}

leer_base_contexto <- function() {
  read_csv(archivo_base_contexto, show_col_types = FALSE,
           col_types = cols(departamento = col_character()))
}

agregar_contexto <- function(datos, contexto) {
  datos %>%
    llaves_numericas() %>%
    inner_join(contexto, by = llaves_persona) %>%
    mutate(nombre_dep = unname(nombres_departamento[departamento]))
}

# Empleo informal de un asalariado (aproximación al criterio del INEI para
# trabajadores dependientes): sin contrato o sin aporte a un sistema de pensiones.
# Las FF.AA. y la PNP se consideran formales.
informalidad_aprox <- function(contrato, afiliado_pensiones) {
  contrato <- as.character(contrato)
  case_when(contrato == "ffaa_pnp" ~ 0L,
            contrato == "sin_contrato" | afiliado_pensiones == 0 ~ 1L,
            TRUE ~ 0L)
}

# ---------------------------------------------------------------------
# 2. Rubro económico: agrupación de las ramas de actividad
# ---------------------------------------------------------------------
rubro_economico <- function(rama) {
  rama <- as.character(rama)
  case_when(
    rama == "A_agropecuario" ~ "Agropecuario",
    rama == "B_mineria"      ~ "Minería",
    rama %in% c("C_manufactura", "DE_electricidad_agua", "F_construccion") ~ "Industria y construcción",
    rama == "G_comercio"     ~ "Comercio",
    rama %in% c("O_administracion_publica", "P_ensenanza", "Q_salud") ~ "Adm. pública, educación y salud",
    TRUE                     ~ "Otros servicios"
  )
}

# ---------------------------------------------------------------------
# 3. Heckman por segmento
#    - si el segmento es geográfico, la etapa 1 se estima dentro del segmento
#    - si no (formalidad, rubro, grupo), se usa la λ nacional, porque la etapa 1
#      modela la decisión de ser asalariado, no la de entrar a un rubro
# ---------------------------------------------------------------------
terminos_segmento <- c("anios_educ", "mujer", "sector_publico", "afiliado_pensiones", "lambda")

heckman_por_segmento <- function(base, variable, lambda_nacional = NULL, seleccion = NULL,
                                 excluir = character(), min_n = 2000) {
  niveles <- base %>% count(.data[[variable]]) %>% filter(n >= min_n) %>% pull(1)
  map_dfr(set_names(as.character(niveles)), function(nivel) {
    b <- filter(base, .data[[variable]] == nivel)
    lam <- if (!is.null(seleccion)) {
      etapa1_probit(filter(seleccion, .data[[variable]] == nivel), excluir)$lambda
    } else lambda_nacional
    e2 <- etapa2_multivariada(b, lam, excluir = excluir)
    coeficientes_mv(e2$heckman, "Heckman") %>%
      filter(termino %in% terminos_segmento) %>%
      mutate(n_asalariados = nrow(e2$datos))
  }, .id = "segmento") %>%
    mutate(tipo_segmento = variable)
}

# Pasa los coeficientes de Y1 a % de cambio del ingreso (Y2 queda en horas)
a_efectos <- function(coefs) {
  coefs %>%
    mutate(transformar = objetivo == objetivos[1] & termino != "lambda",
           across(c(estimacion, ic_inf, ic_sup), ~ if_else(transformar, 100 * (exp(.x) - 1), .x))) %>%
    select(-transformar)
}

# ---------------------------------------------------------------------
# 4. Grupos de departamentos similares (k-means)
# ---------------------------------------------------------------------
perfil_departamentos <- function(diseno) {
  svyby(~ingreso_mensual + horas + informal + agro + anios_educ + urbano + sector_publico + mujer,
        ~departamento, diseno, svymean, na.rm = TRUE) %>%
    as_tibble() %>%
    select(departamento, ingreso_mensual, horas, informal, agro, anios_educ,
           urbano, sector_publico, mujer) %>%
    mutate(nombre_dep = unname(nombres_departamento[departamento]))
}

agrupar_departamentos <- function(perfil, k_max = 6) {
  x <- perfil %>%
    select(ingreso_mensual, horas, informal, agro, anios_educ, urbano, sector_publico) %>%
    mutate(ingreso_mensual = log(ingreso_mensual)) %>%
    scale()
  distancias <- dist(x)
  set.seed(semilla)
  candidatos <- map(2:k_max, ~ kmeans(x, centers = .x, nstart = 100))
  silueta <- map_dbl(candidatos, ~ mean(cluster::silhouette(.x$cluster, distancias)[, "sil_width"]))
  mejor <- which.max(silueta)
  km <- candidatos[[mejor]]
  pca <- prcomp(x)
  list(
    k = mejor + 1,
    siluetas = tibble(k = 2:k_max, silueta_media = silueta),
    asignacion = perfil %>%
      mutate(grupo = paste("Grupo", km$cluster),
             pc1 = pca$x[, 1], pc2 = pca$x[, 2]),
    varianza_pca = summary(pca)$importance[2, 1:2]
  )
}

# ---------------------------------------------------------------------
# 5. Random Forest: un modelo para todo el Perú frente a uno por segmento
# ---------------------------------------------------------------------
rf_por_segmento <- function(particion, variable, vars_globales, excluir_local, n_max_local = n_rf_region) {
  niveles <- sort(unique(as.character(particion$prueba[[variable]])))
  map_dfr(set_names(objetivos), function(obj) {
    global <- ajustar_rf(obj, particion$entrenamiento, vars_globales, n_entrenamiento_rf)
    map_dfr(set_names(niveles), function(nivel) {
      prueba_s <- filter(particion$prueba, .data[[variable]] == nivel)
      entr_s   <- filter(particion$entrenamiento, .data[[variable]] == nivel)
      vars_local <- quitar_constantes(setdiff(vars_globales, excluir_local), entr_s)
      local <- ajustar_rf(obj, entr_s, vars_local, n_max_local)
      bind_rows(
        predict(global, prueba_s) %>% mutate(real = prueba_s[[obj]], enfoque = "Un modelo para el Perú"),
        predict(local,  prueba_s) %>% mutate(real = prueba_s[[obj]], enfoque = "Un modelo por segmento")
      ) %>%
        group_by(enfoque) %>%
        metricas(truth = real, estimate = .pred) %>%
        select(enfoque, metrica = .metric, valor = .estimate)
    }, .id = "segmento")
  }, .id = "objetivo") %>%
    mutate(tipo_segmento = variable)
}
