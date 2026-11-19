# =====================================================================
#  02_funciones_limpieza.R — construcción de base_modelo
#  Misma lógica que la tabla base_modelo creada en PostgreSQL, pero en R
#  (así cualquiera puede reproducirla sin una base de datos).
# =====================================================================

# Columnas de la base unida que se necesitan para el modelo
columnas_modelo <- c("anio", "conglome", "vivienda", "hogar", "codperso", "fac500a", "p204",
                     "ocu500", "p507", "i524a1", "p513t", "p207", "p208a",
                     "p301a", "p301b", "p301c", "p300a", "p558c", "p209", "p203",
                     "estrato", "dominio", "p510", "p511a", "p512a",
                     "p558a1", "p558a2", "p558a3", "p558a4", "p558a5", "p505r4", "p506r4")

# Lee la base unida de todos los años (solo las columnas del modelo)
leer_base_unida <- function() {
  archivos <- file.path(rutas$interim, sprintf("enaho_unida_%d.rds", anios))
  archivos <- archivos[file.exists(archivos)]
  map_dfr(archivos, ~ read_rds(.x) %>%
            select(any_of(columnas_modelo)) %>%
            mutate(across(everything(), ~ if (is.character(.x)) .x else as.numeric(.x))))
}

# Años de educación a partir del nivel (p301a) y el último año (p301b) o grado (p301c)
anios_educacion <- function(p301a, p301b, p301c) {
  anio_ok <- function(x, min, max) if_else(!is.na(x) & x >= min & x <= max, x, NA_real_)
  case_when(
    p301a %in% c(1, 2) ~ 0,
    p301a == 3  ~ coalesce(anio_ok(p301b, 0, 5), anio_ok(p301c, 0, 5), 3),
    p301a == 4  ~ 6,
    p301a == 5  ~ 6  + coalesce(anio_ok(p301b, 1, 4), 2),
    p301a == 6  ~ 11,
    p301a == 7  ~ 11 + coalesce(anio_ok(p301b, 1, 4), 2),
    p301a == 8  ~ 11 + coalesce(anio_ok(p301b, 1, 5), 3),
    p301a == 9  ~ 11 + coalesce(anio_ok(p301b, 1, 6), 2),
    p301a == 10 ~ 11 + coalesce(anio_ok(p301b, 1, 7), 5),
    p301a == 11 ~ 16 + coalesce(anio_ok(p301b, 1, 4), 2)
  )
}

# Rama de actividad: sección CIIU rev. 4 a partir de la división (2 primeros dígitos)
rama_ciiu <- function(p506r4) {
  division <- floor(p506r4 / 100)
  case_when(
    between(division, 1, 3)   ~ "A_agropecuario",
    between(division, 5, 9)   ~ "B_mineria",
    between(division, 10, 33) ~ "C_manufactura",
    between(division, 35, 39) ~ "DE_electricidad_agua",
    between(division, 41, 43) ~ "F_construccion",
    between(division, 45, 47) ~ "G_comercio",
    between(division, 49, 53) ~ "H_transporte",
    between(division, 55, 56) ~ "I_alojamiento_comidas",
    between(division, 58, 63) ~ "J_informacion",
    between(division, 64, 68) ~ "KL_financieras_inmobiliarias",
    between(division, 69, 82) ~ "MN_profesionales_administrativos",
    division == 84            ~ "O_administracion_publica",
    division == 85            ~ "P_ensenanza",
    between(division, 86, 88) ~ "Q_salud",
    between(division, 97, 98) ~ "T_hogares",
    TRUE                      ~ "RSU_otros_servicios"
  )
}

# Construye base_modelo: población, eliminación de faltantes reales y recodificación
construir_base_modelo <- function(unida) {
  hogares <- unida %>%
    filter(p204 == 1) %>%
    count(anio, conglome, vivienda, hogar, name = "tam_hogar")

  unida %>%
    inner_join(hogares, by = c("anio", "conglome", "vivienda", "hogar")) %>%
    filter(ocu500 == 1,
           p507 %in% c(3, 4, 6),                 # empleado, obrero, trabajador del hogar
           i524a1 > 0,
           p513t > 0,
           !is.na(p208a), p208a != 99,
           !is.na(p301a), !p301a %in% c(12, 99),
           !is.na(p300a), !p300a %in% c(9, 99),
           !is.na(p558c), !is.na(p209),
           !is.na(p505r4), p505r4 != 9999,
           !is.na(p506r4), p506r4 != 9999,
           p510 %in% 1 | !is.na(p511a),                         # FF.AA./PNP: pase
           p510 %in% 1 | (!is.na(p512a) & p512a != 9)) %>%      # FF.AA./PNP: pase
    transmute(
      anio, conglome, vivienda, hogar, codperso, fac500a,
      # variables objetivo
      y1_ln_ingreso_mensual = log(i524a1 / 12),
      y2_horas_semanales    = p513t,
      # predictoras
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
      tam_hogar,
      urbano             = as.integer(estrato %in% 1:5),
      region             = case_when(dominio %in% 1:3 ~ "costa",
                                     dominio %in% 4:6 ~ "sierra",
                                     dominio == 7     ~ "selva",
                                     TRUE             ~ "lima_metropolitana"),
      cat_ocupacional    = case_when(p507 == 3 ~ "empleado",
                                     p507 == 4 ~ "obrero",
                                     TRUE      ~ "trab_hogar"),
      sector_publico     = as.integer(p510 %in% c(1, 2, 3)),     # trab. del hogar (NA) = 0
      tam_empresa        = case_when(p510 %in% 1 ~ "ffaa_pnp",
                                     p512a == 1  ~ "hasta_20",
                                     p512a == 2  ~ "21_a_50",
                                     p512a == 3  ~ "51_a_100",
                                     p512a == 4  ~ "101_a_500",
                                     TRUE        ~ "mas_de_500"),
      contrato           = case_when(p510 %in% 1 ~ "ffaa_pnp",
                                     p511a == 1  ~ "indefinido",
                                     p511a == 2  ~ "plazo_fijo",
                                     p511a == 6  ~ "cas",
                                     p511a == 7  ~ "sin_contrato",
                                     TRUE        ~ "otro"),
      afiliado_pensiones = as.integer(coalesce(p558a1, 0) + coalesce(p558a2, 0) +
                                      coalesce(p558a3, 0) + coalesce(p558a4, 0) > 0),
      rama               = rama_ciiu(p506r4),
      grupo_ocupacional  = paste0("grupo_", floor(p505r4 / 1000))
    )
}
