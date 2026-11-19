# =====================================================================
#  01_funciones_integracion.R — inventario y unión de los módulos ENAHO
#  Módulos: 200 (miembros del hogar), 300 (educación), 500 (empleo e ingresos)
# =====================================================================

modulos <- c("enaho01-200"  = "200",
             "enaho01a-300" = "300",
             "enaho01a-500" = "500")

# La variable del año aparece como AÑO, año o con caracteres raros según la codificación
es_anio    <- function(v) v %in% c("anio", "ano", "a_o") | str_detect(v, "^a[^a-z0-9_]{1,2}o$")
normalizar <- function(v) if_else(es_anio(v), "anio", v)

# Encabezado de un CSV: nombres de columnas, separador y codificación (UTF-8 o latin1)
leer_encabezado <- function(ruta) {
  muestra <- readLines(ruta, n = 2000L, warn = FALSE)
  cod     <- if (all(validUTF8(muestra))) "UTF-8" else "latin1"
  linea   <- iconv(muestra[1], cod, "UTF-8") %>% str_remove("^﻿")
  seps    <- c(",", ";", "|", "\t")
  sep     <- seps[which.max(str_count(linea, fixed(seps)))]
  cols    <- str_split(linea, fixed(sep))[[1]] %>%
    str_remove_all("\"") %>% str_trim() %>% str_to_lower()
  list(cols = cols, sep = sep, cod = cod)
}

a_numero <- function(x) suppressWarnings(as.numeric(x))

# ¿La columna tiene decimales escritos con coma (12000,5)?
usa_coma <- function(x) {
  x    <- na_if(str_trim(x), "")
  n_ok <- sum(!is.na(x))
  sum(!is.na(a_numero(x))) < n_ok &&
    sum(!is.na(a_numero(str_replace_all(x, fixed(","), ".")))) == n_ok
}

# Limpia una columna leída como texto: espacios -> NA, texto numérico -> número,
# coma decimal -> punto, enteros -> integer
limpiar_col <- function(x) {
  x    <- na_if(str_trim(x), "")
  n_ok <- sum(!is.na(x))
  num  <- a_numero(x)
  if (sum(!is.na(num)) < n_ok) num <- a_numero(str_replace_all(x, fixed(","), "."))
  es_numero <- sum(!is.na(num)) == n_ok && (n_ok == 0 || max(abs(num), na.rm = TRUE) < 2^53)
  if (!es_numero) return(x)
  ok <- num[!is.na(num)]
  if (length(ok) > 0 && all(ok == round(ok)) && max(abs(ok)) < .Machine$integer.max) {
    as.integer(num)
  } else {
    num
  }
}

# Lee de un CSV solo las columnas pedidas y lo limpia
leer_modulo <- function(ruta, quiero) {
  enc  <- leer_encabezado(ruta)
  norm <- normalizar(enc$cols)
  pos  <- which(norm %in% quiero)
  df <- read_delim(ruta, delim = enc$sep, col_select = all_of(pos),
                   col_types = cols(.default = col_character()),
                   locale = locale(encoding = enc$cod), na = c("", "NA"),
                   trim_ws = TRUE, progress = FALSE, name_repair = "minimal")
  names(df) <- norm[pos]

  coma <- names(df)[map_lgl(df, usa_coma)]
  df <- df %>%
    mutate(across(-any_of("ubigeo"), limpiar_col),
           across(all_of(llaves), as.integer))
  if ("ubigeo" %in% names(df)) {
    df <- df %>% mutate(ubigeo = str_pad(as.character(suppressWarnings(as.integer(ubigeo))), 6, pad = "0"))
  }
  n_antes <- nrow(df)
  df <- distinct(df, !!!rlang::syms(llaves), .keep_all = TRUE)
  if (nrow(df) < n_antes) warning(basename(ruta), ": ", n_antes - nrow(df), " duplicados por llave")
  list(datos = df, coma = coma)
}

# Lista los CSV de los 3 módulos dentro de datos/raw (en cualquier subcarpeta)
buscar_archivos <- function(ruta_raw) {
  tibble(ruta = list.files(ruta_raw, pattern = "\\.csv$", recursive = TRUE,
                           full.names = TRUE, ignore.case = TRUE)) %>%
    mutate(nombre = str_to_lower(tools::file_path_sans_ext(basename(ruta)))) %>%
    filter(str_detect(nombre, "-(19|20)\\d{2}")) %>%
    mutate(anio  = as.integer(str_match(nombre, "-((19|20)\\d{2})")[, 2]),
           clave = str_remove(nombre, "-(19|20)\\d{2}")) %>%
    filter(clave %in% names(modulos), anio %in% anios) %>%
    mutate(modulo = unname(modulos[clave]),
           mb     = round(file.size(ruta) / 1024^2, 1)) %>%
    arrange(modulo, anio) %>%
    distinct(modulo, anio, .keep_all = TRUE)
}

# Inventario: una fila por variable, con los módulos y años en que aparece
inventario_variables <- function(info) {
  registros <- info %>%
    select(modulo, anio, ruta) %>%
    mutate(variable = map(ruta, ~ normalizar(leer_encabezado(.x)$cols))) %>%
    select(-ruta) %>%
    unnest(variable) %>%
    filter(variable != "") %>%
    distinct()
  n_anios_total <- n_distinct(registros$anio)
  registros %>%
    group_by(variable) %>%
    summarise(modulos   = str_c(sort(unique(modulo)), collapse = ", "),
              n_modulos = n_distinct(modulo),
              n_anios   = n_distinct(anio),
              desde     = min(anio),
              hasta     = max(anio),
              .groups   = "drop") %>%
    mutate(en_todos_los_anios = n_anios == n_anios_total,
           modulo_origen      = str_remove(modulos, ",.*$")) %>%
    arrange(modulo_origen, variable)
}

# Une los 3 módulos de un año (base: módulo 200) y lo guarda en datos/interim
unir_anio <- function(a, info, vars_mod, orden) {
  partes <- list()
  coma   <- list()
  for (m in c("200", "300", "500")) {
    ruta <- info$ruta[info$modulo == m & info$anio == a]
    if (length(ruta) == 0) next
    r <- leer_modulo(ruta, c(llaves, vars_mod[[m]]))
    partes[[m]] <- r$datos
    if (length(r$coma) > 0) coma[[m]] <- tibble(anio = a, modulo = m, variable = r$coma)
  }
  if (is.null(partes[["200"]])) return(NULL)

  base <- partes[["200"]]
  for (m in c("300", "500")) {
    if (!is.null(partes[[m]])) {
      base <- left_join(base, partes[[m]], by = llaves, relationship = "one-to-one")
    }
  }
  faltan <- setdiff(orden, c(names(base), "anio"))
  base <- base %>%
    mutate(anio = a, !!!set_names(rep(list(NA), length(faltan)), faltan)) %>%
    select(all_of(orden))
  write_rds(base, file.path(rutas$interim, sprintf("enaho_unida_%d.rds", a)))

  list(
    resumen = tibble(anio = a,
                     filas_mod_200 = nrow(partes[["200"]]),
                     filas_mod_300 = if (is.null(partes[["300"]])) 0L else nrow(partes[["300"]]),
                     filas_mod_500 = if (is.null(partes[["500"]])) 0L else nrow(partes[["500"]]),
                     filas_unida   = nrow(base),
                     columnas      = ncol(base)),
    coma = bind_rows(coma))
}
