# =====================================================================
#  verificar_pipeline.R — verificaciones automáticas del pipeline
#
#  Uso (en la consola de Positron, con el proyecto abierto como carpeta):
#      source("R/verificar_pipeline.R")
#
#  Comprueba que los datos y los resultados guardados por las páginas
#  01 a 08 sean los esperados. Si alguna verificación falla, el script
#  termina con error y dice cuál. Guarda dos tablas para el artículo:
#      outputs/tablas/verificaciones.csv   (una fila por verificación)
#      outputs/tablas/huellas_sha256.csv   (huella de cada archivo de datos)
# =====================================================================

source(here::here("R", "00_config.R"))
archivo_base_seleccion <- file.path(rutas$processed, "base_seleccion.csv.gz")

registro <- tibble(etapa = character(), verificacion = character(),
                   obtenido = character(), esperado = character(), cumple = logical())

# Registra una verificación; no se detiene aquí para poder ver todas al final
chequear <- function(etapa, verificacion, condicion, obtenido = "", esperado = "") {
  ok <- isTRUE(condicion)
  registro <<- add_row(registro, etapa = etapa, verificacion = verificacion,
                       obtenido = as.character(obtenido), esperado = as.character(esperado),
                       cumple = ok)
  cat(if (ok) "  OK   " else "  FALLA", etapa, "|", verificacion, "\n")
  invisible(ok)
}

leer_tabla <- function(nombre) {
  ruta <- file.path(rutas$tablas, paste0(nombre, ".csv"))
  if (!file.exists(ruta)) stop("Falta ", ruta, ". Ejecuta primero quarto render.")
  read_csv(ruta, show_col_types = FALSE)
}

# ---------------------------------------------------------------------
# 1. Huellas SHA-256 de los archivos de datos
#    Si alguien descarga los datos de Zenodo y obtiene la misma huella,
#    tiene exactamente los mismos archivos.
# ---------------------------------------------------------------------
cat("\n== 1. Huellas de los archivos de datos\n")
huella <- function(ruta) {
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(ruta, open = "rb"); on.exit(close(con))
    as.character(openssl::sha256(con))
  } else {
    paste0("md5:", unname(tools::md5sum(ruta)))
  }
}
archivos_datos <- c(list.files(rutas$raw, pattern = "\\.(csv|CSV|sav|dta|zip)$",
                               recursive = TRUE, full.names = TRUE),
                    list.files(rutas$processed, pattern = "\\.csv\\.gz$", full.names = TRUE))
huellas <- tibble(
  archivo = sub(paste0("^", here::here(), "/?"), "", archivos_datos),
  megabytes = round(file.size(archivos_datos) / 1024^2, 1),
  sha256 = vapply(archivos_datos, huella, character(1), USE.NAMES = FALSE)
)
write_csv(huellas, file.path(rutas$tablas, "huellas_sha256.csv"))
bases <- file.path(rutas$processed,
                   c("base_modelo.csv.gz", "base_seleccion.csv.gz", "base_contexto.csv.gz"))
chequear("Datos", "Bases procesadas presentes (modelo, selección, contexto)",
         all(file.exists(bases)), paste(sum(file.exists(bases)), "de 3"), "3 de 3")
cat("  ", nrow(huellas), "archivos con huella guardada en huellas_sha256.csv\n")

# ---------------------------------------------------------------------
# 2. Base del modelo
# ---------------------------------------------------------------------
cat("\n== 2. Base del modelo\n")
base <- read_csv(archivo_base_modelo, show_col_types = FALSE)
embudo <- leer_tabla("embudo")

chequear("Datos", "Filas de base_modelo = último paso del embudo",
         nrow(base) == tail(embudo$Personas, 1),
         fmt(nrow(base)), fmt(tail(embudo$Personas, 1)))
chequear("Datos", "El embudo solo disminuye en cada paso",
         all(diff(embudo$Personas) <= 0),
         paste(fmt(embudo$Personas), collapse = " > "), "decreciente")
dup <- sum(duplicated(base[, intersect(c("anio", llaves), names(base))]))
chequear("Datos", "Sin personas repetidas (año + conglome + vivienda + hogar + codperso)",
         dup == 0, dup, 0)
chequear("Datos", "Los 12 años (2014–2025) están presentes",
         setequal(unique(base$anio), anios),
         paste(range(base$anio), collapse = "–"), "2014–2025")
na_vars <- sum(is.na(base[, c(objetivos, predictoras)]))
chequear("Datos", "Sin faltantes en Y1, Y2 y las 19 predictoras", na_vars == 0, na_vars, 0)
chequear("Datos", "Ingreso mensual positivo (Y1 finito)",
         all(is.finite(base$y1_ln_ingreso_mensual)),
         paste(round(range(base$y1_ln_ingreso_mensual), 2), collapse = " a "), "finito")
chequear("Datos", "Horas por semana entre 1 y 168",
         all(base$y2_horas_semanales > 0 & base$y2_horas_semanales <= 168),
         paste(range(base$y2_horas_semanales), collapse = " a "), "1 a 168")
chequear("Datos", "Edad de 14 años a más", min(base$edad) >= 14, min(base$edad), ">= 14")

# ---------------------------------------------------------------------
# 3. Base de selección (para Heckman)
# ---------------------------------------------------------------------
cat("\n== 3. Base de selección\n")
sel <- read_csv(archivo_base_seleccion, show_col_types = FALSE,
                col_select = c(anio, conglome, vivienda, hogar, codperso, asalariado, edad))
chequear("Heckman", "Personas de 14 años a más en la base de selección",
         min(sel$edad) >= 14, fmt(nrow(sel)), "todas >= 14")
chequear("Heckman", "Sin personas repetidas en la base de selección",
         !anyDuplicated(sel[, c("anio", llaves)]), "0", "0")
# Pueden diferir un poco: la selección exige 14 años a más y datos de hogar completos
chequear("Heckman", "Asalariados de la selección ≈ filas de base_modelo (diferencia < 1 %)",
         abs(sum(sel$asalariado) / nrow(base) - 1) < 0.01,
         fmt(sum(sel$asalariado)), paste("≈", fmt(nrow(base))))
rm(sel); invisible(gc())

# ---------------------------------------------------------------------
# 4. Modelos predictivos
# ---------------------------------------------------------------------
cat("\n== 4. Modelos predictivos\n")
part <- leer_tabla("particion")
n_part <- setNames(part$personas, part$conjunto)
chequear("Predictivo", "Entrenamiento + prueba = total",
         n_part["entrenamiento"] + n_part["prueba"] == n_part["total"],
         fmt(n_part["entrenamiento"] + n_part["prueba"]), fmt(n_part["total"]))
chequear("Predictivo", "Partición 80/20",
         abs(n_part["entrenamiento"] / n_part["total"] - 0.8) < 0.001,
         round(n_part["entrenamiento"] / n_part["total"], 3), 0.8)

prueba <- leer_tabla("metricas_prueba")
cv     <- leer_tabla("metricas_validacion_cruzada")
mejor  <- prueba |> filter(metrica == "rmse") |> group_by(objetivo) |> slice_min(valor)
chequear("Predictivo", "Random Forest tiene el menor RMSE en Y1 y Y2",
         all(mejor$modelo == "random_forest"), paste(mejor$modelo, collapse = ", "),
         "random_forest")
dif <- prueba |> filter(metrica == "rsq") |>
  inner_join(cv |> filter(metrica == "rsq"), by = c("objetivo", "modelo", "metrica")) |>
  mutate(d = abs(valor - media))
chequear("Predictivo", "R² de prueba y de validación cruzada difieren < 0,03 (sin sobreajuste)",
         all(dif$d < 0.03), round(max(dif$d), 3), "< 0.03")

# Reproducibilidad de la semilla: la misma partición se repite
set.seed(semilla); a <- sample.int(1e6, 5)
set.seed(semilla); b <- sample.int(1e6, 5)
chequear("Predictivo", "La semilla 2026 reproduce los mismos números aleatorios",
         identical(a, b), paste(a, collapse = " "), "idénticos")

# ---------------------------------------------------------------------
# 5. Regresión multivariada y Heckman
# ---------------------------------------------------------------------
cat("\n== 5. Regresión multivariada y Heckman\n")
pillai <- leer_tabla("pillai_multivariado")
chequear("Heckman", "Todos los predictores con Pillai significativo (p < 0,05)",
         all(pillai$`Pr(>F)`[!is.na(pillai$`Pr(>F)`)] < 0.05),
         sum(!is.na(pillai$Pillai)), "todos")
r <- leer_tabla("correlacion_residuos")[[1]]
chequear("Heckman", "Residuos de Y1 y Y2 correlacionados (justifica el modelo multivariado)",
         r > 0.1, round(r, 3), "> 0.1")

hc <- leer_tabla("heckman_coeficientes")
lam <- hc |> filter(termino == "lambda")
chequear("Heckman", "λ negativa en Y1 y Y2 (selección negativa)",
         nrow(lam) == 2 && all(lam$estimacion < 0),
         paste(round(lam$estimacion, 3), collapse = " y "), "< 0")
hp <- leer_tabla("heckman_pillai") |> filter(termino == "lambda")
chequear("Heckman", "λ conjuntamente significativa (Pillai)",
         hp$`Pr(>F)` < 0.05, signif(hp$`Pr(>F)`, 3), "< 0.05")
r2l <- leer_tabla("heckman_diagnostico")$r2_lambda
chequear("Heckman", "λ no es combinación lineal exacta de X (R² < 0,99)",
         r2l < 0.99, round(r2l, 3), "< 0.99")
educ <- hc |> filter(objetivo == "y1_ln_ingreso_mensual", termino == "anios_educ")
chequear("Heckman", "Retorno de la educación menor con Heckman que con MCO",
         educ$estimacion[educ$modelo == "Heckman bietápico"] <
           educ$estimacion[educ$modelo == "MCO sin corrección"],
         paste(round(100 * educ$estimacion, 2), collapse = " / "), "Heckman < MCO")

ruta_heckit <- file.path(rutas$tablas, "heckman_heckit.csv")
if (file.exists(ruta_heckit)) {
  hk <- read_csv(ruta_heckit, show_col_types = FALSE)
  # El nombre de las columnas depende de cómo se guardó la tabla: se buscan
  # las filas de λ en cualquier columna de texto y la estimación en la
  # primera columna numérica (o en una que se llame estimación / estimate)
  es_lambda <- Reduce(`|`, lapply(hk[vapply(hk, is.character, logical(1))],
                                  function(x) grepl("lambda|invMills|imr|mills", x,
                                                    ignore.case = TRUE)),
                      accumulate = FALSE, init = rep(FALSE, nrow(hk)))
  col_est <- intersect(c("estimacion", "estimate", "Estimate", "coef"), names(hk))[1]
  if (is.na(col_est)) col_est <- names(hk)[vapply(hk, is.numeric, logical(1))][1]
  lam_hk <- if (is.na(col_est)) numeric(0) else hk[[col_est]][es_lambda]
  cat("  columnas de heckman_heckit.csv:", paste(names(hk), collapse = ", "), "\n")
  chequear("Heckman", "heckit (sampleSelection) da λ con el mismo signo",
           length(lam_hk) > 0 && all(sign(lam_hk) == sign(lam$estimacion[seq_along(lam_hk)])),
           paste(round(lam_hk, 3), collapse = " y "), "< 0")
} else {
  cat("  (sin heckman_heckit.csv: se omite la comparación con sampleSelection)\n")
}

# ---------------------------------------------------------------------
# 6. Heterogeneidad
# ---------------------------------------------------------------------
cat("\n== 6. Heterogeneidad\n")
dep <- read_csv(file.path(rutas$tablas, "departamentos_coeficientes.csv"),
                show_col_types = FALSE, col_types = cols(segmento = col_character()))
dep$segmento <- str_pad(dep$segmento, 2, pad = "0")
chequear("Segmentos", "Heckman estimado en los 25 departamentos",
         n_distinct(dep$segmento) == 25, n_distinct(dep$segmento), 25)
chequear("Segmentos", "Puno (21) incluido", "21" %in% dep$segmento, "21", "presente")
inter <- leer_tabla("regiones_interacciones_pillai")
chequear("Segmentos", "Interacciones con la región significativas",
         all(inter$`Pr(>F)` < 0.05), nrow(inter), "todas p < 0.05")
sil <- leer_tabla("departamentos_siluetas")
chequear("Segmentos", "k elegido por silueta = número de grupos usado",
         sil$k[which.max(sil$silueta_media)] ==
           n_distinct(leer_tabla("departamentos_grupos")$grupo),
         sil$k[which.max(sil$silueta_media)], "igual")

# ---------------------------------------------------------------------
# 7. Entorno de software
# ---------------------------------------------------------------------
cat("\n== 7. Entorno\n")
sinc <- tryCatch(renv::status()$synchronized, error = function(e) NA)
chequear("Entorno", "Paquetes instalados = renv.lock", isTRUE(sinc), sinc, TRUE)
chequear("Entorno", "Versión de R registrada", TRUE, R.version.string, "")
qv <- tryCatch(as.character(quarto::quarto_version()),
               error = function(e) tryCatch(system("quarto --version", intern = TRUE),
                                            error = function(e) NA))
chequear("Entorno", "Quarto disponible", !is.na(qv[1]), qv[1], "")

# ---------------------------------------------------------------------
# Resultado
# ---------------------------------------------------------------------
write_csv(registro, file.path(rutas$tablas, "verificaciones.csv"))
resumen <- registro |> count(etapa, cumple)
cat("\n== Resumen\n"); print(resumen)
cat("\nGuardado en outputs/tablas/verificaciones.csv y huellas_sha256.csv\n")
if (any(!registro$cumple)) {
  stop(sum(!registro$cumple), " verificación(es) fallaron: ",
       paste(registro$verificacion[!registro$cumple], collapse = "; "))
}
cat("Todas las verificaciones se cumplen (", nrow(registro), ").\n", sep = "")
