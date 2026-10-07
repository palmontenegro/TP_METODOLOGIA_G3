# =====================================================================
# Prototipo TP - Evaluación del diseño: variables A, P, E, T y eficiencia
#   Eficiencia = 0,25*A + 0,35*P + 0,25*E + 0,15*T
# Requiere haber corrido antes 01_schema_supabase.sql y 02_prototipo_R.R
# (tablas cargadas en Supabase). Ejecutar sección por sección.
# =====================================================================

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(mgcv)

# ---------------------------------------------------------------------
# 0. PARÁMETROS DEL DISEÑO (los mismos que en Materiales y Métodos)
# ---------------------------------------------------------------------
PROP_ENTRENAMIENTO <- 0.80      # 80 % entrenamiento / 20 % prueba
MAPE_REF           <- 10        # % a partir del cual P = 0
T_MAX              <- 300       # s (5 min) a partir del cual T = 0
N_REPETICIONES     <- 5         # ejecuciones para medir el tiempo
PASO_GRILLA        <- 0.1       # °C, para calcular pendientes
FORMULA_GAM        <- demanda_mw ~ s(temp_media_c) + dia_semana + es_feriado

descripcion_corrida <- "GAM s(temp)+dia_semana+feriado; datos sinteticos 2024"

# ---------------------------------------------------------------------
# 1. CONEXIÓN (si ya existe 'con' de 02_prototipo_R.R, se reutiliza)
# ---------------------------------------------------------------------
if (!exists("con") || !dbIsValid(con)) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SB_HOST"),
    port     = as.integer(Sys.getenv("SB_PORT", "5432")),
    dbname   = "postgres",
    user     = Sys.getenv("SB_USER"),
    password = Sys.getenv("SB_PASS"),
    sslmode  = "require"
  )
}

# ---------------------------------------------------------------------
# 2. FUNCIONES AUXILIARES
# ---------------------------------------------------------------------
leer_datos <- function(con) {
  dbGetQuery(con, "select * from datos_unificados order by fecha") |>
    mutate(fecha = as.Date(fecha),
           dia_semana = factor(dia_semana))
}

# Curva ajustada (demanda estimada) sobre una grilla de temperaturas.
# Los controles se fijan en un valor de referencia: como el modelo es
# aditivo, no afectan la pendiente de la curva.
curva <- function(modelo, temps, niveles_dia) {
  nd <- data.frame(
    temp_media_c = temps,
    dia_semana   = factor(niveles_dia[1], levels = niveles_dia),
    es_feriado   = 0
  )
  as.numeric(predict(modelo, newdata = nd, type = "response"))
}

# Pendiente media (MW/°C) de la curva entre 'desde' y 'hasta', sin salir
# del rango de temperaturas observado en el subconjunto
pendiente_media <- function(modelo, datos_sub, desde, hasta, niveles_dia) {
  lo <- max(desde, min(datos_sub$temp_media_c))
  hi <- min(hasta, max(datos_sub$temp_media_c))
  if (hi - lo < 2 * PASO_GRILLA) return(NA_real_)
  g <- seq(lo, hi, by = PASO_GRILLA)
  mean(diff(curva(modelo, g, niveles_dia)) / PASO_GRILLA)
}

# Similitud entre dos pendientes: 1 = idénticas, 0 = difieren al máximo
similitud <- function(s1, s2) {
  if (anyNA(c(s1, s2))) return(0)
  den <- max(abs(s1), abs(s2))
  if (den == 0) return(1)
  max(0, 1 - abs(s1 - s2) / den)
}

# ---------------------------------------------------------------------
# 3. FLUJO COMPLETO: lectura -> división -> ajuste -> A, P, E
#    (lo que se cronometra para la variable T)
# ---------------------------------------------------------------------
flujo <- function(con) {
  datos <- leer_datos(con)
  niveles_dia <- levels(datos$dia_semana)

  # División cronológica 80 / 20
  n_ent <- floor(PROP_ENTRENAMIENTO * nrow(datos))
  entren <- datos[seq_len(n_ent), ]
  prueba <- datos[(n_ent + 1):nrow(datos), ]

  modelo <- gam(FORMULA_GAM, data = entren, method = "REML")

  # --- A: ajuste (R² ajustado sobre entrenamiento)
  A <- max(0, summary(modelo)$r.sq)

  # --- P: precisión predictiva (MAPE sobre prueba)
  pred <- as.numeric(predict(modelo, newdata = prueba))
  mape <- mean(abs(prueba$demanda_mw - pred) / prueba$demanda_mw) * 100
  P <- max(0, 1 - mape / MAPE_REF)

  # --- E: estabilidad (subconjuntos por paridad del mes)
  p10 <- as.numeric(quantile(datos$temp_media_c, 0.10))
  p90 <- as.numeric(quantile(datos$temp_media_c, 0.90))
  tmin <- min(datos$temp_media_c); tmax <- max(datos$temp_media_c)

  sub1 <- datos[month(datos$fecha) %% 2 == 1, ]
  sub2 <- datos[month(datos$fecha) %% 2 == 0, ]
  m1 <- gam(FORMULA_GAM, data = sub1, method = "REML")
  m2 <- gam(FORMULA_GAM, data = sub2, method = "REML")

  S_calor <- c(pendiente_media(m1, sub1, p90, tmax, niveles_dia),
               pendiente_media(m2, sub2, p90, tmax, niveles_dia))
  S_frio  <- c(pendiente_media(m1, sub1, tmin, p10, niveles_dia),
               pendiente_media(m2, sub2, tmin, p10, niveles_dia))

  E_calor <- similitud(S_calor[1], S_calor[2])
  E_frio  <- similitud(S_frio[1],  S_frio[2])
  E <- (E_calor + E_frio) / 2

  # Sensibilidad del modelo de entrenamiento en los extremos (MW/°C),
  # para contrastar la hipótesis calor > frío
  sens_calor <- pendiente_media(modelo, entren, p90, tmax, niveles_dia)
  sens_frio  <- pendiente_media(modelo, entren, tmin, p10, niveles_dia)

  list(A = A, P = P, E = E, mape = mape,
       E_calor = E_calor, E_frio = E_frio,
       S_calor = S_calor, S_frio = S_frio,
       sens_calor = sens_calor, sens_frio = sens_frio,
       n_entrenamiento = nrow(entren), n_prueba = nrow(prueba))
}

# ---------------------------------------------------------------------
# 4. TIEMPO DE EJECUCIÓN (T): promedio de N repeticiones
# ---------------------------------------------------------------------
tiempos <- numeric(N_REPETICIONES)
for (i in seq_len(N_REPETICIONES)) {
  tiempos[i] <- system.time(resultado <- flujo(con))[["elapsed"]]
}
t_prom <- mean(tiempos)
Tvar   <- max(0, 1 - t_prom / T_MAX)

# ---------------------------------------------------------------------
# 5. EFICIENCIA
# ---------------------------------------------------------------------
A <- resultado$A; P <- resultado$P; E <- resultado$E
eficiencia <- 0.25 * A + 0.35 * P + 0.25 * E + 0.15 * Tvar

tabla <- data.frame(
  variable = c("A (ajuste)", "P (precisión predictiva)",
               "E (estabilidad)", "T (tiempo de ejecución)", "Eficiencia"),
  valor    = round(c(A, P, E, Tvar, eficiencia), 4),
  detalle  = c(
    "R² ajustado, entrenamiento",
    sprintf("MAPE = %.2f %% (prueba, n = %d)", resultado$mape, resultado$n_prueba),
    sprintf("E_calor = %.3f ; E_frío = %.3f", resultado$E_calor, resultado$E_frio),
    sprintf("t promedio = %.2f s (%d ejecuciones)", t_prom, N_REPETICIONES),
    ifelse(eficiencia >= 0.70, "cumple el umbral de 0,70", "NO alcanza el umbral de 0,70")
  )
)
print(tabla, row.names = FALSE)

cat(sprintf("\nSensibilidad media (modelo de entrenamiento): calor = %.0f MW/°C ; frío = %.0f MW/°C\n",
            resultado$sens_calor, resultado$sens_frio))

# ---------------------------------------------------------------------
# 6. GUARDAR LA CORRIDA EN SUPABASE (tabla 'evaluaciones')
#    La eficiencia la calcula la propia base (columna generada).
# ---------------------------------------------------------------------
dbExecute(
  con,
  "insert into evaluaciones (descripcion, ajuste_a, precision_p, estabilidad_e, tiempo_t)
   values ($1, $2, $3, $4, $5)",
  params = list(descripcion_corrida,
                round(A, 4), round(P, 4), round(E, 4), round(Tvar, 4))
)

dbGetQuery(con, "select * from evaluaciones order by id desc limit 5")

# dbDisconnect(con)   # al terminar de trabajar
