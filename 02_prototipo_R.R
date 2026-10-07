# =====================================================================
# Prototipo TP - Demanda eléctrica vs temperatura
# Paso a paso en RStudio: conexión a Supabase, carga de datos de prueba,
# verificación y primeros gráficos.
# Requiere R >= 4.1 (usa el pipe nativo |>).
# Ejecutar sección por sección con Ctrl + Enter (o Cmd + Enter).
# =====================================================================


# ---------------------------------------------------------------------
# 1. PAQUETES (se instalan una sola vez)
# ---------------------------------------------------------------------
# install.packages(c("DBI", "RPostgres", "dplyr", "lubridate",
#                    "ggplot2", "mgcv", "readr", "tidyr", "usethis"))

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(mgcv)


# ---------------------------------------------------------------------
# 2. CREDENCIALES (una sola vez, NO escribir la contraseña en este script)
# ---------------------------------------------------------------------
usethis::edit_r_environ()
# Se abre el archivo .Renviron. Agregar estas 4 líneas con los datos de
# Supabase (Connect > Connection string), guardar y reiniciar R
# (Session > Restart R):
#
#   SB_HOST=aws-0-xxxx.pooler.supabase.com
#   SB_PORT=5432
#   SB_USER=postgres.xxxxxxxxxxxxxxxx
#   SB_PASS=tu_contraseña_de_la_base
#
# Si usan la conexión directa (no el pooler), el usuario es solo "postgres".


# ---------------------------------------------------------------------
# 3. CONEXIÓN
# ---------------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("SB_HOST"),
  port     = as.integer(Sys.getenv("SB_PORT", "5432")),
  dbname   = "postgres",
  user     = Sys.getenv("SB_USER"),
  password = Sys.getenv("SB_PASS"),
  sslmode  = "require"
)

# Prueba: debe devolver las tablas creadas con 01_schema_supabase.sql
dbListTables(con)
dbGetQuery(con, "select * from regiones")


# ---------------------------------------------------------------------
# 4. CARGA DE LA BASE SINTÉTICA (ejecutar UNA sola vez)
# ---------------------------------------------------------------------
# El CSV debe estar en la carpeta del proyecto de RStudio.
datos_csv <- read_csv("demanda_temperatura_sintetico.csv",
                      show_col_types = FALSE) |>
  mutate(fecha = as.Date(fecha))

# id de la estación sembrada en el script SQL
estacion_id <- dbGetQuery(
  con,
  "select id from estaciones_smn where nombre = 'Buenos Aires (Observatorio Central)'"
)$id |> as.integer()

demanda <- datos_csv |>
  transmute(fecha, region, demanda_mw)

temperatura <- datos_csv |>
  transmute(fecha, estacion_id = estacion_id,
            temp_media_c, temp_max_c, temp_min_c)

# Si necesitan volver a cargar, vaciar primero (BORRA todos los datos
# de ambas tablas, también los reales):
# dbExecute(con, "truncate demanda_cammesa, temperatura_smn")

dbWriteTable(con, "demanda_cammesa", demanda,     append = TRUE, row.names = FALSE)
dbWriteTable(con, "temperatura_smn", temperatura, append = TRUE, row.names = FALSE)


# ---------------------------------------------------------------------
# 5. LECTURA DE LA VISTA UNIFICADA Y VERIFICACIÓN
# ---------------------------------------------------------------------
datos <- dbGetQuery(con, "select * from datos_unificados order by fecha") |>
  mutate(fecha = as.Date(fecha),
         dia_semana = factor(dia_semana),
         estacion_anio = factor(estacion_anio,
                                levels = c("verano", "otoño", "invierno", "primavera")))

nrow(datos)               # debe dar 366
glimpse(datos)
summary(datos$demanda_mw)
summary(datos$temp_media_c)
colSums(is.na(datos))     # no debe haber NA


# ---------------------------------------------------------------------
# 6. GRÁFICOS DE LA FIGURA 1 (exploración)
# ---------------------------------------------------------------------
dir.create("figuras", showWarnings = FALSE)

# 6.1 Dispersión demanda vs temperatura, con curva suave
g_dispersion <- ggplot(datos, aes(temp_media_c, demanda_mw)) +
  geom_point(aes(color = estacion_anio), alpha = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = "black", se = TRUE) +
  labs(x = "Temperatura media (°C)", y = "Demanda eléctrica (MW)",
       color = "Estación del año") +
  theme_minimal()
g_dispersion
ggsave("figuras/dispersion_demanda_temperatura.png", g_dispersion,
       width = 7, height = 4.5, dpi = 200)

# 6.2 Series temporales superpuestas (eje secundario para la temperatura)
coef <- max(datos$demanda_mw) / max(datos$temp_media_c)

g_series <- ggplot(datos, aes(fecha)) +
  geom_line(aes(y = demanda_mw, color = "Demanda (MW)")) +
  geom_line(aes(y = temp_media_c * coef, color = "Temperatura (°C)")) +
  scale_y_continuous(name = "Demanda eléctrica (MW)",
                     sec.axis = sec_axis(~ . / coef, name = "Temperatura media (°C)")) +
  labs(x = "Fecha", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
g_series
ggsave("figuras/series_temporales.png", g_series,
       width = 7, height = 4.5, dpi = 200)


# ---------------------------------------------------------------------
# 7. VISTAZO AL MODELO (GAM) - todavía sin métricas de evaluación
# ---------------------------------------------------------------------
modelo <- gam(demanda_mw ~ s(temp_media_c) + dia_semana + es_feriado,
              data = datos, method = "REML")
summary(modelo)           # R² ajustado y devianza explicada
plot(modelo, select = 1, shade = TRUE,
     xlab = "Temperatura media (°C)", ylab = "Efecto sobre la demanda (MW)")


# ---------------------------------------------------------------------
# 8. CIERRE DE LA CONEXIÓN (al terminar de trabajar)
# ---------------------------------------------------------------------
dbDisconnect(con)
