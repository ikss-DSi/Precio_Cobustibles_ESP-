library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# ==============================================================================
# Script: 02_Imputacion_nan_data.R
# Objetivo:
#   Eliminar del dataset normalizado los registros correspondientes al carburante
#   Adblue e imputar fechas completas ausentes para los carburantes restantes.
#
# Entradas:
#   Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv
#
# Salidas:
#   Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv
#
#
# Descripcion general:
#   El dataset normalizado contiene muchos dias sin registro para el carburante
#   Adblue. Debido a su baja cobertura temporal respecto al resto de carburantes,
#   se eliminan directamente todos los registros donde Carburante = "Adblue".
#
#   Despues se detectan fechas naturales que no tienen ningun registro en el
#   dataset. Para esas fechas se crean registros por Provincia-Carburante y se
#   imputa Precio mediante interpolacion lineal temporal dentro de cada serie.
#
#   Se usa interpolacion lineal porque los huecos detectados son pocos dias
#   aislados dentro de una serie diaria. Agrupar por Provincia-Carburante evita
#   mezclar comportamientos regionales o tipos de combustible diferentes, y el
#   uso de fechas cercanas conserva la tendencia local mejor que una media global.
#   Los precios se redondean a 3 decimales para evitar valores con
#   demasiadas cifras decimales generados por la interpolacion.
#
#   El archivo de entrada se sobrescribe con la version filtrada e imputada.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Definicion de rutas relativas del proyecto
# ------------------------------------------------------------------------------

ruta_dataset <- "../../Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv"


# ------------------------------------------------------------------------------
# 2. Carga del dataset y validacion de columnas necesarias
# ------------------------------------------------------------------------------

df <- read_csv(
  file = ruta_dataset,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

columnas_esperadas <- c(
  "Comunidad Autonoma",
  "Id_Provincia",
  "Provincia",
  "Carburante",
  "Fecha",
  "Mes",
  "Dia",
  "Numero_Semana",
  "Precio"
)

columnas_faltantes <- setdiff(columnas_esperadas, names(df))

if (length(columnas_faltantes) > 0) {
  stop(
    "El dataset no contiene las siguientes columnas esperadas: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

dias_semana_es <- c(
  "lunes",
  "martes",
  "miercoles",
  "jueves",
  "viernes",
  "sabado",
  "domingo"
)

df <- df %>%
  mutate(
    Fecha = as.Date(Fecha),
    Precio = parse_number(
      as.character(Precio),
      locale = locale(decimal_mark = ".")
    )
  )


# ------------------------------------------------------------------------------
# 3. Eliminacion de registros Adblue
# ------------------------------------------------------------------------------

registros_iniciales <- nrow(df)

fecha_inicial <- min(df$Fecha, na.rm = TRUE)
fecha_final <- max(df$Fecha, na.rm = TRUE)
dias_periodo <- as.integer(fecha_final - fecha_inicial) + 1
provincias_disponibles <- n_distinct(df$Id_Provincia)
registros_posibles_adblue <- dias_periodo * provincias_disponibles
registros_adblue <- sum(df$Carburante == "Adblue", na.rm = TRUE)
porcentaje_cobertura_adblue <- round(
  registros_adblue / registros_posibles_adblue * 100,
  2
)
porcentaje_faltante_adblue <- round(100 - porcentaje_cobertura_adblue, 2)

df_sin_adblue <- df %>%
  filter(is.na(Carburante) | Carburante != "Adblue")

registros_eliminados <- registros_iniciales - nrow(df_sin_adblue)


# ------------------------------------------------------------------------------
# 4. Deteccion de fechas completas ausentes
# ------------------------------------------------------------------------------

calendario_completo <- tibble(
  Fecha = seq.Date(
    from = fecha_inicial,
    to = fecha_final,
    by = "day"
  )
)

# Calendario esperado: todos los dias naturales entre fecha minima y maxima.
fechas_registradas <- df_sin_adblue %>%
  distinct(Fecha)

# Fechas faltantes: dias del calendario sin ningun registro tras retirar Adblue.
fechas_faltantes <- calendario_completo %>%
  anti_join(fechas_registradas, by = "Fecha") %>%
  arrange(Fecha)


# ------------------------------------------------------------------------------
# 5. Creacion de registros faltantes e imputacion de Precio
# ------------------------------------------------------------------------------

series_provincia_carburante <- df_sin_adblue %>%
  distinct(
    `Comunidad Autonoma`,
    Id_Provincia,
    Provincia,
    Carburante
  )

# Se crean filas solo para combinaciones Provincia-Carburante ya existentes.
registros_faltantes <- series_provincia_carburante %>%
  crossing(fechas_faltantes) %>%
  mutate(
    Mes = month(Fecha),
    Dia = dias_semana_es[wday(Fecha, week_start = 1)],
    Numero_Semana = isoweek(Fecha),
    Precio = NA_real_
  ) %>%
  select(
    `Comunidad Autonoma`,
    Id_Provincia,
    Provincia,
    Carburante,
    Fecha,
    Mes,
    Dia,
    Numero_Semana,
    Precio
  )

imputar_precio_lineal <- function(fechas, precios) {
  # Recibe una serie Provincia-Carburante y usa precios reales como puntos base.
  indices_validos <- which(!is.na(precios))

  # Sin precios reales no hay base para imputar.
  if (length(indices_validos) == 0) {
    return(precios)
  }

  # Con un solo precio real, se replica ese valor en los NA de la serie.
  if (length(indices_validos) == 1) {
    return(ifelse(is.na(precios), precios[indices_validos], precios))
  }

  # Interpolacion lineal por distancia temporal; rule = 2 cubre extremos.
  precios_interpolados <- approx(
    x = as.numeric(fechas[indices_validos]),
    y = precios[indices_validos],
    xout = as.numeric(fechas),
    method = "linear",
    rule = 2
  )$y

  # Solo se sustituyen NA; el redondeo global se aplica antes de exportar.
  ifelse(is.na(precios), precios_interpolados, precios)
}

# Se imputan precios dentro de cada serie Provincia-Carburante.
df_imputado <- bind_rows(
  df_sin_adblue,
  registros_faltantes
) %>%
  group_by(
    `Comunidad Autonoma`,
    Id_Provincia,
    Provincia,
    Carburante
  ) %>%
  arrange(Fecha, .by_group = TRUE) %>%
  mutate(
    Precio = imputar_precio_lineal(Fecha, Precio)
  ) %>%
  ungroup() %>%
  mutate(
    Precio = round(Precio, 3)
  ) %>%
  arrange(
    `Comunidad Autonoma`,
    Id_Provincia,
    Provincia,
    Carburante,
    Fecha
  )

# Registros insertados que se imprimen por consola como trazabilidad.
registros_insertados <- df_imputado %>%
  semi_join(
    registros_faltantes %>%
      select(Id_Provincia, Provincia, Carburante, Fecha),
    by = c("Id_Provincia", "Provincia", "Carburante", "Fecha")
  ) %>%
  arrange(Fecha, Id_Provincia, Carburante)


# ------------------------------------------------------------------------------
# 6. Exportacion del dataset actualizado
# ------------------------------------------------------------------------------

write_csv(
  x = df_imputado,
  file = ruta_dataset,
  na = ""
)


# ------------------------------------------------------------------------------
# 7. Mensajes de control en consola
# ------------------------------------------------------------------------------

cat("\n================ IMPUTACION DE FECHAS FALTANTES ================\n")
cat("Archivo actualizado:", ruta_dataset, "\n")
cat("Registros iniciales:", registros_iniciales, "\n")
cat("Rango de fechas:", as.character(fecha_inicial), "a", as.character(fecha_final), "\n")
cat("Dias del periodo:", dias_periodo, "\n")
cat("Provincias disponibles:", provincias_disponibles, "\n")
cat("Registros posibles para Adblue:", registros_posibles_adblue, "\n")
cat(
  "Cobertura de Adblue:",
  registros_adblue,
  "registros de",
  registros_posibles_adblue,
  "posibles =",
  porcentaje_cobertura_adblue,
  "%.\n"
)
cat("Porcentaje faltante estimado para Adblue:", porcentaje_faltante_adblue, "%.\n")
cat("Registros eliminados con Carburante = Adblue:", registros_eliminados, "\n")
cat("\nDias faltantes encontrados:\n")
print(fechas_faltantes)
cat("\nRegistros minimos Fecha-Carburante faltantes:", nrow(fechas_faltantes) * 7, "\n")
cat("Registros insertados Provincia-Carburante-Fecha:", nrow(registros_insertados), "\n")
cat("\nRegistros insertados al dataset:\n")
print(registros_insertados, n = nrow(registros_insertados))
cat("\nColumna Precio redondeada a 3 decimales antes de exportar.\n")
cat("\nRegistros finales:", nrow(df_imputado), "\n")
cat("===============================================================\n")
