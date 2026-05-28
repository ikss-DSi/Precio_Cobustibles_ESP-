library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# ==============================================================================
# Script: 04_tratamiento_valores_extremos.R
# Objetivo:
#   Identificar y tratar valores extremos en el precio por litro del dataset final
#   de combustibles, usando rangos Año-Mes por Provincia y Carburante.
#
# Entradas:
#   Dataset_Processed/Historico_precios_combustibles_España.csv
#
# Salidas:
#   Dataset_Processed/Historico_precios_combustibles_España_outliers_tratados.csv
#   Dataset_Processed/Dataset_aux/outliers_precio_litro_detectados.csv
#   Dataset_Processed/Dataset_aux/resumen_outliers_precio_litro.csv
#   docs/figures/outliers/*.png
#
#
# Descripcion general:
#   El script analiza Precio_Litro_Euros por Provincia-Carburante-AñoMes.
#   Los outliers se detectan con el metodo IQR mensual y se imputan mediante
#   interpolacion lineal dentro de cada serie Provincia-Carburante.
#
#   Las columnas auxiliares de outliers se usan para auditar el tratamiento. El
#   dataset final conserva solo las variables analiticas originales y el precio
#   tratado.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Definicion de rutas relativas del proyecto
# ------------------------------------------------------------------------------

ruta_dataset <- "../../Dataset_Processed/Historico_precios_combustibles_España.csv"

ruta_salida_dataset <- "../../Dataset_Processed/Historico_precios_combustibles_España_outliers_tratados.csv"

ruta_dataset_aux <- "../../Dataset_Processed/Dataset_aux"

ruta_salida_outliers <- file.path(
  ruta_dataset_aux,
  "outliers_precio_litro_detectados.csv"
)

ruta_salida_resumen <- file.path(
  ruta_dataset_aux,
  "resumen_outliers_precio_litro.csv"
)

ruta_figuras <- "../../docs/figures/outliers"

dir.create(ruta_dataset_aux, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# 2. Carga del dataset y validacion de columnas necesarias
# ------------------------------------------------------------------------------

df <- read_csv(
  file = ruta_dataset,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

columnas_esperadas <- c(
  "Comunidad_Autonoma",
  "ID_Provincia",
  "Provincia",
  "Carburante",
  "Fecha",
  "Precio_Litro_Euros"
)

columnas_faltantes <- setdiff(columnas_esperadas, names(df))

if (length(columnas_faltantes) > 0) {
  stop(
    "El dataset no contiene las siguientes columnas esperadas: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

df <- df %>%
  mutate(
    Fecha = as.Date(Fecha),
    Precio_Litro_Euros = parse_number(
      as.character(Precio_Litro_Euros),
      locale = locale(decimal_mark = ".")
    ),
    Anio_Mes = floor_date(Fecha, unit = "month")
  )


# ------------------------------------------------------------------------------
# 3. Deteccion de outliers por Provincia-Carburante-AñoMes
# ------------------------------------------------------------------------------

# Esta funcion permite calcular percentiles aunque el grupo tenga valores NA.
calcular_percentil <- function(x, probabilidad) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  as.numeric(quantile(x, probabilidad, na.rm = TRUE, names = FALSE))
}

# Estadisticos auxiliares:
# Q1, Q3 e IQR_Mensual definen el rango esperado de precio por grupo.
estadisticos_mensuales <- df %>%
  # Anio_Mes incluye año y mes; evita comparar enero 2020 con enero 2026.
  group_by(Comunidad_Autonoma, ID_Provincia, Provincia, Carburante, Anio_Mes) %>%
  summarise(
    Registros_Mes = n(),
    Q1 = calcular_percentil(Precio_Litro_Euros, 0.25),
    Q3 = calcular_percentil(Precio_Litro_Euros, 0.75),
    IQR_Mensual = Q3 - Q1,
    Limite_Inferior = pmax(0, Q1 - 1.5 * IQR_Mensual),
    Limite_Superior = Q3 + 1.5 * IQR_Mensual,
    .groups = "drop"
  )

# Columnas auxiliares de deteccion:
# indican si un precio es invalido, outlier mensual o valor aceptado.
df_outliers <- df %>%
  left_join(
    estadisticos_mensuales,
    by = c(
      "Comunidad_Autonoma",
      "ID_Provincia",
      "Provincia",
      "Carburante",
      "Anio_Mes"
    )
  ) %>%
  mutate(
    Precio_No_Valido = is.na(Precio_Litro_Euros) | Precio_Litro_Euros <= 0,
    Outlier_IQR_Mensual = Registros_Mes >= 10 &
      IQR_Mensual > 0 &
      (
        Precio_Litro_Euros < Limite_Inferior |
          Precio_Litro_Euros > Limite_Superior
      ),
    Outlier_Final = Precio_No_Valido | Outlier_IQR_Mensual,
    Metodo_Outlier = case_when(
      Precio_No_Valido ~ "precio_no_valido",
      Outlier_IQR_Mensual ~ "iqr_mensual_provincia_carburante",
      TRUE ~ "sin_outlier"
    )
  )


# ------------------------------------------------------------------------------
# 4. Imputacion de valores extremos
# ------------------------------------------------------------------------------

# Imputa NA por interpolacion lineal en la serie temporal recibida.
# Se usa despues de convertir outliers en NA dentro de cada serie.
imputar_precio_lineal <- function(fechas, precios) {
  indices_validos <- which(!is.na(precios))

  if (length(indices_validos) == 0) {
    return(precios)
  }

  puntos_validos <- tibble(
    Fecha = fechas[indices_validos],
    Precio = precios[indices_validos]
  ) %>%
    group_by(Fecha) %>%
    summarise(Precio = mean(Precio, na.rm = TRUE), .groups = "drop") %>%
    arrange(Fecha)

  if (nrow(puntos_validos) == 1) {
    return(ifelse(is.na(precios), puntos_validos$Precio[1], precios))
  }

  precios_interpolados <- approx(
    x = as.numeric(puntos_validos$Fecha),
    y = puntos_validos$Precio,
    xout = as.numeric(fechas),
    method = "linear",
    rule = 2
  )$y

  ifelse(is.na(precios), precios_interpolados, precios)
}

# Los outliers se tratan como NA y se imputan dentro de su serie temporal.
# Precio_Litro_Euros_Original conserva el dato fuente para auditoria.
# Precio_Litro_Euros_Tratado almacena el valor despues del tratamiento.
df_tratado <- df_outliers %>%
  mutate(
    Precio_Litro_Euros_Original = Precio_Litro_Euros,
    Precio_Base_Imputacion = if_else(
      Outlier_Final,
      NA_real_,
      Precio_Litro_Euros
    )
  ) %>%
  group_by(Comunidad_Autonoma, ID_Provincia, Provincia, Carburante) %>%
  arrange(Fecha, .by_group = TRUE) %>%
  mutate(
    Precio_Litro_Euros_Tratado = imputar_precio_lineal(
      Fecha,
      Precio_Base_Imputacion
    ),
    Precio_Litro_Euros_Tratado = round(Precio_Litro_Euros_Tratado, 3),
    Precio_Litro_Euros = Precio_Litro_Euros_Tratado
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(
        Precio_Litro_Euros,
        Precio_Litro_Euros_Original,
        Precio_Litro_Euros_Tratado,
        Q1,
        Q3,
        IQR_Mensual,
        Limite_Inferior,
        Limite_Superior,
        Precio_Barril_Petroleo_Dolares
      ),
      ~ round(.x, 3)
    )
  ) %>%
  select(-Precio_Base_Imputacion)

# Dataset final para analisis:
# conserva solo variables analiticas y usa Precio_Litro_Euros ya tratado.
df_final <- df_tratado %>%
  select(
    Comunidad_Autonoma, ID_Provincia, Provincia, Carburante,
    Fecha, Mes, Dia_Semana, Numero_Semana,
    Precio_Litro_Euros,
    ID, Festivo, Dependencia_Petroleo, Precio_Barril_Petroleo_Dolares
  )


# ------------------------------------------------------------------------------
# 5. Generacion de reportes
# ------------------------------------------------------------------------------

outliers_detectados <- df_tratado %>%
  filter(Outlier_Final) %>%
  select(
    Comunidad_Autonoma,
    ID_Provincia,
    Provincia,
    Carburante,
    Fecha,
    Anio_Mes,
    Precio_Litro_Euros_Original,
    Precio_Litro_Euros_Tratado,
    Q1,
    Q3,
    IQR_Mensual,
    Limite_Inferior,
    Limite_Superior,
    Metodo_Outlier
  ) %>%
  arrange(Carburante, Provincia, Fecha)

resumen_outliers <- df_tratado %>%
  group_by(Carburante) %>%
  summarise(
    Registros = n(),
    Outliers_Detectados = sum(Outlier_Final, na.rm = TRUE),
    Porcentaje_Outliers = round(Outliers_Detectados / Registros * 100, 3),
    Precio_Minimo_Original = min(Precio_Litro_Euros_Original, na.rm = TRUE),
    Precio_Maximo_Original = max(Precio_Litro_Euros_Original, na.rm = TRUE),
    Precio_Minimo_Tratado = min(Precio_Litro_Euros_Tratado, na.rm = TRUE),
    Precio_Maximo_Tratado = max(Precio_Litro_Euros_Tratado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Outliers_Detectados))


# ------------------------------------------------------------------------------
# 6. Generacion de graficos de justificacion
# ------------------------------------------------------------------------------

tema_grafico_claro <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#d9d9d9"),
    panel.grid.minor = element_line(color = "#eeeeee"),
    axis.text = element_text(color = "#222222"),
    axis.title = element_text(color = "#222222"),
    plot.title = element_text(color = "#111111", face = "bold"),
    plot.subtitle = element_text(color = "#333333")
  )

normalizar_nombre_archivo <- function(texto) {
  texto %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("^_|_$", "", .)
}

grafico_boxplot <- ggplot(
  df_tratado,
  aes(x = Carburante, y = Precio_Litro_Euros_Original)
) +
  geom_boxplot(
    fill = "#f4f4f4",
    color = "#333333",
    outlier.color = "#b33a3a",
    outlier.alpha = 0.35,
    outlier.size = 0.8
  ) +
  coord_flip() +
  labs(
    title = "Distribucion global del precio por litro",
    subtitle = "Boxplot por carburante, sin componente temporal",
    x = "Carburante",
    y = "Precio por litro en euros"
  ) +
  tema_grafico_claro

ggsave(
  filename = file.path(ruta_figuras, "boxplot_precio_litro_por_carburante.png"),
  plot = grafico_boxplot,
  width = 10,
  height = 6,
  dpi = 150,
  bg = "white"
)

evolucion_mensual <- df_tratado %>%
  group_by(Anio_Mes, Carburante) %>%
  summarise(
    Precio_Mediano_Sin_Tratar = median(
      Precio_Litro_Euros_Original,
      na.rm = TRUE
    ),
    Precio_Mediano_Tratado = median(
      Precio_Litro_Euros_Tratado,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Precio_Mediano_Sin_Tratar, Precio_Mediano_Tratado),
    names_to = "Tipo_Precio",
    values_to = "Precio_Mediano_Mensual"
  ) %>%
  mutate(
    Tipo_Precio = recode(
      Tipo_Precio,
      Precio_Mediano_Sin_Tratar = "Sin tratar",
      Precio_Mediano_Tratado = "Tratado"
    )
  )

grafico_evolucion <- ggplot(
  evolucion_mensual,
  aes(x = Anio_Mes, y = Precio_Mediano_Mensual, color = Tipo_Precio)
) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~Carburante, scales = "free_y") +
  scale_color_manual(
    values = c("Sin tratar" = "#2f6f8f", "Tratado" = "#b33a3a")
  ) +
  labs(
    title = "Evolucion temporal del precio por litro",
    subtitle = "Mediana mensual por carburante: sin tratar vs tratado",
    x = "Mes",
    y = "Precio mediano mensual en euros",
    color = "Serie"
  ) +
  tema_grafico_claro

ggsave(
  filename = file.path(ruta_figuras, "evolucion_mediana_mensual_por_carburante.png"),
  plot = grafico_evolucion,
  width = 12,
  height = 8,
  dpi = 150,
  bg = "white"
)

for (carburante_actual in sort(unique(df_tratado$Carburante))) {
  datos_carburante <- df_tratado %>%
    filter(Carburante == carburante_actual)

  lineas_carburante <- datos_carburante %>%
    group_by(Anio_Mes) %>%
    summarise(
      Precio_Mediano_Sin_Tratar = median(
        Precio_Litro_Euros_Original,
        na.rm = TRUE
      ),
      Precio_Mediano_Tratado = median(
        Precio_Litro_Euros_Tratado,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(Precio_Mediano_Sin_Tratar, Precio_Mediano_Tratado),
      names_to = "Tipo_Precio",
      values_to = "Precio_Mediano_Mensual"
    ) %>%
    mutate(
      Tipo_Precio = recode(
        Tipo_Precio,
        Precio_Mediano_Sin_Tratar = "Sin tratar",
        Precio_Mediano_Tratado = "Tratado"
      )
    )

  grafico_carburante <- ggplot(datos_carburante, aes(x = Anio_Mes)) +
    geom_boxplot(
      aes(
        group = Anio_Mes,
        y = Precio_Litro_Euros_Original
      ),
      fill = "#f4f4f4",
      color = "#777777",
      outlier.alpha = 0.15,
      width = 20
    ) +
    geom_line(
      data = lineas_carburante,
      aes(
        y = Precio_Mediano_Mensual,
        color = Tipo_Precio
      ),
      linewidth = 0.8
    ) +
    scale_color_manual(
      values = c("Sin tratar" = "#2f6f8f", "Tratado" = "#b33a3a")
    ) +
    labs(
      title = paste("Outliers mensuales y evolucion -", carburante_actual),
      subtitle = "Boxplot mensual original, linea azul sin tratar y linea roja tratada",
      x = "Año-Mes",
      y = "Precio por litro en euros",
      color = "Serie"
    ) +
    tema_grafico_claro

  ggsave(
    filename = file.path(
      ruta_figuras,
      paste0(
        "mensual_boxplot_lineas_",
        normalizar_nombre_archivo(carburante_actual),
        ".png"
      )
    ),
    plot = grafico_carburante,
    width = 12,
    height = 7,
    dpi = 150,
    bg = "white"
  )
}

grafico_resumen <- ggplot(
  resumen_outliers,
  aes(x = reorder(Carburante, Outliers_Detectados), y = Outliers_Detectados)
) +
  geom_col(fill = "#2f6f8f") +
  coord_flip() +
  labs(
    title = "Outliers detectados por carburante",
    x = "Carburante",
    y = "Numero de outliers"
  ) +
  tema_grafico_claro

ggsave(
  filename = file.path(ruta_figuras, "outliers_detectados_por_carburante.png"),
  plot = grafico_resumen,
  width = 10,
  height = 6,
  dpi = 150,
  bg = "white"
)

if (nrow(outliers_detectados) > 0) {
  series_con_registros <- df_tratado %>%
    count(
      Comunidad_Autonoma,
      ID_Provincia,
      Provincia,
      Carburante,
      name = "Registros_Serie"
    )

  serie_ejemplo_gasolina <- outliers_detectados %>%
    filter(Carburante == "Gasolina 95 E5") %>%
    count(
      Comunidad_Autonoma,
      ID_Provincia,
      Provincia,
      Carburante,
      sort = TRUE,
      name = "Outliers_Serie"
    ) %>%
    left_join(
      series_con_registros,
      by = c("Comunidad_Autonoma", "ID_Provincia", "Provincia", "Carburante")
    ) %>%
    filter(Registros_Serie >= 30) %>%
    slice(1)

  serie_ejemplo <- if (nrow(serie_ejemplo_gasolina) > 0) {
    serie_ejemplo_gasolina
  } else {
    outliers_detectados %>%
      count(
        Comunidad_Autonoma,
        ID_Provincia,
        Provincia,
        Carburante,
        sort = TRUE,
        name = "Outliers_Serie"
      ) %>%
      left_join(
        series_con_registros,
        by = c("Comunidad_Autonoma", "ID_Provincia", "Provincia", "Carburante")
      ) %>%
      filter(Registros_Serie >= 30) %>%
      slice(1)
  }

  fecha_central <- outliers_detectados %>%
    filter(
      Comunidad_Autonoma == serie_ejemplo$Comunidad_Autonoma,
      ID_Provincia == serie_ejemplo$ID_Provincia,
      Provincia == serie_ejemplo$Provincia,
      Carburante == serie_ejemplo$Carburante
    ) %>%
    summarise(Fecha = min(Fecha, na.rm = TRUE)) %>%
    pull(Fecha)

  datos_ejemplo <- df_tratado %>%
    filter(
      Comunidad_Autonoma == serie_ejemplo$Comunidad_Autonoma,
      ID_Provincia == serie_ejemplo$ID_Provincia,
      Provincia == serie_ejemplo$Provincia,
      Carburante == serie_ejemplo$Carburante,
      Fecha >= fecha_central - 45,
      Fecha <= fecha_central + 45
    )

  if (nrow(datos_ejemplo) < 2) {
    datos_ejemplo <- df_tratado %>%
      filter(
        Comunidad_Autonoma == serie_ejemplo$Comunidad_Autonoma,
        ID_Provincia == serie_ejemplo$ID_Provincia,
        Provincia == serie_ejemplo$Provincia,
        Carburante == serie_ejemplo$Carburante
      )
  }

  grafico_serie <- ggplot(datos_ejemplo, aes(x = Fecha)) +
    geom_line(aes(y = Precio_Litro_Euros_Original, group = 1), color = "#2f6f8f") +
    geom_line(aes(y = Precio_Litro_Euros_Tratado, group = 1), color = "#b33a3a") +
    geom_point(
      data = datos_ejemplo %>% filter(Outlier_Final),
      aes(y = Precio_Litro_Euros_Original),
      color = "#111111",
      size = 2
    ) +
    labs(
      title = "Ejemplo de serie con outliers tratados",
      subtitle = paste(
        serie_ejemplo$Provincia,
        "-",
        serie_ejemplo$Carburante,
        "| azul: sin tratar, rojo: tratado"
      ),
      x = "Fecha",
      y = "Precio por litro en euros"
    ) +
    tema_grafico_claro

  ggsave(
    filename = file.path(ruta_figuras, "ejemplo_serie_outliers_tratados.png"),
    plot = grafico_serie,
    width = 10,
    height = 6,
    dpi = 150,
    bg = "white"
  )
}


# ------------------------------------------------------------------------------
# 7. Exportacion de resultados
# ------------------------------------------------------------------------------

write_csv(
  x = df_final,
  file = ruta_salida_dataset,
  na = ""
)

write_csv(
  x = outliers_detectados,
  file = ruta_salida_outliers,
  na = ""
)

write_csv(
  x = resumen_outliers,
  file = ruta_salida_resumen,
  na = ""
)


# ------------------------------------------------------------------------------
# 8. Mensajes de control en consola
# ------------------------------------------------------------------------------

cat("\n================ TRATAMIENTO DE VALORES EXTREMOS ================\n")
cat("Archivo de entrada:", ruta_dataset, "\n")
cat("Dataset tratado:", ruta_salida_dataset, "\n")
cat("Registros analizados:", nrow(df), "\n")
cat("Registros exportados en dataset final:", nrow(df_final), "\n")
cat("Outliers detectados:", nrow(outliers_detectados), "\n")
cat("Grupos mensuales evaluados:", nrow(estadisticos_mensuales), "\n")

cat("\nResumen por carburante:\n")
print(resumen_outliers)

cat("\nPrimeros outliers detectados:\n")
print(head(outliers_detectados, 20))

cat("\nGraficos generados en:", ruta_figuras, "\n")
cat("=================================================================\n")
