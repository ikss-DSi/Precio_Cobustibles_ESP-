library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)

# ==============================================================================
# Script: 03_integracion_festivos.R
# Objetivo:
#   Integrar datasets con los días festivos por provincia con el dataset 
#   general de los años 2023 a 2026 y convertir a festivos los domingos.
#   Se integra también el dataset de precios en Europa del barril de petróleo en
#   dólares.
#   Gestión del tipo de datos de cada atributo.
#
# Entradas:
#   Dataset_Raw/festivos_provincias_2023.csv,
#   Dataset_Raw/festivos_provincias_2024.csv,
#   Dataset_Raw/festivos_provincias_2025.csv,
#   Dataset_Raw/calendario_2026.csv,
#   docs/Codes_Provincia/Provincia_ids.csv,
#   Dataset_Raw/RBRTEd.xls
#   Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv
#
# Salidas:
#   Dataset_Processed/Historico_precios_combustibles_España.csv
#
#
# Descripcion general:
#   El script carga el dataset normalizado y lo integra con los datasets de días
#   Festivos por año y provincia. Una vez integrados, se incluyen en días 
#   festivos los domingos que no se hayan incluído en el paso anterior.
#   Posteriormente se integran los datos de precio del barril de petróleo en 
#   función de la fecha y el tipo de carburante. Se crea la variable
#   Dependencia_Petroleo y se imputan los días sin cotización Brent mediante
#   Fill Forward.
#   Por último, se gestionan los tipos de datos de cada atributo y finalmente se
#   exporta el dataset.
#
# Fuentes de datos de días festivos:
#   2026: Sede electrónica de la Seguridad Social
#   2023-2025: Kaggle: Calendarios Laborales España - Datos JSON 
#              (Fuente: https://miclaendariolaboral.com)
# Fuente de datos de precio del barril de petróleo:
#   Independent Statistics and Analysis. U.s. Energy Information Administration
#   (https://www.eia.gov/dnav/pet/hist/RBRTED.htm)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Definicion de rutas relativas del proyecto
# ------------------------------------------------------------------------------

ruta_dataset <- "../../Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv"

ruta_provincias <- "../../docs/Codes_Provincia/Provincia_ids.csv"

rutas_festivos <- c("festivos_provincias_2023.csv",
                    "festivos_provincias_2024.csv",
                    "festivos_provincias_2025.csv",
                    "calendario_2026.csv")


# ------------------------------------------------------------------------------
# 2. Carga del dataset y del catalogo de provincias
# ------------------------------------------------------------------------------

# Carga del dataset normalizado y creación de ID
df <- read.csv(file = ruta_dataset, fileEncoding = "UTF-8")

df$ID <- paste(df$Id_Provincia,df$Fecha, sep ="-")

# Carga del csv con las provincias, eliminación de acentos, conversión a mayúsculas y creación del campo palabra_mas_larga con la palabra del nombre más larga de cada provincia
provincias <- read.csv(file = ruta_provincias, fileEncoding = "UTF-8")

provincias$Provincia <- gsub("á", "a",
                             gsub("é", "e",
                                  gsub("í", "i",
                                       gsub("ó", "o",
                                            gsub("ú", "u",
                                                 gsub("Á", "A",
                                                      gsub("É", "E",
                                                           gsub("Í", "I",
                                                                gsub("Ó", "O",
                                                                     gsub("Ú", "U", provincias$Provincia))))))))))


provincias$Provincia <- toupper(gsub("[[:punct:]]", " ", provincias$Provincia))

provincias$palabra_mas_larga <- sapply(
  strsplit(provincias$Provincia, "\\s+"),
  function(x) x[which.max(nchar(x))]
)


# ------------------------------------------------------------------------------
# 3. Procesamiento de festivos y carga en df
# ------------------------------------------------------------------------------

# Procesamiento de festivos y carga a df
procesar_festivos <- function(df, df_festivos, provincias) {
  
  # Convertir campos a mayúsculas
  names(df_festivos) <- toupper(names(df_festivos))
  
  # Normalizar fechas
  df_festivos$FECHA <- format(as.Date(df_festivos$FECHA, tryFormats = c("%d/%m/%Y", "%d-%m-%Y")), "%Y-%m-%d")
  
  # Normalizar provincias
  df_festivos$PROVINCIA <- toupper(gsub("[[:punct:]]", " ", df_festivos$PROVINCIA))
  
  df_festivos$PROVINCIA <- str_replace_all(df_festivos$PROVINCIA, c(
    "MALLORCA" = "BALEARES",
    "GIJON" = "ASTURIAS",
    "OVIEDO" = "ASTURIAS"
  ))
  
  # Fiestas nacionales (sin provincia) aplicadas a todas las provincias
  
  vacias <- df_festivos %>%
    filter(PROVINCIA == "" | is.na(PROVINCIA))
  head(vacias)
  
  no_vacias <- df_festivos %>%
    filter(!(PROVINCIA == "" | is.na(PROVINCIA)))
  head(no_vacias)
  
  nacionales <- vacias %>% 
    crossing(provincias) %>%
    mutate(PROVINCIA = Provincia) %>%
    select(-matches("^PROVINCIA\\.[xy]?$"))
  
  df_festivos <- dplyr::bind_rows(no_vacias, nacionales)
  
  # Seleccionar palabra más larga del nombre
  df_festivos$palabra_mas_larga <- sapply(
    strsplit(df_festivos$PROVINCIA, "\\s+"),
    function(x) x[which.max(nchar(x))]
  )
  
  # Generar ID comparando con los códigos de provincia
  
  df_festivos$ID <- paste(
    sapply(df_festivos$palabra_mas_larga, function(x){
      dist <- adist(x, provincias$palabra_mas_larga)
      idx <- which.min(dist)
      provincias$id[idx]
    }), df_festivos$FECHA, sep = "-")
  
  # Completar campo Festivo en tabla df
  
  if (!"Festivo" %in% names(df)) {
    df$Festivo <- ifelse(df$ID %in% df_festivos$ID, 1, 0)
  } else {
    df$Festivo[df$ID %in% df_festivos$ID] <- 1
  }
  return(df)
}

#Ejecución en bucle para los distintos años
for (i in rutas_festivos) {
  ruta_file = paste("../../Dataset_Raw/", i, sep="")
  message("Leyendo " ,i)
  df_festivo <- read.csv(file = ruta_file, fileEncoding = "ISO-8859-1")
  message("Procesando ", i)
  df <- procesar_festivos(df, df_festivo, provincias)
  message(i, " incorporado al dataset")
}

# Conversión de domingos a festivos
df$Festivo[df$Dia == "domingo"] <- 1

# ------------------------------------------------------------------------------
# 4. Integración de precio del barril de petróleo
# ------------------------------------------------------------------------------

# Se integran los precios en dólares del barril de petróleo. Los días sin
# cotización se completan con el último precio disponible anterior.
df_petroleo <- read_excel("../../Dataset_Raw/RBRTEd.xls", sheet = "Data 1", skip = 2)

combustibles_dependencia_alta <- c(
  "Gasolina 95 E5",
  "Gasolina 98 E5",
  "Gasoleo A habitual",
  "Gasoleo Premium"
)

combustibles_dependencia_media <- c(
  "Gases licuados del petroleo"
)

df$Fecha <- as.Date(df$Fecha, format = "%Y-%m-%d")
df_petroleo <- df_petroleo %>%
  rename(Precio_Barril_Petroleo_Dolares = `Europe Brent Spot Price FOB (Dollars per Barrel)`) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Precio_Barril_Petroleo_Dolares) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date)

calendario_petroleo <- tibble(
  Date = seq.Date(
    from = min(df_petroleo$Date, na.rm = TRUE),
    to = max(df$Fecha, na.rm = TRUE),
    by = "day"
  )
)

# Fill Forward es adecuado porque los mercados no cotizan fines de semana.
# El último precio Brent disponible se usa como referencia vigente.
df_petroleo_diario <- calendario_petroleo %>%
  left_join(df_petroleo, by = "Date") %>%
  arrange(Date) %>%
  fill(Precio_Barril_Petroleo_Dolares, .direction = "down") %>%
  filter(Date >= min(df$Fecha, na.rm = TRUE))

df <- df %>%
  mutate(
    Dependencia_Petroleo = case_when(
      Carburante %in% combustibles_dependencia_alta ~ "Alta",
      Carburante %in% combustibles_dependencia_media ~ "Media",
      TRUE ~ "Baja"
    )
  ) %>%
  left_join(df_petroleo_diario, by = c("Fecha" = "Date")) %>%
  mutate(
    Precio_Barril_Petroleo_Dolares = if_else(
      Dependencia_Petroleo %in% c("Alta", "Media"),
      Precio_Barril_Petroleo_Dolares,
      NA_real_
    )
  )


# ------------------------------------------------------------------------------
# 5. Normalización final del formato de fecha
# ------------------------------------------------------------------------------

# El join con RBRTEd.xls puede convertir Fecha a fecha-hora POSIXct.
# Se fuerza Date para exportar el CSV final como YYYY-MM-DD.
df$Fecha <- as.Date(df$Fecha)


# ------------------------------------------------------------------------------
# 6. Gestión del tipo de datos
# ------------------------------------------------------------------------------

# Se convierten las variables Dia, Mes, Festivo, ID_Provincia, Provincia, 
# Comunidad Autónoma, Carburante y Dependencia_Petroleo a factor

df$Dia <- factor(
  df$Dia,
  levels = c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado", "domingo"),
  ordered = TRUE
)

df$Mes <- factor(
  df$Mes,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  labels = c("enero", "febrero", "marzo", "abril", "mayo", "junio",
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"),
  ordered = TRUE
)

df$Festivo <- factor(
  df$Festivo,
  levels = c(0, 1),
  labels = c("No", "Si")
)

df$Id_Provincia <- factor(
  df$Id_Provincia
)

df$Provincia <- factor(
  df$Provincia
)

df$Comunidad.Autonoma <- factor(
  df$Comunidad.Autonoma
)

df$Carburante <- factor(
  df$Carburante
)

df$Dependencia_Petroleo <- factor(
  df$Dependencia_Petroleo,
  levels = c("Baja", "Media", "Alta"),
  ordered = TRUE
)

# Se renombran los atributos necesarios
df <- df %>%
  rename(
    Dia_Semana = Dia,
    Precio_Litro_Euros = Precio,
    Comunidad_Autonoma = Comunidad.Autonoma,
    ID_Provincia = Id_Provincia
  )

# ------------------------------------------------------------------------------
# 7. Exportación del dataset final
# ------------------------------------------------------------------------------

write_csv(
  df,
  "../../Dataset_Processed/Historico_precios_combustibles_España.csv",
  na = ""
)
message("Dataset final exportado en Dataset_Processed")
