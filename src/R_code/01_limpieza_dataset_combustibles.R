library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(lubridate)

# ==============================================================================
# Script: 01_limpieza_dataset_combustibles.R
# Objetivo:
#   Realizar la primera fase de limpieza del dataset historico de precios de
#   combustibles en Espana, usando un catalogo oficial de provincias para
#   normalizar la columna Provincia y agregar Id_Provincia.
#
# Entradas:
#   Dataset_Raw/Historico_precios_combustibles_España.csv
#   docs/Codes_Provincia/Provincia_ids.csv
#
# Salidas:
#   Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv
#   Dataset_Processed/Dataset_aux/palabras_erroneas_detectadas.csv
#   Dataset_Processed/Dataset_aux/diccionario_correccion_texto.csv
#
# Descripcion general:
#   El script carga el dataset raw, valida sus columnas, detecta valores con
#   simbolos no permitidos y genera un diccionario auditable de correccion. Para
#   la columna Provincia, el diccionario se construye comparando los valores
#   detectados con el catalogo docs/Codes_Provincia/Provincia_ids.csv mediante
#   similitud textual. Despues aplica las correcciones, agrega Id_Provincia,
#   crea variables temporales derivadas de Fecha y guarda el dataset limpio en
#   Dataset_Processed.
#
# Fuentes de datos Nombres de Provincias:
#   2026: Instituto Nacional de Estadística
#       (Fuente: https://www.ine.es/daco/daco42/codmun/cod_provincia_estandar.htm)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Definicion de rutas relativas del proyecto
# ------------------------------------------------------------------------------

ruta_entrada <- file.path(
  "Dataset_Raw",
  "Historico_precios_combustibles_España.csv"
)

ruta_provincias <- file.path(
  "docs",
  "Codes_Provincia",
  "Provincia_ids.csv"
)

ruta_salida <- file.path(
  "Dataset_Processed",
  "Historico_precios_combustibles_España_texto_normalizado.csv"
)

ruta_dataset_aux <- file.path(
  "Dataset_Processed",
  "Dataset_aux"
)

ruta_palabras_erroneas <- file.path(
  ruta_dataset_aux,
  "palabras_erroneas_detectadas.csv"
)

ruta_diccionario <- file.path(
  ruta_dataset_aux,
  "diccionario_correccion_texto.csv"
)

dir.create(ruta_dataset_aux, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Carga del dataset y del catalogo de provincias
# ------------------------------------------------------------------------------

datos_originales <- read_csv(
  file = ruta_entrada,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Provincia_ids.csv se lee manualmente porque contiene valores como
# "Balears, Illes". Al separar solo por la primera coma, se conserva el nombre
# completo de la provincia aunque contenga comas internas.
lineas_provincias <- read_lines(
  file = ruta_provincias,
  locale = locale(encoding = "UTF-8")
)

provincias_ids <- tibble(linea = lineas_provincias[-1]) %>%
  mutate(
    Id_Provincia = str_match(linea, "^([^,]+),(.*)$")[, 2],
    Provincia = str_match(linea, "^([^,]+),(.*)$")[, 3]
  ) %>%
  select(Id_Provincia, Provincia) %>%
  filter(!is.na(Id_Provincia), !is.na(Provincia))

filas_originales <- nrow(datos_originales)

# ------------------------------------------------------------------------------
# 3. Validacion de columnas esperadas
# ------------------------------------------------------------------------------

columnas_esperadas <- c(
  "Comunidad Autonoma",
  "Provincia",
  "Carburante",
  "Fecha",
  "Precio"
)

columnas_faltantes <- setdiff(columnas_esperadas, names(datos_originales))

if (length(columnas_faltantes) > 0) {
  stop(
    "El dataset no contiene las siguientes columnas esperadas: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

columnas_faltantes_provincias <- setdiff(
  c("Id_Provincia", "Provincia"),
  names(provincias_ids)
)

if (length(columnas_faltantes_provincias) > 0) {
  stop(
    "El catalogo de provincias no contiene las siguientes columnas esperadas: ",
    paste(columnas_faltantes_provincias, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 4. Definicion de calendario auxiliar en espanol
# ------------------------------------------------------------------------------

dias_semana_es <- c(
  "lunes",
  "martes",
  "miercoles",
  "jueves",
  "viernes",
  "sabado",
  "domingo"
)

# ------------------------------------------------------------------------------
# 5. Deteccion automatica de simbolos extranos
# ------------------------------------------------------------------------------

# Permitidos:
#   - Letras A-Z y a-z
#   - Numeros 0-9
#   - Punto decimal
#   - Espacio
#   - Separadores habituales del dataset: /, -, (, )
#
# Todo caracter fuera de este patron se marca como simbolo extrano. Se excluyen
# los acentos porque el dataset raw contiene problemas de codificacion y se
# busca una representacion estable para el procesamiento posterior.
patron_simbolos_no_permitidos <- "[^A-Za-z0-9\\. /()\\-]"

datos_como_texto <- datos_originales %>%
  mutate(across(everything(), as.character))

matriz_texto <- as.matrix(datos_como_texto)

detectar_celdas_con_simbolos_extranos <- function(matriz, patron_no_permitido) {
  matriz_logica <- matrix(
    str_detect(as.vector(matriz), patron_no_permitido),
    nrow = nrow(matriz),
    ncol = ncol(matriz),
    dimnames = dimnames(matriz)
  )
  matriz_logica[is.na(matriz_logica)] <- FALSE

  posiciones_con_error <- which(matriz_logica, arr.ind = TRUE)

  if (length(posiciones_con_error) == 0) {
    return(tibble(
      fila = integer(),
      columna = character(),
      valor_celda = character(),
      simbolo_extrano = character(),
      palabra_erronea = character()
    ))
  }

  tibble(
    fila = posiciones_con_error[, "row"],
    columna = colnames(matriz)[posiciones_con_error[, "col"]],
    valor_celda = matriz[posiciones_con_error],
    palabra_erronea = matriz[posiciones_con_error]
  ) %>%
    group_by(columna, valor_celda, palabra_erronea) %>%
    summarise(
      fila = min(fila),
      simbolo_extrano = paste(
        unique(str_extract_all(first(valor_celda), patron_no_permitido)[[1]]),
        collapse = " "
      ),
      .groups = "drop"
    ) %>%
    select(fila, columna, valor_celda, simbolo_extrano, palabra_erronea) %>%
    arrange(columna, palabra_erronea)
}

detecciones_simbolos_extranos <- detectar_celdas_con_simbolos_extranos(
  matriz = matriz_texto,
  patron_no_permitido = patron_simbolos_no_permitidos
)

columnas_con_simbolos_extranos <- detecciones_simbolos_extranos %>%
  distinct(columna) %>%
  arrange(columna)

palabras_erroneas_detectadas <- detecciones_simbolos_extranos %>%
  select(columna, simbolo_extrano, palabra_erronea) %>%
  distinct() %>%
  arrange(columna, palabra_erronea)

total_valores_con_codificacion_danada <- nrow(detecciones_simbolos_extranos)

# Este archivo documenta que valores fueron detectados con simbolos extranos.
# Sirve como evidencia auditable antes de aplicar el diccionario de correccion.
write_csv(
  x = palabras_erroneas_detectadas,
  file = ruta_palabras_erroneas,
  na = ""
)

# ------------------------------------------------------------------------------
# 6. Funciones auxiliares para normalizar y comparar texto
# ------------------------------------------------------------------------------

normalizar_para_comparacion <- function(texto) {
  texto_limpio <- texto %>%
    str_replace_all(fixed("�"), "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

  texto_limpio %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]", " ") %>%
    str_squish() %>%
    str_trim()
}

normalizar_texto_categorico <- function(texto) {
  texto_limpio <- texto %>%
    str_replace_all(fixed("�"), "") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

  texto_limpio %>%
    str_replace_all("[[:cntrl:]]", " ") %>%
    str_replace_all("[^A-Za-z0-9 /()\\-]", " ") %>%
    str_squish() %>%
    str_trim()
}

corregir_restos_codificacion <- function(texto) {
  texto %>%
    str_replace_all(fixed("Pas Vasco"), "Pais Vasco") %>%
    str_replace_all(fixed("Gasleo A habitual"), "Gasoleo A habitual") %>%
    str_replace_all(fixed("Gasleo Premium"), "Gasoleo Premium") %>%
    str_replace_all(
      fixed("Gases licuados del petrleo"),
      "Gases licuados del petroleo"
    )
}

corregir_provincia_codificacion <- function(texto) {
  clave_provincia <- normalizar_para_comparacion(texto)

  case_when(
    clave_provincia == "CORUA A" ~ "A Coruña",
    clave_provincia == "CRDOBA" ~ "Córdoba",
    clave_provincia == "BALEARS ILLES" ~ "Islas Baleares",
    clave_provincia == "ARABA LAVA" ~ "Álava",
    clave_provincia == "CASTELLN CASTELL" ~ "Castellón",
    clave_provincia == "VALENCIA VALNCIA" ~ "Valencia",
    clave_provincia == "ALMERA" ~ "Almería",
    clave_provincia == "CCERES" ~ "Cáceres",
    clave_provincia == "CDIZ" ~ "Cádiz",
    clave_provincia == "JAN" ~ "Jaén",
    clave_provincia == "LEN" ~ "León",
    clave_provincia == "MLAGA" ~ "Málaga",
    clave_provincia == "VILA" ~ "Ávila",
    TRUE ~ texto
  )
}

normalizar_tokens_ordenados <- function(texto) {
  texto_normalizado <- normalizar_para_comparacion(texto)

  vapply(
    str_split(texto_normalizado, "\\s+"),
    function(tokens) {
      tokens <- tokens[tokens != ""]

      if (length(tokens) == 0) {
        return("")
      }

      paste(sort(unique(tokens)), collapse = " ")
    },
    character(1)
  )
}

buscar_provincia_mas_parecida <- function(provincia_erronea, catalogo) {
  provincia_corregida <- corregir_provincia_codificacion(provincia_erronea)
  correccion_manual <- !identical(provincia_corregida, provincia_erronea)

  provincia_base <- normalizar_para_comparacion(provincia_corregida)
  provincia_base_tokens <- normalizar_tokens_ordenados(provincia_corregida)
  tokens_base <- str_split(provincia_base, "\\s+")[[1]]
  tokens_base <- tokens_base[tokens_base != ""]

  indice_mejor_match <- which(catalogo$Provincia_Normalizada == provincia_base)
  metodo_match <- if (correccion_manual) {
    "regla_manual_codificacion"
  } else {
    "coincidencia_exacta"
  }
  distancia_match <- 0

  if (length(indice_mejor_match) == 0) {
    indice_mejor_match <- which(
      catalogo$Provincia_Tokens_Ordenados == provincia_base_tokens
    )
    metodo_match <- "coincidencia_tokens_ordenados"
  }

  if (length(indice_mejor_match) == 0 && length(tokens_base) > 0) {
    indice_mejor_match <- which(
      vapply(
        catalogo$Provincia_Tokens,
        function(tokens_catalogo) all(tokens_base %in% tokens_catalogo),
        logical(1)
      )
    )
    metodo_match <- "tokens_raw_contenidos_en_catalogo"
  }

  if (length(indice_mejor_match) > 1) {
    distancias_candidatos <- adist(
      provincia_base,
      catalogo$Provincia_Normalizada[indice_mejor_match],
      ignore.case = TRUE
    )
    posicion_mejor_candidato <- which.min(distancias_candidatos)
    distancia_match <- as.integer(distancias_candidatos[posicion_mejor_candidato])
    indice_mejor_match <- indice_mejor_match[posicion_mejor_candidato]
  }

  if (length(indice_mejor_match) == 0) {
    metodo_match <- "distancia_levenshtein"
    distancias <- adist(
      provincia_base,
      catalogo$Provincia_Normalizada,
      ignore.case = TRUE
    )
    indice_mejor_match <- which.min(distancias)
    distancia_match <- as.integer(distancias[indice_mejor_match])
  }

  if (metodo_match == "distancia_levenshtein") {
    distancia_maxima <- max(2, floor(nchar(provincia_base) * 0.30))

    if (distancia_match > distancia_maxima) {
      return(tibble(
        palabra_sustituta = NA_character_,
        Id_Provincia = NA_character_,
        provincia_catalogo_normalizada = NA_character_,
        distancia_similitud = distancia_match,
        metodo_match = "sin_match_confiable"
      ))
    }
  }

  if (length(indice_mejor_match) == 1 && metodo_match != "distancia_levenshtein") {
    distancia_match <- as.integer(adist(
      provincia_base,
      catalogo$Provincia_Normalizada[indice_mejor_match],
      ignore.case = TRUE
    ))
  }

  tibble(
    palabra_sustituta = catalogo$Provincia[indice_mejor_match],
    Id_Provincia = catalogo$Id_Provincia[indice_mejor_match],
    provincia_catalogo_normalizada = catalogo$Provincia_Normalizada[indice_mejor_match],
    distancia_similitud = distancia_match,
    metodo_match = metodo_match
  )
}

provincias_ids <- provincias_ids %>%
  mutate(
    Provincia_Normalizada = normalizar_para_comparacion(Provincia),
    Provincia_Tokens_Ordenados = normalizar_tokens_ordenados(Provincia),
    Provincia_Tokens = str_split(Provincia_Normalizada, "\\s+")
  )

provincias_raw_unicas <- datos_originales %>%
  transmute(
    columna = "Provincia",
    palabra_erronea = Provincia
  ) %>%
  distinct() %>%
  arrange(palabra_erronea)

# ------------------------------------------------------------------------------
# 7. Creacion del diccionario de correccion
# ------------------------------------------------------------------------------

# Para Provincia, el diccionario se crea comparando cada valor unico del raw
# contra Provincia_ids.csv y seleccionando el nombre con menor distancia textual.
# Se usan todas las provincias, no solo las que contienen simbolos extranos,
# porque tambien hay diferencias validas de formato como "RIOJA (LA)" frente a
# "La Rioja". Si solo se revisaran simbolos raros, estos casos quedarian sin id.
diccionario_provincias <- provincias_raw_unicas %>%
  rowwise() %>%
  mutate(
    resultado_match = list(
      buscar_provincia_mas_parecida(
        provincia_erronea = palabra_erronea,
        catalogo = provincias_ids
      )
    )
  ) %>%
  tidyr::unnest(resultado_match) %>%
  ungroup() %>%
  select(
    columna,
    palabra_erronea,
    palabra_sustituta,
    Id_Provincia,
    provincia_catalogo_normalizada,
    distancia_similitud,
    metodo_match
  )

# Las demas categorias con simbolos de codificacion se corrigen con reglas
# explicitas porque no existe todavia un catalogo auxiliar equivalente al de
# provincias.
diccionario_otras_categorias <- palabras_erroneas_detectadas %>%
  filter(columna %in% c("Comunidad Autonoma", "Carburante")) %>%
  mutate(
    palabra_erronea_normalizada = palabra_erronea %>%
      normalizar_texto_categorico() %>%
      corregir_restos_codificacion(),
    palabra_sustituta = case_when(
      palabra_erronea == "Arag�n" ~ "Aragon",
      palabra_erronea == "Castilla y Le�n" ~ "Castilla y Leon",
      palabra_erronea == "Catalu�a" ~ "Cataluna",
      palabra_erronea == "Gases licuados del petr�leo" ~ "Gases licuados del petroleo",
      palabra_erronea == "Gas�leo A habitual" ~ "Gasoleo A habitual",
      palabra_erronea == "Gas�leo Premium" ~ "Gasoleo Premium",
      palabra_erronea == "Pa�s Vasco" ~ "Pais Vasco",
      palabra_erronea_normalizada == "Pais Vasco" ~ "Pais Vasco",
      palabra_erronea_normalizada == "Gases licuados del petroleo" ~ "Gases licuados del petroleo",
      palabra_erronea_normalizada == "Gasoleo A habitual" ~ "Gasoleo A habitual",
      palabra_erronea_normalizada == "Gasoleo Premium" ~ "Gasoleo Premium",
      TRUE ~ NA_character_
    ),
    Id_Provincia = NA_character_,
    provincia_catalogo_normalizada = NA_character_,
    distancia_similitud = NA_integer_,
    metodo_match = NA_character_
  ) %>%
  select(
    columna,
    palabra_erronea,
    palabra_sustituta,
    Id_Provincia,
    provincia_catalogo_normalizada,
    distancia_similitud,
    metodo_match
  )

diccionario_correccion <- bind_rows(
  diccionario_provincias,
  diccionario_otras_categorias
) %>%
  distinct(columna, palabra_erronea, palabra_sustituta, .keep_all = TRUE) %>%
  arrange(columna, palabra_erronea)

correcciones_pendientes <- diccionario_correccion %>%
  filter(is.na(palabra_sustituta))

if (nrow(correcciones_pendientes) > 0) {
  warning(
    "Hay valores con simbolos extranos sin sustitucion definida. ",
    "Revisa el archivo de diccionario antes de continuar."
  )
}

# Este archivo se genera para que el usuario pueda auditar y reutilizar las
# reglas de correccion antes de continuar con imputaciones u otros tratamientos.
write_csv(
  x = diccionario_correccion,
  file = ruta_diccionario,
  na = ""
)

aplicar_diccionario_columna <- function(texto, diccionario, nombre_columna) {
  texto_corregido <- texto

  diccionario_aplicable <- diccionario %>%
    filter(columna == nombre_columna, !is.na(palabra_sustituta))

  if (nrow(diccionario_aplicable) == 0) {
    return(texto_corregido)
  }

  for (indice in seq_len(nrow(diccionario_aplicable))) {
    texto_corregido <- str_replace_all(
      string = texto_corregido,
      pattern = fixed(diccionario_aplicable$palabra_erronea[indice]),
      replacement = diccionario_aplicable$palabra_sustituta[indice]
    )
  }

  texto_corregido
}

# ------------------------------------------------------------------------------
# 8. Aplicacion de limpieza sobre columnas categoricas
# ------------------------------------------------------------------------------

datos_texto_normalizado <- datos_originales %>%
  mutate(
    Comunidad_Autonoma_Original = `Comunidad Autonoma`,
    Provincia_Original = Provincia,
    Carburante_Original = Carburante,
    `Comunidad Autonoma` = `Comunidad Autonoma` %>%
      aplicar_diccionario_columna(diccionario_correccion, "Comunidad Autonoma") %>%
      normalizar_texto_categorico() %>%
      corregir_restos_codificacion(),
    Provincia = Provincia %>%
      aplicar_diccionario_columna(diccionario_correccion, "Provincia") %>%
      normalizar_texto_categorico(),
    Carburante = Carburante %>%
      aplicar_diccionario_columna(diccionario_correccion, "Carburante") %>%
      normalizar_texto_categorico() %>%
      corregir_restos_codificacion()
  ) %>%
  mutate(
    `Comunidad Autonoma` = if_else(
      is.na(`Comunidad Autonoma`) | str_trim(`Comunidad Autonoma`) == "",
      normalizar_texto_categorico(Comunidad_Autonoma_Original),
      `Comunidad Autonoma`
    ),
    Provincia = if_else(
      is.na(Provincia) | str_trim(Provincia) == "",
      normalizar_texto_categorico(Provincia_Original),
      Provincia
    ),
    Carburante = if_else(
      is.na(Carburante) | str_trim(Carburante) == "",
      normalizar_texto_categorico(Carburante_Original),
      Carburante
    )
  )

# La columna Id_Provincia se agrega desde Provincia_ids.csv. Para evitar fallos
# por diferencias de mayusculas, tildes o espacios, el cruce se hace con una
# version normalizada auxiliar de la provincia.
datos_texto_normalizado <- datos_texto_normalizado %>%
  mutate(Provincia_Normalizada = normalizar_para_comparacion(Provincia)) %>%
  left_join(
    provincias_ids %>%
      select(Id_Provincia, Provincia, Provincia_Normalizada) %>%
      rename(Provincia_Catalogo = Provincia),
    by = "Provincia_Normalizada"
  ) %>%
  mutate(
    Provincia = coalesce(Provincia_Catalogo, Provincia),
    .after = `Comunidad Autonoma`
  ) %>%
  select(
    `Comunidad Autonoma`,
    Id_Provincia,
    Provincia,
    Carburante,
    Fecha,
    Precio,
    everything(),
    -Provincia_Normalizada,
    -Provincia_Catalogo,
    -Comunidad_Autonoma_Original,
    -Provincia_Original,
    -Carburante_Original
  )

# ------------------------------------------------------------------------------
# 9. Creacion de columnas derivadas de Fecha
# ------------------------------------------------------------------------------

# Las columnas Mes, Dia y Numero_Semana se crean despues de limpiar las variables
# categoricas y agregar Id_Provincia. De esta forma, el dataset final conserva la
# fecha original y tambien incorpora atributos temporales utiles para analisis,
# agrupaciones y visualizaciones posteriores.
datos_texto_normalizado <- datos_texto_normalizado %>%
  mutate(
    Fecha = parse_date_time(
      Fecha,
      orders = c("dmy", "ymd")
    ) %>%
      as_date(),
    Mes = month(Fecha),
    Dia = dias_semana_es[wday(Fecha, week_start = 1)],
    Numero_Semana = isoweek(Fecha),
    .after = Fecha
  )

fechas_no_convertidas <- datos_texto_normalizado %>%
  filter(is.na(Fecha)) %>%
  summarise(total = n()) %>%
  pull(total)

filas_finales <- nrow(datos_texto_normalizado)
provincias_sin_id <- datos_texto_normalizado %>%
  filter(is.na(Id_Provincia)) %>%
  distinct(Provincia)

celdas_categoricas_vacias <- datos_texto_normalizado %>%
  summarise(
    comunidad_autonoma_vacia = sum(
      is.na(`Comunidad Autonoma`) | str_trim(`Comunidad Autonoma`) == ""
    ),
    provincia_vacia = sum(is.na(Provincia) | str_trim(Provincia) == ""),
    carburante_vacio = sum(is.na(Carburante) | str_trim(Carburante) == "")
  )

if (sum(celdas_categoricas_vacias) > 0) {
  warning(
    "Hay celdas vacias en columnas categoricas despues de limpiar. ",
    "Revisa el resumen impreso en consola."
  )
}

if (nrow(provincias_sin_id) > 0) {
  warning(
    "Hay provincias sin Id_Provincia despues del cruce con el catalogo. ",
    "Revisa la salida impresa en consola y el diccionario de correccion."
  )
}

# ------------------------------------------------------------------------------
# 10. Guardado del dataset con texto normalizado, Id_Provincia y fechas derivadas
# ------------------------------------------------------------------------------

write_csv(
  x = datos_texto_normalizado,
  file = ruta_salida,
  na = ""
)

# ------------------------------------------------------------------------------
# 11. Mensajes de control en consola
# ------------------------------------------------------------------------------

cat("\n================ LIMPIEZA DE TEXTO ================\n")
cat("Archivo de entrada:", ruta_entrada, "\n")
cat("Catalogo de provincias:", ruta_provincias, "\n")
cat("Filas originales:", filas_originales, "\n")
cat("Filas finales:", filas_finales, "\n")
cat(
  "Valores unicos con simbolos extranos detectados antes de limpiar:",
  total_valores_con_codificacion_danada,
  "\n"
)

cat("\nColumnas con simbolos extranos detectados:\n")
print(columnas_con_simbolos_extranos)

cat("\nPrimeros valores erroneos detectados:\n")
print(head(palabras_erroneas_detectadas, 20))

cat("\nDiccionario de correccion generado:\n")
print(diccionario_correccion)

cat("\nProvincias sin Id_Provincia despues del cruce:\n")
print(provincias_sin_id)

cat("\nCeldas vacias en columnas categoricas despues de limpiar:\n")
print(celdas_categoricas_vacias)

cat("\nFechas no convertidas correctamente:", fechas_no_convertidas, "\n")

cat("\nArchivo con valores erroneos detectados generado en:\n")
cat(ruta_palabras_erroneas, "\n")

cat("\nArchivo de diccionario de correccion generado en:\n")
cat(ruta_diccionario, "\n")

cat("\nTipos de datos despues de la normalizacion de texto:\n")
print(glimpse(datos_texto_normalizado))

cat("\nPrimeras filas del dataset con texto normalizado:\n")
print(head(datos_texto_normalizado))

cat("\nArchivo final generado en:", ruta_salida, "\n")
cat("====================================================\n")
