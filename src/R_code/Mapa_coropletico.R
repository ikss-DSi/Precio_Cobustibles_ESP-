# ==============================================================================
# Script: Mapa_coropletico.R
# Objetivo:
#   Generar una imagen PNG con un mapa coropletico de precios medios de
#   combustibles por provincia, segun rango de fechas y carburante.
#
# Uso desde la raiz del proyecto:
#   Rscript src/R_code/Mapa_coropletico.R --start 01-01-2020 --end 03-04-2020 --carburante 1
#
# Parametros:
#   --start       Fecha inicial en formato DD-MM-YYYY.
#   --end         Fecha final en formato DD-MM-YYYY.
#   --carburante  Numero del carburante:
#                 1 - Gas natural licuado
#                 2 - Gases licuados del petroleo
#                 3 - Gasoleo A habitual
#                 4 - Gasoleo Premium
#                 5 - Gasolina 95 E5
#                 6 - Gasolina 98 E5
#                 7 - Gas natural comprimido
#
# Salida:
#   docs/figures/maps/map_<fecha_inicio>_<fecha_final>_<carburante>.png
#
# Descripcion general:
#   El script lee el dataset tratado, filtra por fechas y carburante, calcula la
#   media de Precio_Litro_Euros por provincia y pinta esa media sobre el mapa.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Validacion de paquetes y rutas
# ------------------------------------------------------------------------------

paquetes_necesarios <- c(
  "sf",
  "dplyr",
  "readr",
  "ggplot2",
  "RColorBrewer",
  "mapSpain",
  "stringr",
  "lubridate"
)

paquetes_faltantes <- paquetes_necesarios[
  !vapply(paquetes_necesarios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(paquetes_faltantes) > 0) {
  stop(
    "Faltan paquetes necesarios. Instalalos con: install.packages(c(",
    paste(sprintf('"%s"', paquetes_faltantes), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(mapSpain)
  library(stringr)
  library(lubridate)
})

rutas_dataset_posibles <- c(
  file.path(
    "Dataset_Processed",
    "Historico_precios_combustibles_España_outliers_tratados.csv"
  ),
  file.path(
    "..",
    "..",
    "Dataset_Processed",
    "Historico_precios_combustibles_España_outliers_tratados.csv"
  )
)

ruta_dataset <- rutas_dataset_posibles[file.exists(rutas_dataset_posibles)][1]

if (is.na(ruta_dataset)) {
  stop(
    "No se encontro el dataset tratado. Ejecuta el script desde la raiz del ",
    "proyecto o desde src/R_code."
  )
}

rutas_figuras_posibles <- c(
  file.path("docs", "figures", "maps"),
  file.path("..", "..", "docs", "figures", "maps")
)

ruta_figuras <- rutas_figuras_posibles[
  dir.exists(dirname(rutas_figuras_posibles))
][1]

if (is.na(ruta_figuras)) {
  ruta_figuras <- file.path("docs", "figures", "maps")
}

dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Catalogos y funciones auxiliares
# ------------------------------------------------------------------------------

carburantes_disponibles <- c(
  "Gas natural licuado",
  "Gases licuados del petroleo",
  "Gasoleo A habitual",
  "Gasoleo Premium",
  "Gasolina 95 E5",
  "Gasolina 98 E5",
  "Gas natural comprimido"
)

mostrar_uso <- function() {
  cat("\nUso:\n")
  cat("  Rscript src/R_code/Mapa_coropletico.R --start 01-01-2020 --end 03-04-2020 --carburante 1\n\n")
  cat("Carburantes disponibles:\n")

  for (indice in seq_along(carburantes_disponibles)) {
    cat(" ", indice, "-", carburantes_disponibles[indice], "\n")
  }

  cat("\n")
}

leer_argumentos <- function(argumentos) {
  if ("--help" %in% argumentos || "-h" %in% argumentos) {
    mostrar_uso()
    quit(status = 0)
  }

  obtener_valor <- function(nombre) {
    posicion <- match(nombre, argumentos)

    if (is.na(posicion) || posicion == length(argumentos)) {
      return(NA_character_)
    }

    argumentos[posicion + 1]
  }

  fecha_inicio <- obtener_valor("--start")
  fecha_fin <- obtener_valor("--end")
  carburante_id <- obtener_valor("--carburante")

  if (any(is.na(c(fecha_inicio, fecha_fin, carburante_id)))) {
    mostrar_uso()
    stop("Faltan parametros obligatorios: --start, --end y --carburante.")
  }

  list(
    fecha_inicio_texto = fecha_inicio,
    fecha_fin_texto = fecha_fin,
    carburante_id = carburante_id
  )
}

parsear_fecha_usuario <- function(fecha_texto, nombre_parametro) {
  fecha <- dmy(fecha_texto, quiet = TRUE)

  if (is.na(fecha)) {
    stop(
      "El parametro ",
      nombre_parametro,
      " debe tener formato DD-MM-YYYY. Valor recibido: ",
      fecha_texto
    )
  }

  as.Date(fecha)
}

normalizar_texto_mapa <- function(texto) {
  texto %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]", " ") %>%
    str_squish() %>%
    str_trim()
}

normalizar_nombre_archivo <- function(texto) {
  texto %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

detectar_columna_codigo_provincia <- function(provincias_sf) {
  candidatos <- c(
    "cpro",
    "cod_prov",
    "codprov",
    "ine.prov.code",
    "ine.prov.cod",
    "prov_code",
    "CC_2"
  )

  columna <- candidatos[candidatos %in% names(provincias_sf)][1]

  if (is.na(columna)) {
    stop(
      "No se encontro una columna de codigo provincial en las geometrias. ",
      "Revisa names(mapSpain::esp_get_prov())."
    )
  }

  columna
}

detectar_columna_nombre_provincia <- function(provincias_sf) {
  candidatos <- c(
    "name",
    "ine.prov.name",
    "nuts.prov.name",
    "NAME_2",
    "Provincia"
  )

  columna <- candidatos[candidatos %in% names(provincias_sf)][1]

  if (is.na(columna)) {
    stop(
      "No se encontro una columna de nombre provincial en las geometrias. ",
      "Revisa names(mapSpain::esp_get_prov())."
    )
  }

  columna
}

cargar_provincias <- function() {
  provincias_sf <- mapSpain::esp_get_prov()
  columna_codigo <- detectar_columna_codigo_provincia(provincias_sf)
  columna_nombre <- detectar_columna_nombre_provincia(provincias_sf)

  provincias_sf %>%
    mutate(
      ID_Provincia = str_pad(
        as.character(.data[[columna_codigo]]),
        width = 2,
        side = "left",
        pad = "0"
      ),
      Provincia_Mapa = as.character(.data[[columna_nombre]]),
      Provincia_Mapa_Normalizada = normalizar_texto_mapa(Provincia_Mapa)
    ) %>%
    select(ID_Provincia, Provincia_Mapa, Provincia_Mapa_Normalizada, geometry) %>%
    st_transform(4326)
}

preparar_dataset <- function(ruta) {
  read_csv(ruta, show_col_types = FALSE) %>%
    mutate(
      Fecha = as.Date(Fecha),
      ID_Provincia = str_pad(
        as.character(as.integer(ID_Provincia)),
        width = 2,
        side = "left",
        pad = "0"
      ),
      Provincia_Normalizada = normalizar_texto_mapa(Provincia),
      Precio_Litro_Euros = parse_number(
        as.character(Precio_Litro_Euros),
        locale = locale(decimal_mark = ".")
      )
    ) %>%
    filter(
      !is.na(Fecha),
      !is.na(Precio_Litro_Euros),
      !is.na(Carburante),
      !is.na(ID_Provincia)
    )
}

# ------------------------------------------------------------------------------
# 3. Lectura de parametros y filtrado de datos
# ------------------------------------------------------------------------------

argumentos <- leer_argumentos(commandArgs(trailingOnly = TRUE))

fecha_inicio <- parsear_fecha_usuario(argumentos$fecha_inicio_texto, "--start")
fecha_fin <- parsear_fecha_usuario(argumentos$fecha_fin_texto, "--end")

if (fecha_inicio > fecha_fin) {
  stop("La fecha inicial no puede ser posterior a la fecha final.")
}

carburante_id <- suppressWarnings(as.integer(argumentos$carburante_id))

if (
  is.na(carburante_id) ||
    carburante_id < 1 ||
    carburante_id > length(carburantes_disponibles)
) {
  mostrar_uso()
  stop("El parametro --carburante debe ser un numero valido del catalogo.")
}

carburante_seleccionado <- carburantes_disponibles[carburante_id]

df_precios <- preparar_dataset(ruta_dataset)

rango_dataset <- range(df_precios$Fecha, na.rm = TRUE)

if (fecha_fin < rango_dataset[1] || fecha_inicio > rango_dataset[2]) {
  stop(
    "El rango solicitado no cruza con el dataset. Rango disponible: ",
    format(rango_dataset[1], "%d-%m-%Y"),
    " a ",
    format(rango_dataset[2], "%d-%m-%Y")
  )
}

datos_filtrados <- df_precios %>%
  filter(
    Fecha >= fecha_inicio,
    Fecha <= fecha_fin,
    Carburante == carburante_seleccionado
  )

if (nrow(datos_filtrados) == 0) {
  stop(
    "No hay registros para el carburante y rango de fechas seleccionados."
  )
}

precios_provincia <- datos_filtrados %>%
  group_by(ID_Provincia, Provincia) %>%
  summarise(
    Precio_Medio = round(mean(Precio_Litro_Euros, na.rm = TRUE), 3),
    Registros = n(),
    .groups = "drop"
  )

provincias <- cargar_provincias()

datos_mapa <- provincias %>%
  left_join(precios_provincia, by = "ID_Provincia") %>%
  mutate(
    Etiqueta = if_else(
      is.na(Precio_Medio),
      paste0(Provincia_Mapa, "\nSin datos"),
      paste0(
        Provincia_Mapa,
        "\n",
        format(Precio_Medio, nsmall = 3),
        " €/L"
      )
    )
  )

# ------------------------------------------------------------------------------
# 4. Generacion del mapa coropletico
# ------------------------------------------------------------------------------

titulo_mapa <- paste0(
  "Precio medio por provincia - ",
  carburante_seleccionado
)

subtitulo_mapa <- paste0(
  "Periodo: ",
  format(fecha_inicio, "%d-%m-%Y"),
  " a ",
  format(fecha_fin, "%d-%m-%Y"),
  " | Provincias con datos: ",
  sum(!is.na(datos_mapa$Precio_Medio)),
  " | Registros: ",
  sum(datos_mapa$Registros, na.rm = TRUE)
)

grafico_mapa <- ggplot(datos_mapa) +
  geom_sf(
    aes(fill = Precio_Medio),
    color = "white",
    linewidth = 0.18
  ) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "YlOrRd"),
    na.value = "#d9d9d9",
    name = "€/L"
  ) +
  labs(
    title = titulo_mapa,
    subtitle = subtitulo_mapa,
    caption = paste0(
      "Fuente: dataset tratado del proyecto. ",
      "Los valores representan medias provinciales."
    )
  ) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "#444444"),
    plot.caption = element_text(size = 9, color = "#666666"),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

nombre_archivo <- paste0(
  "map_",
  format(fecha_inicio, "%d-%m-%Y"),
  "_",
  format(fecha_fin, "%d-%m-%Y"),
  "_",
  normalizar_nombre_archivo(carburante_seleccionado),
  ".png"
)

ruta_salida <- file.path(ruta_figuras, nombre_archivo)

ggsave(
  filename = ruta_salida,
  plot = grafico_mapa,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 5. Mensajes de control
# ------------------------------------------------------------------------------

cat("\n================ MAPA COROPLETICO ================\n")
cat("Dataset:", ruta_dataset, "\n")
cat("Fecha inicial:", format(fecha_inicio, "%d-%m-%Y"), "\n")
cat("Fecha final:", format(fecha_fin, "%d-%m-%Y"), "\n")
cat("Carburante:", carburante_seleccionado, "\n")
cat("Registros usados:", nrow(datos_filtrados), "\n")
cat("Provincias con datos:", sum(!is.na(datos_mapa$Precio_Medio)), "\n")
cat("Imagen generada en:", ruta_salida, "\n")
cat("==================================================\n")
