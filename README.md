# Precio Combustibles ESP

Proyecto para procesar, limpiar y preparar el dataset historico de precios de combustibles en España para su utilización en un análisis estadístico cuyo objetivo es responder la pregunta **¿Qué factores —como el tipo de carburante, el precio internacional del petróleo, la ubicación geográfica y la temporalidad— explican las variaciones del precio de los combustibles en España?**


## Estructura Relevante

```text
Precio_Combustibles_ESP/
├── Dataset_Raw/
│   ├── Historico_precios_combustibles_España.csv
│   ├── RBRTEd.xls
│   ├── festivos_provincias_2023.csv
│   ├── festivos_provincias_2024.csv
│   ├── festivos_provincias_2025.csv
│   └── calendario_2026.csv
├── Dataset_Processed/
│   ├── Historico_precios_combustibles_España.csv
│   ├── Historico_precios_combustibles_España_outliers_tratados.csv
│   ├── Historico_precios_combustibles_España_texto_normalizado.csv
|   └──Dataset_aux/
|       ├── diccionario_correccion_texto.csv
|       ├── outliers_precio_litro_detectados.csv
|       ├── palabras_erroneas_detectadas.csv
|       └── resumen_outliers_precio_litro.csv
├── docs/
│   ├── Codes_Provincia/
│   |   └── Provincia_ids.csv
│   ├── figures/
│   |   ├── analisis/...
│   |   ├── maps/...
│   |   └── outliers/...
│   └── pdf/
├── src/
│   └── R_code/
|       ├── 01_limpieza_dataset_combustibles.R
|       ├── 02_Imputacion_nan_data.R
|       ├── 03_integracion_festivos.R
|       ├── 04_tratamiento_valores_extremos.R
|       ├── 05_analisis_de_datos.R
│       └── Mapa_coropletico.R
├── README.md
└── requirements.txt
```

## Dependencias

El script principal usa R y los siguientes paquetes:

- `readr`
- `dplyr`
- `stringr`
- `janitor`
- `tidyr`
- `lubridate`
- `readxl`
- `ggplot2`
- `sf`
- `RColorBrewer`
- `mapSpain`
- `ranger`
- `caret`
- `factoextra`
- `cluster`
- `dunn.test`
- `car`

El archivo `requirements.txt` lista estos paquetes para documentar las dependencias del proyecto. Aunque el nombre `requirements.txt` suele usarse en Python, en este proyecto se utiliza como referencia simple de paquetes R necesarios.

Para instalarlos desde R o RStudio:

```r
install.packages(c("readr", "dplyr", "stringr", "janitor", "tidyr", "lubridate", "readxl", "ggplot2", "sf", "RColorBrewer", "mapSpain", "ranger", "caret", "factoextra", "cluster", "dunn.test", "car"))
```

## Resumen De Scripts

`01_limpieza_dataset_combustibles.R`

Limpia el dataset raw, normaliza texto, corrige problemas de codificacion,
asocia provincias con `Provincia_ids.csv`, agrega `Id_Provincia` y crea columnas
temporales (`Mes`, `Dia`, `Numero_Semana`).

`02_Imputacion_nan_data.R`

Trabaja sobre el dataset normalizado. Elimina `Adblue` por su baja cobertura e
inserta las fechas completamente ausentes usando interpolacion lineal por
`Provincia + Carburante`.

`03_integracion_festivos.R`

Toma el dataset normalizado ya corregido, integra festivos por provincia,
convierte domingos en festivos, crea `Dependencia_Petroleo`, agrega el precio
Brent desde `RBRTEd.xls` y aplica Fill Forward para dias sin cotizacion.
Exporta el dataset final:

```text
Dataset_Processed/Historico_precios_combustibles_España.csv
```

`04_tratamiento_valores_extremos.R`

Identifica valores extremos en `Precio_Litro_Euros` por rangos mensuales,
agrupando por `Provincia + Carburante + Año-Mes`. Los valores extremos se
tratan como ausentes y se imputan mediante interpolacion lineal dentro de cada
serie temporal. El dataset final conserva solo variables analiticas y deja la
auditoría del tratamiento en `Dataset_aux`.


`05_analisis_de_datos.R`

Responde al apartado 4 de la practica. Aplica un modelo supervisado, un modelo
no supervisado y un contraste de hipotesis sobre el dataset con outliers
tratados.

- Modelo supervisado: entrena un Random Forest para predecir `Precio_Litro_Euros`.
- Evalua el modelo con `RMSE`, `MAE` y `R²`.
- Genera graficos de importancia de variables y predichos vs reales.
- Modelo no supervisado: aplica K-Means sobre perfiles `Provincia x Carburante`.
- Usa graficos de codo y silueta para justificar el numero de clusters.
- Contraste de hipotesis: compara precios por `Dependencia_Petroleo`.
- Verifica normalidad con KS y QQ-plots.
- Verifica homocedasticidad con Levene.
- Aplica Kruskal-Wallis y post-hoc de Dunn.


## Como Ejecutar Los Scripts

Desde PowerShell, terminal de VS Code o terminal de RStudio, ubicarse en la raiz del proyecto:

```powershell
cd ruta-al-proyecto\Precio_Combustibles_ESP
```

Ejecutar en este orden:

```powershell
Rscript src/R_code/01_limpieza_dataset_combustibles.R

cd src/R_code
Rscript .\02_Imputacion_nan_data.R
Rscript .\03_integracion_festivos.R
Rscript .\04_tratamiento_valores_extremos.R
Rscript .\05_analisis_de_datos.R
```

Tambien se puede abrir el archivo en RStudio y ejecutarlo completo respetando
los directorios de trabajo:

```r
setwd("ruta-al-proyecto/Precio_Combustibles_ESP")
source("src/R_code/01_limpieza_dataset_combustibles.R")

setwd("ruta-al-proyecto/Precio_Combustibles_ESP/src/R_code")
source("02_Imputacion_nan_data.R")
source("03_integracion_festivos.R")
source("04_tratamiento_valores_extremos.R")
source("05_analisis_de_datos.R")
```

## Como Generar Mapas Coropleticos

El script `src/R_code/Mapa_coropletico.R` genera una imagen PNG con el precio
medio por provincia para un carburante y rango de fechas definidos por el
usuario. Usa el dataset final tratado:

```text
Dataset_Processed/Historico_precios_combustibles_España_outliers_tratados.csv
```

Ejecutar desde la raiz del proyecto:

```powershell
Rscript src/R_code/Mapa_coropletico.R --start 01-01-2020 --end 03-04-2020 --carburante 1
```

Parametros:

```text
--start       Fecha inicial en formato DD-MM-YYYY.
--end         Fecha final en formato DD-MM-YYYY.
--carburante  Numero del carburante a representar.
```

Catalogo de carburantes:

```text
1 - Gas natural licuado
2 - Gases licuados del petroleo
3 - Gasoleo A habitual
4 - Gasoleo Premium
5 - Gasolina 95 E5
6 - Gasolina 98 E5
7 - Gas natural comprimido
```

La imagen se guarda en:

```text
docs/figures/maps/
```

con un nombre de archivo como:

```text
map_01-01-2020_03-04-2020_gas_natural_licuado.png
```

## Entradas

Dataset raw principal:

```text
Dataset_Raw/Historico_precios_combustibles_España.csv
```

Catalogo de provincias:

```text
docs/Codes_Provincia/Provincia_ids.csv
```

El catalogo debe contener:

```text
id,Provincia
```

Donde `id` se usa para crear la columna final `Id_Provincia`.

## Salidas

Dataset limpio:

```text
Dataset_Processed/Historico_precios_combustibles_España_texto_normalizado.csv
```
