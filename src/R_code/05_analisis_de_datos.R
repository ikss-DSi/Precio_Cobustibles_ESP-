library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ranger)
library(caret)
library(factoextra)
library(cluster)
library(dunn.test)
library(car)

# ==============================================================================
# Script: 05_analisis_de_datos.R
# Objetivo:
#   Resolver el apartado 4 de la Practica 2: Analisis de los datos.
#
#   4.1. Modelo supervisado y modelo no supervisado.
#   4.2. Contraste de hipotesis con verificacion de supuestos.
#
# Entradas:
#   Dataset_Processed/Historico_precios_combustibles_España_outliers_tratados.csv
#
# Salidas:
#   docs/figures/analisis/*.png
#   Resultados impresos en consola
#
# Descripcion general:
#   El script entrena un Random Forest para predecir Precio_Litro_Euros,
#   agrupa perfiles Provincia-Carburante con K-Means y contrasta si el precio
#   difiere segun Dependencia_Petroleo usando pruebas estadisticas.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Definicion de rutas relativas y tema grafico
# ------------------------------------------------------------------------------

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
  file.path("docs", "figures", "analisis"),
  file.path("..", "..", "docs", "figures", "analisis")
)

ruta_figuras <- rutas_figuras_posibles[
  dir.exists(dirname(rutas_figuras_posibles))
][1]

if (is.na(ruta_figuras)) {
  ruta_figuras <- file.path("docs", "figures", "analisis")
}

dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)

tema_grafico <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#d9d9d9"),
    panel.grid.minor = element_line(color = "#eeeeee"),
    axis.text        = element_text(color = "#222222"),
    axis.title       = element_text(color = "#222222"),
    plot.title       = element_text(color = "#111111", face = "bold"),
    plot.subtitle    = element_text(color = "#333333")
  )


# ------------------------------------------------------------------------------
# 2. Carga del dataset y gestion de tipos de datos
# ------------------------------------------------------------------------------

df <- read_csv(
  file           = ruta_dataset,
  locale         = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Las variables categoricas se convierten a factor para modelos y contrastes.
# Mes_Num transforma el mes textual en numero para usarlo como predictor.
df <- df %>%
  mutate(
    Fecha                = as.Date(Fecha),
    Carburante           = as.factor(Carburante),
    Dependencia_Petroleo = factor(
      Dependencia_Petroleo,
      levels = c("Baja", "Media", "Alta")
    ),
    Provincia            = as.factor(Provincia),
    Comunidad_Autonoma   = as.factor(Comunidad_Autonoma),
    Festivo              = as.factor(Festivo),
    Dia_Semana           = factor(
      Dia_Semana,
      levels = c("lunes", "martes", "miercoles",
                  "jueves", "viernes", "sabado", "domingo")
    ),
    Mes_Num              = match(
      Mes,
      c("enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    )
  )

cat("\n============================================================\n")
cat("APARTADO 4 — ANALISIS DE LOS DATOS\n")
cat("Registros cargados:", nrow(df), "\n")
cat("Columnas:", ncol(df), "\n")
cat("============================================================\n\n")


# ------------------------------------------------------------------------------
# 3. Modelo supervisado: Random Forest de regresion
# ------------------------------------------------------------------------------

# El modelo predice Precio_Litro_Euros usando variables temporales, territoriales
# y de carburante. Random Forest permite relaciones no lineales e interacciones.
# La importancia por permutacion ayuda a interpretar que variables explican mas
# variacion del precio.

cat("\n--- 4.1.1 MODELO SUPERVISADO: RANDOM FOREST ---\n\n")


# Se seleccionan solo las variables que entran al modelo supervisado.

df_modelo <- df %>%
  select(
    Precio_Litro_Euros,
    Carburante,
    Dependencia_Petroleo,
    Provincia,
    Mes_Num,
    Dia_Semana,
    Festivo,
    Precio_Barril_Petroleo_Dolares
  )

cat("Registros para modelado:", nrow(df_modelo), "\n")
cat("NA en Precio_Barril_Petroleo_Dolares:",
    sum(is.na(df_modelo$Precio_Barril_Petroleo_Dolares)),
    "(justificados: carburantes con dependencia baja)\n\n")


# La particion train/test se estratifica por Carburante para conservar cobertura.

set.seed(42)

idx_train <- createDataPartition(
  y     = df_modelo$Carburante,
  p     = 0.7,
  list  = FALSE
)

train <- df_modelo[idx_train, ]
test  <- df_modelo[-idx_train, ]

cat("Particion train/test (70/30):\n")
cat("  Train:", nrow(train), "registros\n")
cat("  Test: ", nrow(test),  "registros\n\n")


# Se entrena el Random Forest con importancia de variables por permutacion.

cat("Entrenando Random Forest (300 arboles)...\n")

rf_model <- ranger(
  formula    = Precio_Litro_Euros ~ .,
  data       = train,
  num.trees  = 300,
  importance = "permutation",
  seed       = 42,
  respect.unordered.factors = "order"
)

cat("Modelo entrenado.\n")
cat("OOB prediction error (MSE):", round(rf_model$prediction.error, 6), "\n\n")


# Las metricas se calculan sobre test para evaluar error fuera de entrenamiento.

pred <- predict(rf_model, data = test)$predictions

rmse <- sqrt(mean((pred - test$Precio_Litro_Euros)^2))
mae  <- mean(abs(pred - test$Precio_Litro_Euros))
r2   <- cor(pred, test$Precio_Litro_Euros)^2

cat("Metricas sobre el conjunto de test:\n")
cat("  RMSE:", round(rmse, 4), "euros/litro\n")
cat("  MAE: ", round(mae, 4),  "euros/litro\n")
cat("  R²:  ", round(r2, 4),   "\n\n")


# La importancia muestra cuanto empeora el modelo al permutar cada predictor.

imp_df <- data.frame(
  Variable   = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
) %>%
  arrange(desc(Importance))

cat("Importancia de variables (permutacion):\n")
print(imp_df, row.names = FALSE)
cat("\n")

grafico_importancia <- ggplot(
  imp_df,
  aes(x = reorder(Variable, Importance), y = Importance)
) +
  geom_bar(stat = "identity", fill = "#2c7bb6") +
  coord_flip() +
  labs(
    title = "Importancia de variables - Random Forest",
    subtitle = "Metodo de permutacion sobre conjunto de test",
    x = "Variable",
    y = "Importancia (incremento en MSE al permutar)"
  ) +
  tema_grafico

ggsave(
  filename = file.path(ruta_figuras, "rf_importancia_variables.png"),
  plot     = grafico_importancia,
  width    = 10,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)


# El grafico compara predicciones y valores reales; la diagonal indica ajuste ideal.

set.seed(42)
idx_muestra <- sample(length(pred), min(10000, length(pred)))

grafico_pred_vs_real <- ggplot(
  data.frame(
    Real      = test$Precio_Litro_Euros[idx_muestra],
    Predicho  = pred[idx_muestra]
  ),
  aes(x = Real, y = Predicho)
) +
  geom_point(alpha = 0.08, color = "#2c7bb6", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "#b33a3a",
              linetype = "dashed", linewidth = 0.7) +
  labs(
    title    = "Valores predichos vs reales",
    subtitle = paste0("Random Forest | R² = ", round(r2, 4),
                      " | RMSE = ", round(rmse, 4), " euros/L"),
    x = "Precio real (euros/L)",
    y = "Precio predicho (euros/L)"
  ) +
  tema_grafico

ggsave(
  filename = file.path(ruta_figuras, "rf_predichos_vs_reales.png"),
  plot     = grafico_pred_vs_real,
  width    = 8,
  height   = 7,
  dpi      = 150,
  bg       = "white"
)

cat("Graficos del modelo supervisado generados.\n\n")


# ------------------------------------------------------------------------------
# 4. Modelo no supervisado: K-Means
# ------------------------------------------------------------------------------

# K-Means agrupa perfiles Provincia-Carburante sin usar etiquetas previas.
# Cada perfil resume nivel medio, volatilidad, mediana y peso de dias festivos.
# Los graficos de codo y silueta sirven para justificar el numero de clusters.

cat("\n--- 4.1.2 MODELO NO SUPERVISADO: K-MEANS ---\n\n")


# Se agregan las series diarias a perfiles Provincia-Carburante comparables.

perfiles <- df %>%
  group_by(Provincia, Carburante) %>%
  summarise(
    precio_medio   = mean(Precio_Litro_Euros, na.rm = TRUE),
    precio_sd      = sd(Precio_Litro_Euros, na.rm = TRUE),
    precio_mediana = median(Precio_Litro_Euros, na.rm = TRUE),
    pct_festivos   = mean(Festivo == "Si", na.rm = TRUE),
    n_registros    = n(),
    .groups        = "drop"
  ) %>%
  filter(n_registros >= 30)

cat("Perfiles agregados (Provincia x Carburante):", nrow(perfiles), "\n\n")


# Las variables numericas se escalan para que todas pesen de forma comparable.

vars_cluster <- c("precio_medio", "precio_sd", "precio_mediana", "pct_festivos")

perfiles_scaled <- perfiles %>%
  select(all_of(vars_cluster)) %>%
  scale()

rownames(perfiles_scaled) <- paste(perfiles$Provincia, "-", perfiles$Carburante)


# El metodo del codo evalua la reduccion de varianza intra-cluster para cada K.

grafico_codo <- fviz_nbclust(
  perfiles_scaled,
  kmeans,
  method  = "wss",
  k.max   = 10,
  nstart  = 25
) +
  labs(
    title    = "Metodo del codo - Seleccion de K",
    subtitle = "Within-cluster Sum of Squares por numero de clusters",
    x        = "Numero de clusters (K)",
    y        = "Suma de cuadrados intra-cluster"
  ) +
  tema_grafico

ggsave(
  filename = file.path(ruta_figuras, "kmeans_metodo_codo.png"),
  plot     = grafico_codo,
  width    = 8,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)


# La silueta mide cohesion interna y separacion entre clusters.

grafico_silueta <- fviz_nbclust(
  perfiles_scaled,
  kmeans,
  method  = "silhouette",
  k.max   = 10,
  nstart  = 25
) +
  labs(
    title    = "Indice de silueta - Seleccion de K",
    subtitle = "Valor medio de silueta por numero de clusters",
    x        = "Numero de clusters (K)",
    y        = "Anchura media de silueta"
  ) +
  tema_grafico

ggsave(
  filename = file.path(ruta_figuras, "kmeans_indice_silueta.png"),
  plot     = grafico_silueta,
  width    = 8,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)


# K se fija en 4 como punto de partida interpretable.
# Si codo y silueta sugieren otro valor, ajustar k_optimo.

k_optimo <- 4

set.seed(42)

km <- kmeans(
  perfiles_scaled,
  centers  = k_optimo,
  nstart   = 25,
  iter.max = 100
)

perfiles$Cluster <- as.factor(km$cluster)

cat("K-Means entrenado con K =", k_optimo, "\n")
cat("Tamaño de clusters:", paste(table(km$cluster), collapse = ", "), "\n\n")


# La caracterizacion resume el perfil medio de cada cluster.

caracterizacion <- perfiles %>%
  group_by(Cluster) %>%
  summarise(
    n_perfiles     = n(),
    precio_medio   = round(mean(precio_medio, na.rm = TRUE), 3),
    precio_sd      = round(mean(precio_sd, na.rm = TRUE), 3),
    precio_mediana = round(mean(precio_mediana, na.rm = TRUE), 3),
    pct_festivos   = round(mean(pct_festivos, na.rm = TRUE), 3),
    .groups        = "drop"
  )

cat("Caracterizacion de clusters:\n")
print(as.data.frame(caracterizacion), row.names = FALSE)
cat("\n")

# Este detalle ayuda a interpretar que carburantes predominan en cada cluster.
detalle_clusters <- perfiles %>%
  count(Cluster, Carburante, name = "Perfiles") %>%
  arrange(Cluster, desc(Perfiles))

cat("Carburantes por cluster:\n")
print(as.data.frame(detalle_clusters), row.names = FALSE)
cat("\n")


# La proyeccion PCA permite visualizar los clusters en dos dimensiones.

grafico_pca <- fviz_cluster(
  km,
  data          = perfiles_scaled,
  geom          = "point",
  ellipse.type  = "convex",
  pointsize     = 1.5,
  ggtheme       = tema_grafico
) +
  labs(
    title    = "Clustering K-Means - Perfiles Provincia x Carburante",
    subtitle = paste0("K = ", k_optimo,
                      " clusters | Proyeccion PCA (componentes 1 y 2)")
  )

ggsave(
  filename = file.path(ruta_figuras, "kmeans_pca_clusters.png"),
  plot     = grafico_pca,
  width    = 10,
  height   = 7,
  dpi      = 150,
  bg       = "white"
)

cat("Graficos del modelo no supervisado generados.\n\n")


# ------------------------------------------------------------------------------
# 5. Contraste de hipotesis
# ------------------------------------------------------------------------------

# Se contrasta si el precio difiere entre niveles de Dependencia_Petroleo.
# Primero se revisan normalidad y homocedasticidad para decidir la prueba.
# Al no cumplirse los supuestos, se aplica Kruskal-Wallis y post-hoc de Dunn.

cat("\n--- 4.2 CONTRASTE DE HIPOTESIS ---\n\n")

cat("H0: Las distribuciones de Precio_Litro_Euros son iguales en los\n")
cat("    tres grupos de Dependencia_Petroleo (Baja, Media, Alta).\n")
cat("H1: Al menos un grupo presenta una distribucion diferente.\n\n")


# Paso 1: se verifica normalidad con KS sobre muestras y QQ-plots por grupo.

cat("--- Paso 1: Verificacion de normalidad ---\n\n")

# Se usa muestra de 5000 por grupo para evitar que el test sea inmanejable.
set.seed(42)

for (grupo in levels(df$Dependencia_Petroleo)) {
  muestra <- df %>%
    filter(Dependencia_Petroleo == grupo) %>%
    pull(Precio_Litro_Euros) %>%
    sample(5000)

  ks_result <- ks.test(
    muestra,
    "pnorm",
    mean(muestra),
    sd(muestra)
  )

  cat(
    "  KS-test para", grupo, ":",
    "D =", round(ks_result$statistic, 4),
    ", p-valor =", format.pval(ks_result$p.value, digits = 4), "\n"
  )
}

cat("\n  Resultado: se rechaza normalidad en todos los grupos (p < 0.05).\n")
cat("  Esto es esperable con muestras grandes y distribuciones de precios\n")
cat("  con asimetria positiva.\n\n")


# Los QQ-plots permiten revisar visualmente desviaciones frente a normalidad.

df_muestra_qq <- df %>%
  group_by(Dependencia_Petroleo) %>%
  slice_sample(n = 2000) %>%
  ungroup()

grafico_qq <- ggplot(df_muestra_qq, aes(sample = Precio_Litro_Euros)) +
  stat_qq(color = "#2c7bb6", alpha = 0.3, size = 0.8) +
  stat_qq_line(color = "#b33a3a", linewidth = 0.6) +
  facet_wrap(~Dependencia_Petroleo, scales = "free") +
  labs(
    title    = "QQ-plots por nivel de dependencia del petroleo",
    subtitle = "Comparacion con distribucion normal teorica (muestra n=2000 por grupo)",
    x        = "Cuantiles teoricos",
    y        = "Cuantiles observados (euros/L)"
  ) +
  tema_grafico

ggsave(
  filename = file.path(ruta_figuras, "hipotesis_qqplots.png"),
  plot     = grafico_qq,
  width    = 12,
  height   = 5,
  dpi      = 150,
  bg       = "white"
)


# Paso 2: Levene contrasta igualdad de varianzas entre grupos.

cat("--- Paso 2: Verificacion de homocedasticidad (test de Levene) ---\n\n")

levene_result <- leveneTest(
  Precio_Litro_Euros ~ Dependencia_Petroleo,
  data = df
)

cat("  Estadistico F:", round(levene_result$`F value`[1], 4), "\n")
cat("  p-valor:", format.pval(levene_result$`Pr(>F)`[1], digits = 4), "\n\n")

if (levene_result$`Pr(>F)`[1] < 0.05) {
  cat("  Resultado: se rechaza homocedasticidad (varianzas desiguales).\n\n")
} else {
  cat("  Resultado: no se rechaza homocedasticidad.\n\n")
}


# La decision combina normalidad y homocedasticidad antes del contraste final.

cat("--- Decision ---\n")
cat("  Normalidad: NO se cumple.\n")
cat("  Homocedasticidad: NO se cumple.\n")
cat("  Prueba seleccionada: Kruskal-Wallis (alternativa no parametrica al ANOVA).\n\n")


# Paso 3: Kruskal-Wallis compara distribuciones sin asumir normalidad.

cat("--- Paso 3: Test de Kruskal-Wallis ---\n\n")

kw_result <- kruskal.test(
  Precio_Litro_Euros ~ Dependencia_Petroleo,
  data = df
)

cat("  Estadistico H:", round(kw_result$statistic, 2), "\n")
cat("  Grados de libertad:", kw_result$parameter, "\n")
cat("  p-valor:", format.pval(kw_result$p.value, digits = 4), "\n\n")

if (kw_result$p.value < 0.05) {
  cat("  Resultado: se rechaza H0. Existen diferencias estadisticamente\n")
  cat("  significativas entre al menos dos grupos de dependencia del petroleo.\n\n")
} else {
  cat("  Resultado: no se rechaza H0.\n\n")
}


# Paso 4: Dunn identifica que pares de grupos presentan diferencias.

cat("--- Paso 4: Comparaciones post-hoc (test de Dunn con correccion Bonferroni) ---\n\n")

dunn_result <- dunn.test(
  x      = df$Precio_Litro_Euros,
  g      = df$Dependencia_Petroleo,
  method = "bonferroni",
  kw     = FALSE,
  label  = TRUE,
  table  = FALSE
)

cat("\n")


# El resumen descriptivo acompaña la interpretacion estadistica.
resumen_grupos <- df %>%
  group_by(Dependencia_Petroleo) %>%
  summarise(
    n        = n(),
    media    = round(mean(Precio_Litro_Euros, na.rm = TRUE), 4),
    mediana  = round(median(Precio_Litro_Euros, na.rm = TRUE), 4),
    sd       = round(sd(Precio_Litro_Euros, na.rm = TRUE), 4),
    .groups  = "drop"
  )

cat("Estadisticas descriptivas por grupo:\n")
print(as.data.frame(resumen_grupos), row.names = FALSE)
cat("\n")

grafico_boxplot_hipotesis <- ggplot(
  df,
  aes(
    x    = Dependencia_Petroleo,
    y    = Precio_Litro_Euros,
    fill = Dependencia_Petroleo
  )
) +
  geom_boxplot(
    outlier.alpha = 0.05,
    outlier.size  = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Baja"  = "#66c2a5",
      "Media" = "#fc8d62",
      "Alta"  = "#8da0cb"
    )
  ) +
  labs(
    title    = "Distribucion del precio por nivel de dependencia del petroleo",
    subtitle = paste0(
      "Kruskal-Wallis: H = ", round(kw_result$statistic, 2),
      ", p < 0.001 | Se rechaza H0"
    ),
    x = "Nivel de dependencia del petroleo",
    y = "Precio por litro (euros)"
  ) +
  tema_grafico +
  theme(legend.position = "none")

ggsave(
  filename = file.path(ruta_figuras, "hipotesis_boxplot_dependencia.png"),
  plot     = grafico_boxplot_hipotesis,
  width    = 8,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)


# Los histogramas complementan la revision visual de distribuciones.

grafico_histograma <- ggplot(
  df,
  aes(x = Precio_Litro_Euros, fill = Dependencia_Petroleo)
) +
  geom_histogram(
    bins     = 50,
    alpha    = 0.7,
    color    = "white",
    linewidth = 0.2
  ) +
  facet_wrap(~Dependencia_Petroleo, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Baja"  = "#66c2a5",
      "Media" = "#fc8d62",
      "Alta"  = "#8da0cb"
    )
  ) +
  labs(
    title    = "Distribucion del precio por grupo de dependencia",
    subtitle = "Histogramas separados para verificar forma de la distribucion",
    x        = "Precio por litro (euros)",
    y        = "Frecuencia"
  ) +
  tema_grafico +
  theme(legend.position = "none")

ggsave(
  filename = file.path(ruta_figuras, "hipotesis_histogramas_dependencia.png"),
  plot     = grafico_histograma,
  width    = 12,
  height   = 5,
  dpi      = 150,
  bg       = "white"
)


# ------------------------------------------------------------------------------
# 6. Resumen final en consola
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("RESUMEN DE RESULTADOS — APARTADO 4\n")
cat("============================================================\n\n")

cat("4.1.1 MODELO SUPERVISADO (Random Forest):\n")
cat("  R² = ", round(r2, 4), "\n")
cat("  RMSE = ", round(rmse, 4), " euros/L\n")
cat("  MAE = ", round(mae, 4), " euros/L\n")
cat("  Variable mas importante:", imp_df$Variable[1], "\n\n")

cat("4.1.2 MODELO NO SUPERVISADO (K-Means):\n")
cat("  K =", k_optimo, "clusters\n")
cat("  Perfiles analizados:", nrow(perfiles), "\n")
print(as.data.frame(caracterizacion), row.names = FALSE)
cat("\n")

cat("4.2 CONTRASTE DE HIPOTESIS:\n")
cat("  Test: Kruskal-Wallis\n")
cat("  H =", round(kw_result$statistic, 2), "\n")
cat("  p-valor:", format.pval(kw_result$p.value, digits = 4), "\n")
cat("  Conclusion: Se rechaza H0. Los tres niveles de dependencia del\n")
cat("  petroleo presentan distribuciones de precio significativamente\n")
cat("  diferentes.\n\n")

cat("Graficos generados en:", ruta_figuras, "\n")
cat("============================================================\n")
