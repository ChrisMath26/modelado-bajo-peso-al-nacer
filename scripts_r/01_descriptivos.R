#======================================================================
# ANALISIS BIVARIADO - BAJO PESO AL NACER (BPN)
# Autor: Christian Trilleras
# Objetivo:
# Caracterizar asociaciones bivariadas entre variables explicativas
# y el desenlace BPN.
#======================================================================

#================================================================
#Librerías y opciones globales
#================================================================
# 4 cifras significativas y sin notación científica:
options(digits = 4, scipen = 999) 
# cargar librerías: 
library(FactoClass)
library(plotly) # para gráficos interactivos
library(knitr) # para función kable (tablas estáticas)
library(DT) # para tablas interactivas
library(ggplot2) # para gráficos
library(tidyverse) # para manipulación de datos
library(readr)
library(xtable) 
library(reshape2)
library(GGally)
library(dplyr)
library(effectsize)
#======================================================================
# DIRECTORIOS
#======================================================================

dir_figures <- "reports/figures"
dir_tables  <- "reports/tables"

if(!dir.exists(dir_figures)){
  dir.create(dir_figures, recursive = TRUE)
}

if(!dir.exists(dir_tables)){
  dir.create(dir_tables, recursive = TRUE)
}

# ==========================================================
# CARGA DE DATOS
# ==========================================================

ruta <- ruta <- "data/processed/nacimientos_bogota_preprocesado.csv"


df <- read_delim(
  file = ruta,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
)
#======================================================================
# VERIFICACION INICIAL
#======================================================================

cat("\nDimensiones:\n")
print(dim(df))

cat("\nEstructura:\n")
str(df)

cat("\nResumen:\n")
summary(df)
# ==========================================================
# VARIABLES
# ==========================================================

variables_cuantitativas <- c(
  "PESO_GRAMOS",
  "NUM_CONSULTAS_PRENAT",
  "EDAD_MADRE",
  "NUM_HIJOS_NACIDOS_VIVOS",
  "NUM_EMBARAZOS",
  "EDAD_PADRE",
  "TIEMPO_GESTACION",
  "TALLA_CENTIMETROS"
)

variables_cualitativas <- c(
  "SIT_PARTO",
  "SEXO",
  "PESO",
  "BPN",
  "ATENCION_PARTO_POR",
  "GESTACION",
  "TIPO_PARTO",
  "MULTIPLICIDAD_PARTO",
  "PERTENENCIA_ETNICA",
  "TIPO_DOC_MADRE",
  "GRUPO_QUINQUENAL_MADRE_CALCULADORA",
  "ESTADO_CONYUGAL_MADRE",
  "NIVEL_EDUCATIVO_MADRE",
  "LOCALIDAD_MADRE",
  "REGIMEN_SEGURIDAD",
  "NIVEL_EDUCATIVO_PADRE"
)

#======================================================================
# CONVERSION DE VARIABLES CATEGORICAS A FACTOR
#======================================================================

df <- df %>%
  mutate(
    across(
      all_of(variables_cualitativas),
      as.factor
    )
  )

#=========================================================
# CORRELACIONES ENTRE VARIABLES CUANTITATIVAS
#======================================================================

cat("\n")
cat("====================================================\n")
cat("CORRELACIONES CUANTITATIVAS\n")
cat("====================================================\n")

#----------------------------------------------------------------------
# Pearson
#----------------------------------------------------------------------

corr_pearson <- cor(
  df[, variables_cuantitativas],
  use = "pairwise.complete.obs",
  method = "pearson"
)

write.csv(
  corr_pearson,
  file.path(
    dir_tables,
    "correlacion_pearson.csv"
  )
)

#----------------------------------------------------------------------
# Spearman
#----------------------------------------------------------------------

corr_spearman <- cor(
  df[, variables_cuantitativas],
  use = "pairwise.complete.obs",
  method = "spearman"
)

write.csv(
  corr_spearman,
  file.path(
    dir_tables,
    "correlacion_spearman.csv"
  )
)

#=========================================================
# HEATMAP DE CORRELACIONES PEARSON
#=========================================================

corr_long <- melt(corr_pearson)

p <- ggplot(
  corr_long,
  aes(Var1, Var2, fill = value)
) +
  geom_tile(color = "white") +

  # Mostrar correlación con 2 decimales
  geom_text(
    aes(label = sprintf("%.2f", value)),
    size = 3
  ) +

  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +

  labs(
    title = "Matriz de Correlaciones de Pearson",
    x = "",
    y = ""
  ) +

  theme_minimal() +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    axis.text.y = element_text(
      size = 10
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

print(p)

# Guardar figura
ggsave(
  filename = file.path(
    dir_figures,
    "heatmap_correlacion_pearson.png"
  ),
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

#=========================================================
# PLOT DE PARES
#=========================================================
p <- ggpairs(
  df[, variables_cuantitativas]
)

print(p)

ggsave(
  file.path(
    dir_figures,
    "pairplot_cuantitativas.png"
  ),
  p,
  width = 15,
  height = 15
)

#======================================================================
# VARIABLES CUANTITATIVAS VS BPN
#======================================================================

cat("\n")
cat("====================================================\n")
cat("CUANTITATIVAS VS BPN\n")
cat("====================================================\n")

resultado_cohen <- list()

df_plot_base <- df %>%
  mutate(
    BPN_LABEL = factor(
      BPN,
      levels = c(0, 1),
      labels = c(
        "Sin bajo peso al nacer",
        "Bajo peso al nacer"
      )
    )
  )

for(v in variables_cuantitativas){

  if(v == "PESO_GRAMOS"){
    next
  }

  formula_txt <- paste(v, "~ BPN")

  #--------------------------------------------------------------
  # Cohen d
  #--------------------------------------------------------------

  efecto <- effectsize::cohens_d(
    as.formula(formula_txt),
    data = df
  )

  efecto$variable <- v
  resultado_cohen[[v]] <- efecto

  #--------------------------------------------------------------
  # Boxplot coloreado
  #--------------------------------------------------------------

  p <- ggplot(
    df_plot_base,
    aes(
      x = BPN_LABEL,
      y = .data[[v]],
      fill = BPN_LABEL
    )
  ) +
    geom_boxplot(
      alpha = 0.8,
      outlier.alpha = 0.35
    ) +
    scale_fill_manual(
      values = c(
        "Sin bajo peso al nacer" = "#4E79A7",
        "Bajo peso al nacer" = "#E15759"
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 10)
    ) +
    labs(
      title = paste(v, "según condición de bajo peso al nacer"),
      x = NULL,
      y = v
    )

  print(p)

  #--------------------------------------------------------------
  # Guardar gráfico
  #--------------------------------------------------------------

  ggsave(
    filename = file.path(
      dir_figures,
      paste0("boxplot_", v, "_BPN.png")
    ),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

#--------------------------------------------------------------
# Consolidar y guardar Cohen d
#--------------------------------------------------------------

tabla_cohen <- bind_rows(resultado_cohen)

write.csv(
  tabla_cohen,
  file.path(dir_tables, "cohen_d_bpn.csv"),
  row.names = FALSE
)

#Los resultados sugieren que las variables demográficas y reproductivas evaluadas presentan tamaños de efecto muy pequeños y, por sí solas, 
#tienen escasa capacidad para diferenciar nacimientos con y sin bajo peso al nacer. En contraste, el tiempo de gestación (d=2.15) y la talla al nacer (d=2.10) 
#muestran tamaños de efecto excepcionalmente grandes, evidenciando que son los principales factores asociados al desenlace en la muestra analizada. 
#Esto indica que las diferencias entre grupos están dominadas por características directamente relacionadas con el crecimiento fetal y la duración de la gestación,
# más que por variables sociodemográficas o antecedentes reproductivos.

#======================================================================
# TESTS DE DIFERENCIA DE MEDIAS
# VARIABLES CUANTITATIVAS VS BPN
#======================================================================

cat("\n")
cat("====================================================\n")
cat("T-TEST (WELCH) VS BPN\n")
cat("====================================================\n")

resultado_ttest <- list()

for(v in variables_cuantitativas){

  if(v == "PESO_GRAMOS"){
    next
  }

  datos_tmp <- df %>%
    dplyr::select(BPN, all_of(v)) %>%
    na.omit()

  prueba <- t.test(
    as.formula(
      paste(v, "~ BPN")
    ),
    data = datos_tmp,
    var.equal = FALSE
  )

  media_0 <- mean(
    datos_tmp[[v]][datos_tmp$BPN == 0],
    na.rm = TRUE
  )

  media_1 <- mean(
    datos_tmp[[v]][datos_tmp$BPN == 1],
    na.rm = TRUE
  )

  resultado_ttest[[v]] <- data.frame(
    variable = v,
    media_sin_bpn = media_0,
    media_bpn = media_1,
    diferencia_medias = media_1 - media_0,
    estadistico_t = unname(prueba$statistic),
    p_value = prueba$p.value,
    ic_inf = prueba$conf.int[1],
    ic_sup = prueba$conf.int[2]
  )
}

tabla_ttest <- bind_rows(resultado_ttest)

tabla_ttest <- tabla_ttest %>%
  arrange(p_value)

display(tabla_ttest)

write.csv(
  tabla_ttest,
  file.path(
    dir_tables,
    "ttest_bpn.csv"
  ),
  row.names = FALSE
)

# El Welch t-test confirma que existen diferencias estadísticamente significativas entre nacimientos con y sin BPN para varias variables.
# Sin embargo, al considerar el tamaño del efecto, solo tiempo de gestación y talla al nacer muestran diferencias de gran magnitud y relevancia clínica.
# Las demás variables presentan diferencias muy pequeñas que, aunque detectables estadísticamente por el enorme tamaño muestral, tienen impacto práctico limitado en la discriminación entre grupos.


#======================================================================
# VALORES-TEST (ESTILO FACTOCLASS) PARA BPN
#======================================================================

cat("\n")
cat("====================================================\n")
cat("VALORES TEST - CARACTERIZACIÓN DE BPN\n")
cat("====================================================\n")

resultado_vtest <- list()

#--------------------------------------------------------------
# Etiquetas legibles
#--------------------------------------------------------------

df_vtest <- df %>%
  mutate(
    BPN_LABEL = factor(
      BPN,
      levels = c(0, 1),
      labels = c(
        "Sin bajo peso al nacer",
        "Bajo peso al nacer"
      )
    )
  )

#--------------------------------------------------------------
# Recorrido de variables
#--------------------------------------------------------------

for(v in variables_cuantitativas){

  if(v == "PESO_GRAMOS"){
    next
  }

  datos_tmp <- df_vtest %>%
    dplyr::select(BPN_LABEL, all_of(v)) %>%
    na.omit()

  media_global <- mean(datos_tmp[[v]])
  sd_global <- sd(datos_tmp[[v]])

  n_total <- nrow(datos_tmp)

  resumen_grupos <- datos_tmp %>%
    group_by(BPN_LABEL) %>%
    summarise(
      n = n(),
      media_grupo = mean(.data[[v]]),
      sd_grupo = sd(.data[[v]]),
      .groups = "drop"
    )

  resumen_grupos <- resumen_grupos %>%
    mutate(
      media_global = media_global,
      sd_global = sd_global,

      valor_test =
        (media_grupo - media_global) /
        (sd_global / sqrt(n)),

      variable = v
    )

  resultado_vtest[[v]] <- resumen_grupos
}

tabla_vtest <- bind_rows(resultado_vtest)

#--------------------------------------------------------------
# Ordenar por magnitud absoluta del valor-test
#--------------------------------------------------------------

tabla_vtest <- tabla_vtest %>%
  mutate(
    abs_vtest = abs(valor_test)
  ) %>%
  arrange(desc(abs_vtest))

display(tabla_vtest)

write.csv(
  tabla_vtest,
  file.path(
    dir_tables,
    "vtest_bpn.csv"
  ),
  row.names = FALSE
)
#Los valores-test confirman que los nacimientos con bajo peso al nacer se caracterizan principalmente por una menor duración de la gestación y una menor talla neonatal,
# variables que presentan desviaciones extremadamente grandes respecto al promedio poblacional. El número de controles prenatales muestra una asociación secundaria, 
#mientras que la edad materna, edad paterna y antecedentes reproductivos presentan una contribución marginal. En conjunto, los resultados sugieren que la diferenciación 
#entre grupos está dominada por factores directamente relacionados con el crecimiento fetal y la prematuridad, más que por características demográficas o reproductivas.