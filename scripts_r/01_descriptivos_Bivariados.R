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
setwd("C:/Users/Usuario/Downloads/proyecto GLM/modelado-bajo-peso-al-nacer")
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
library(rcompanion)
library(patchwork)
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

 ruta <- "data/processed/nacimientos_bogota_preprocesado_v2_imputados_winsorizados.csv"


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
  "SEXO_AGRUPADO",
  "BPN",
  "ATENCION_PARTO_GRUPO",
  "GESTACION_AGRUPADA",
  "TIPO_PARTO_AGRUPADO",
  "MULTIPLICIDAD_AGRUPADA",
  "PERTENENCIA_ETNICA_AGRUPADA",
  "TIPO_DOC_MADRE_AGRUPADO",
  "ESTADO_CONYUGAL_AGRUPADO",
  "NIVEL_EDUCATIVO_MADRE_AGRUPADO",
  "AREA_RESIDENCIA_MADRE",  
  "REGIMEN_SEGURIDAD_AGRUPADO",
  "NIVEL_EDUCATIVO_PADRE_AGRUPADO",
  "APGAR1_AGRUPADO"  
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


#======================================================================
# VARIABLES CATEGÓRICAS VS BPN
# CHI-CUADRADO + V DE CRAMER + PERFILES
#======================================================================

cat("\n")
cat("====================================================\n")
cat("CATEGÓRICAS VS BPN\n")
cat("====================================================\n")

resultado_cramer <- data.frame()
resultado_perfiles <- list()

#----------------------------------------------------------------------
# Etiquetas amigables para BPN
#----------------------------------------------------------------------

df_perfiles <- df %>%
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

#======================================================================
# RECORRER VARIABLES
#======================================================================

for(v in variables_cualitativas){

  if(v == "BPN"){
    next
  }

  cat("\n")
  cat("====================================================\n")
  cat("Variable:", v, "\n")
  cat("====================================================\n")

  #--------------------------------------------------------------------
  # TABLA DE CONTINGENCIA
  #--------------------------------------------------------------------

  tc <- table(
    df_perfiles[[v]],
    df_perfiles$BPN_LABEL
  )

  #--------------------------------------------------------------------
  # CHI-CUADRADO Y V DE CRAMER
  #--------------------------------------------------------------------

  chi <- suppressWarnings(
    chisq.test(tc)
  )

  v_cramer <- rcompanion::cramerV(tc)

  resultado_cramer <- rbind(
    resultado_cramer,
    data.frame(
      variable = v,
      chi2 = unname(chi$statistic),
      pvalue = chi$p.value,
      v_cramer = v_cramer
    )
  )

  #--------------------------------------------------------------------
  # PERFIL FILA
  # P(BPN | categoría)
  #--------------------------------------------------------------------

  perfil_fila <- (
    prop.table(tc, margin = 1) * 100
  )

  perfil_fila_df <- (
    as.data.frame.matrix(perfil_fila)
  )

  perfil_fila_df$categoria <- rownames(
    perfil_fila_df
  )

  perfil_fila_df <- perfil_fila_df %>%
    relocate(categoria)

  cat("\nPERFIL FILA\n")

  print(
    knitr::kable(
      perfil_fila_df,
      digits = 2
    )
  )

  #--------------------------------------------------------------------
  # PERFIL COLUMNA
  # P(categoría | BPN)
  #--------------------------------------------------------------------

  perfil_columna <- (
    prop.table(tc, margin = 2) * 100
  )

  perfil_columna_df <- (
    as.data.frame.matrix(perfil_columna)
  )

  perfil_columna_df$categoria <- rownames(
    perfil_columna_df
  )

  perfil_columna_df <- perfil_columna_df %>%
    relocate(categoria)

  cat("\nPERFIL COLUMNA\n")

  print(
    knitr::kable(
      perfil_columna_df,
      digits = 2
    )
  )

  #--------------------------------------------------------------------
  # GUARDAR RESULTADOS
  #--------------------------------------------------------------------

  resultado_perfiles[[v]] <- list(
    tabla = tc,
    perfil_fila = perfil_fila_df,
    perfil_columna = perfil_columna_df
  )

  #--------------------------------------------------------------------
  # GRÁFICO DE PREVALENCIA DE BPN
  #--------------------------------------------------------------------

  if(!("Bajo peso al nacer" %in%
       colnames(perfil_fila_df))){
    next
  }

  perfil_plot <- perfil_fila_df %>%
    select(
      categoria,
      `Bajo peso al nacer`
    ) %>%
    rename(
      prevalencia_bpn =
        `Bajo peso al nacer`
    ) %>%
    arrange(
      desc(prevalencia_bpn)
    )

  p <- ggplot(
    perfil_plot,
    aes(
      x = reorder(
        categoria,
        prevalencia_bpn
      ),
      y = prevalencia_bpn
    )
  ) +
    geom_col(
      fill = "#D73027"
    ) +
    geom_text(
      aes(
        label = paste0(
          round(
            prevalencia_bpn,
            1
          ),
          "%"
        )
      ),
      hjust = -0.15,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste(
        "Prevalencia de BPN por",
        v
      ),
      subtitle = paste(
        "V de Cramer =",
        round(v_cramer, 3)
      ),
      x = NULL,
      y = "Prevalencia de BPN (%)"
    ) +
    expand_limits(
      y = max(
        perfil_plot$prevalencia_bpn
      ) * 1.15
    )

  print(p)

  ggsave(
    filename = file.path(
      dir_figures,
      paste0(
        "prevalencia_bpn_",
        v,
        ".png"
      )
    ),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )
}

#======================================================================
# RESUMEN GLOBAL DE V DE CRAMER
#======================================================================

resultado_cramer <- resultado_cramer %>%
  arrange(
    desc(v_cramer)
  )

cat("\n")
cat("====================================================\n")
cat("RESUMEN V DE CRAMER\n")
cat("====================================================\n")

print(
  knitr::kable(
    resultado_cramer,
    digits = 4
  )
)

write.csv(
  resultado_cramer,
  file.path(
    dir_tables,
    "cramer_v_bpn.csv"
  ),
  row.names = FALSE
)

cat("\n")
cat("====================================================\n")
cat("PROCESO FINALIZADO\n")
cat("====================================================\n")

cat("\nFiguras almacenadas en:\n")
cat(dir_figures)


cat("\n\nTablas almacenadas en:\n")
cat(dir_tables)

#El análisis bivariado muestra que la estructura del bajo peso al nacer está dominada principalmente por factores obstétricos y neonatales, 
#especialmente la edad gestacional (V = 0.586), seguida por la multiplicidad del embarazo, el tipo de parto y el APGAR al minuto. 
#En contraste, las variables sociodemográficas presentan asociaciones débiles, mientras que variables como pertenencia étnica, 
#sitio de parto o tipo de atención muestran efectos prácticamente nulos. Esto sugiere que la mayor capacidad explicativa del fenómeno 
#se encuentra en características clínicas y obstétricas directamente relacionadas con el proceso gestacional, más que en factores demográficos considerados de forma aislada.



