#======================================================================
# ANALISIS DE COMPONENTES PRINCIPALES - ACP
# Autor: Christian Trilleras
# Proyecto: Bajo Peso al Nacer - Bogotá
#======================================================================

#======================================================================
#  CONFIGURACION GLOBAL
#======================================================================
setwd("C:/Users/Usuario/Downloads/proyecto GLM/modelado-bajo-peso-al-nacer")
options(digits = 4, scipen = 999)

#======================================================================
#  LIBRERIAS
#======================================================================

library(tidyverse)
library(readr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(DT)
library(knitr)

#======================================================================
#  DIRECTORIOS
#======================================================================

dir_figures <- "reports/figures"
dir_tables  <- "reports/tables"

if(!dir.exists(dir_figures)) dir.create(dir_figures, recursive = TRUE)
if(!dir.exists(dir_tables))  dir.create(dir_tables, recursive = TRUE)

#======================================================================
# 4. CARGA DE DATOS
#======================================================================

ruta <- "data/processed/nacimientos_bogota_preprocesado.csv"

df <- read_delim(
  file = ruta,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
)

#======================================================================
# VARIABLES DEL ACP
#======================================================================

variables_cuantitativas <- c(
  "NUM_CONSULTAS_PRENAT",
  "EDAD_MADRE",
  "NUM_HIJOS_NACIDOS_VIVOS",
  "NUM_EMBARAZOS",
  "EDAD_PADRE",
  "TIEMPO_GESTACION",
  "TALLA_CENTIMETROS"
)

variables_cuantitativas_sup <- c(
  "PESO_GRAMOS"
)

variables_ilustrativas <- c(
  "BPN",
  "SEXO",
  "GESTACION",
  "TIPO_PARTO",
  "REGIMEN_SEGURIDAD"
)

#======================================================================
#  BASE PARA ACP
#======================================================================

df_acp <- df %>%
  select(
    all_of(
      c(
        variables_cuantitativas,
        variables_cuantitativas_sup,
        variables_ilustrativas
      )
    )
  ) %>%
  mutate(across(all_of(variables_cuantitativas), as.numeric)) %>%
  mutate(across(all_of(variables_cuantitativas_sup), as.numeric)) %>%
  mutate(across(all_of(variables_ilustrativas), as.factor)) %>%
  drop_na()

cat("Dimensiones base ACP:\n")
print(dim(df_acp))

n_cuant_act <- length(variables_cuantitativas)
n_cuant_sup <- length(variables_cuantitativas_sup)
n_cual_sup <- length(variables_ilustrativas)

#======================================================================
#  ACP NORMADO
# scale.unit = TRUE equivale a ACP sobre matriz de correlaciones
# Las cualitativas se proyectan como suplementarias
#======================================================================

res_acp <- PCA(
  df_acp,
  scale.unit = TRUE,
  quanti.sup = (n_cuant_act + 1):(n_cuant_act + n_cuant_sup),
  quali.sup = (n_cuant_act + n_cuant_sup + 1):ncol(df_acp),
  graph = FALSE
)


#======================================================================
# INERCIA EXPLICADA
#======================================================================

eig <- as.data.frame(res_acp$eig)

colnames(eig) <- c(
  "eigenvalue",
  "percentage_variance",
  "cumulative_percentage_variance"
)

eig$dimension <- paste0("Dim.", seq_len(nrow(eig)))

write.csv(
  eig,
  file.path(dir_tables, "acp_inercia_explicada_data_original.csv"),
  row.names = FALSE
)

print(eig)

p_scree <- fviz_eig(
  res_acp,
  addlabels = TRUE,
  ylim = c(0, 100)
) +
  theme_minimal() +
  labs(
    title = "ACP - Inercia explicada",
    x = "Componentes principales",
    y = "Porcentaje de inercia"
  )

print(p_scree)

ggsave(
  file.path(dir_figures, "acp_screeplot_inercia_data_original.png"),
  p_scree,
  width = 8,
  height = 6,
  dpi = 300
)
#======================================================================
#  CURVA DE INERCIA ACUMULADA
#======================================================================

p_acum <- ggplot(
  eig,
  aes(
    x = seq_along(eigenvalue),
    y = cumulative_percentage_variance
  )
) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_hline(yintercept = 90, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "ACP - Inercia acumulada",
    x = "Número de componentes",
    y = "Inercia acumulada (%)"
  )

print(p_acum)
ggsave(
  file.path(dir_figures, "acp_inercia_acumulada_data_original.png"),
  p_acum,
  width = 8,
  height = 6,
  dpi = 300
)

#======================================================================
#   RESULTADOS DE VARIABLES ACTIVAS
#======================================================================

coord_var <- as.data.frame(res_acp$var$coord)
coord_var$variable <- rownames(coord_var)

contrib_var <- as.data.frame(res_acp$var$contrib)
contrib_var$variable <- rownames(contrib_var)

cos2_var <- as.data.frame(res_acp$var$cos2)
cos2_var$variable <- rownames(cos2_var)

write.csv(
  coord_var,
  file.path(dir_tables, "acp_coordenadas_variables_data_original.csv"),
  row.names = FALSE
)

write.csv(
  contrib_var,
  file.path(dir_tables, "acp_contribuciones_variables_data_original.csv"),
  row.names = FALSE
)

write.csv(
  cos2_var,
  file.path(dir_tables, "acp_cos2_variables_data_original.csv"),
  row.names = FALSE
)

#======================================================================
#GRAFICOS DE CONTRIBUCION DE VARIABLES
#======================================================================
for(dim in 1:3){

  p_contrib <- fviz_contrib(
    res_acp,
    choice = "var",
    axes = dim,
    top = length(variables_cuantitativas)
  ) +
    theme_minimal() +
    labs(
      title = paste(
        "ACP - Contribución de variables a Dim",
        dim
      )
    )

  print(p_contrib)

  ggsave(
    file.path(
      dir_figures,
      paste0(
        "acp_contribucion_variables_dim",
        dim,
        "_data_original.png"
      )
    ),
    p_contrib,
    width = 9,
    height = 6,
    dpi = 300
  )
}
#======================================================================
# CALIDAD DE REPRESENTACION DE VARIABLES - COS2
#======================================================================
#======================================================================
# CALIDAD DE REPRESENTACIÓN (COS²)
#======================================================================

p_cos2 <- fviz_pca_var(
  res_acp,
  col.var = "cos2",
  gradient.cols = c(
    "#00AFBB",
    "#E7B800",
    "#FC4E07"
  ),
  repel = TRUE
) +
  theme_minimal() +
  labs(
    title = "ACP - Calidad de representación (Cos²)"
  )

print(p_cos2)
ggsave(
  file.path(
    dir_figures,
    "acp_cos2_variables_data_original.png"
  ),
  p_cos2,
  width = 8,
  height = 7,
  dpi = 300
)

#======================================================================
#  CIRCULO DE CORRELACIONES
#======================================================================
# El color de las variables se asigna segun su contribución a las dimensiones

lista_ejes <- list(c(1,2), c(1,3), c(2,3))

for(ejes in lista_ejes){

  d1 <- ejes[1]
  d2 <- ejes[2]

  p_circle <- fviz_pca_var(
    res_acp,
    axes = c(d1, d2),
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  ) +
    theme_minimal() +
    labs(
      title = paste("ACP - Círculo de correlaciones Dim", d1, "-", d2)
    )

  print(p_circle)

  ggsave(
    file.path(
      dir_figures,
      paste0("acp_circulo_correlaciones_dim", d1, "_dim", d2, "_data_original.png")
    ),
    p_circle,
    width = 8,
    height = 7,
    dpi = 300
  )
}

#======================================================================
#  RESULTADOS DE INDIVIDUOS
#======================================================================
coord_ind <- as.data.frame(res_acp$ind$coord)
coord_ind$BPN <- df_acp$BPN

contrib_ind <- as.data.frame(res_acp$ind$contrib)
contrib_ind$BPN <- df_acp$BPN

cos2_ind <- as.data.frame(res_acp$ind$cos2)
cos2_ind$BPN <- df_acp$BPN

write.csv(
  coord_ind,
  file.path(dir_tables, "acp_scores_individuos_data_original.csv"),
  row.names = FALSE
)

write.csv(
  contrib_ind,
  file.path(dir_tables, "acp_contribuciones_individuos_data_original.csv"),
  row.names = FALSE
)

write.csv(
  cos2_ind,
  file.path(dir_tables, "acp_cos2_individuos_data_original.csv"),
  row.names = FALSE
)
#======================================================================
#  PROYECCION DE INDIVIDUOS COLOREADOS POR BPN
#======================================================================

p_ind_bpn <- fviz_pca_ind(
  res_acp,
  axes = c(1, 2),
  geom.ind = "point",
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = "jco",
  alpha.ind = 0.35,
  pointsize = 0.8,
  repel = FALSE
) +
  theme_minimal() +
  labs(title = "ACP - Individuos proyectados segun BPN")

print(p_ind_bpn)

ggsave(
  file.path(dir_figures, "acp_individuos_bpn_dim1_dim2_data_original.png"),
  p_ind_bpn,
  width = 9,
  height = 7,
  dpi = 300
)
#======================================================================
# INDIVIDUOS DIM 1-3
#======================================================================

p_ind_bpn_13 <- fviz_pca_ind(
  res_acp,
  axes = c(1,3),
  geom.ind = "point",
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = "jco",
  alpha.ind = 0.35,
  pointsize = 0.8
) +
  theme_minimal() +
  labs(
    title = "ACP - Individuos segun BPN Dim 1-3"
  )

print(p_ind_bpn_13)

ggsave(
  file.path(
    dir_figures,
    "acp_individuos_bpn_dim1_dim3_data_original.png"
  ),
  p_ind_bpn_13,
  width = 9,
  height = 7,
  dpi = 300
)

#======================================================================
# INDIVIDUOS DIM 2-3
#======================================================================

p_ind_bpn_23 <- fviz_pca_ind(
  res_acp,
  axes = c(2,3),
  geom.ind = "point",
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = "jco",
  alpha.ind = 0.35,
  pointsize = 0.8
) +
  theme_minimal() +
  labs(
    title = "ACP - Individuos segun BPN Dim 2-3"
  )

print(p_ind_bpn_23)

ggsave(
  file.path(
    dir_figures,
    "acp_individuos_bpn_dim2_dim3_data_original.png"
  ),
  p_ind_bpn_23,
  width = 9,
  height = 7,
  dpi = 300
)

#======================================================================
#  BIPLOT
#======================================================================

p_biplot <- fviz_pca_biplot(
  res_acp,
  axes = c(1, 2),
  geom.ind = "point",
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  col.var = "black",
  alpha.ind = 0.25,
  pointsize = 0.6,
  repel = TRUE
) +
  theme_minimal() +
  labs(title = "ACP - Biplot Dim 1-2 con BPN ilustrativo")

print(p_biplot)

ggsave(
  file.path(dir_figures, "acp_biplot_bpn_dim1_dim2_data_original.png"),
  p_biplot,
  width = 10,
  height = 8,
  dpi = 300
)

#======================================================================
# BIPLOT DIM 1-3
#======================================================================

p_biplot_13 <- fviz_pca_biplot(
  res_acp,
  axes = c(1,3),
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  col.var = "black",
  alpha.ind = 0.25,
  pointsize = 0.6,
  repel = TRUE
) +
  theme_minimal()

print(p_biplot_13)

ggsave(
  file.path(
    dir_figures,
    "acp_biplot_bpn_dim1_dim3_data_original.png"
  ),
  p_biplot_13,
  width = 10,
  height = 8,
  dpi = 300
)

#======================================================================
# BIPLOT DIM 2-3
#======================================================================

p_biplot_23 <- fviz_pca_biplot(
  res_acp,
  axes = c(2,3),
  habillage = df_acp$BPN,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  col.var = "black",
  alpha.ind = 0.25,
  pointsize = 0.6,
  repel = TRUE
) +
  theme_minimal()

print(p_biplot_23)

ggsave(
  file.path(
    dir_figures,
    "acp_biplot_bpn_dim2_dim3_data_original.png"
  ),
  p_biplot_23,
  width = 10,
  height = 8,
  dpi = 300
)

#======================================================================
# MAPA FACTORIAL DE VARIABLES ACTIVAS
#======================================================================

for(ejes in list(
  c(1,2),
  c(1,3),
  c(2,3)
)){

  d1 <- ejes[1]
  d2 <- ejes[2]

  p_var <- fviz_pca_var(
    res_acp,
    axes = c(d1,d2),
    repel = TRUE,
    col.var = "cos2"
  ) +
    theme_minimal() +
    labs(
      title = paste(
        "Mapa factorial de variables activas Dim",
        d1,"-",d2
      )
    )

  print(p_var)

  ggsave(
    file.path(
      dir_figures,
      paste0(
        "acp_mapa_variables_dim",
        d1,
        "_dim",
        d2,
        "_data_original.png"
      )
    ),
    p_var,
    width = 9,
    height = 8,
    dpi = 300
  )
}

#======================================================================
#  VARIABLES ILUSTRATIVAS CUALITATIVAS
#======================================================================

coord_quali <- as.data.frame(res_acp$quali.sup$coord)
coord_quali$categoria <- rownames(coord_quali)

cos2_quali <- as.data.frame(res_acp$quali.sup$cos2)
cos2_quali$categoria <- rownames(cos2_quali)

coord_quanti_sup <- as.data.frame(res_acp$quanti.sup$coord)
coord_quanti_sup$variable <- rownames(coord_quanti_sup)

vtest_quali <- as.data.frame(res_acp$quali.sup$v.test)
vtest_quali$categoria <- rownames(vtest_quali)

write.csv(
  coord_quali,
  file.path(dir_tables, "acp_coordenadas_categorias_ilustrativas_data_original.csv"),
  row.names = FALSE
)

write.csv(
  cos2_quali,
  file.path(dir_tables, "acp_cos2_categorias_ilustrativas_data_original.csv"),
  row.names = FALSE
)
write.csv(
  coord_quanti_sup,
  file.path(dir_tables, "acp_coordenadas_variables_suplementarias.csv"),
  row.names = FALSE
)

write.csv(
  vtest_quali,
  file.path(dir_tables, "acp_valores_test_categorias_ilustrativas_data_original.csv"),
  row.names = FALSE
)

print(vtest_quali)

#======================================================================
# VARIABLES ACTIVAS + VARIABLES SUPLEMENTARIAS
# CUALITATIVAS Y CUANTITATIVAS
#======================================================================

coord_quali_plot <- as.data.frame(
  res_acp$quali.sup$coord
)

coord_quali_plot$categoria <- rownames(
  coord_quali_plot
)

coord_quanti_sup <- as.data.frame(
  res_acp$quanti.sup$coord
)

coord_quanti_sup$variable <- rownames(
  coord_quanti_sup
)

for(ejes in lista_ejes){

  d1 <- ejes[1]
  d2 <- ejes[2]

  p_mix <- fviz_pca_var(
    res_acp,
    axes = c(d1, d2),
    repel = TRUE,
    col.var = "#2166AC"
  ) +

    #--------------------------------------------------
    # CATEGORÍAS ILUSTRATIVAS
    #--------------------------------------------------

    geom_point(
      data = coord_quali_plot,
      aes_string(
        x = paste0("Dim.", d1),
        y = paste0("Dim.", d2)
      ),
      color = "#D73027",
      size = 3,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = coord_quali_plot,
      aes_string(
        x = paste0("Dim.", d1),
        y = paste0("Dim.", d2),
        label = "categoria"
      ),
      color = "#D73027",
      size = 3,
      vjust = -0.6,
      inherit.aes = FALSE
    ) +

    #--------------------------------------------------
    # VARIABLE CUANTITATIVA SUPLEMENTARIA
    #--------------------------------------------------

    geom_segment(
      data = coord_quanti_sup,
      aes_string(
        x = 0,
        y = 0,
        xend = paste0("Dim.", d1),
        yend = paste0("Dim.", d2)
      ),
      color = "#1B7837",
      linewidth = 1.2,
      arrow = arrow(
        length = unit(
          0.25,
          "cm"
        )
      ),
      inherit.aes = FALSE
    ) +

    geom_text(
      data = coord_quanti_sup,
      aes_string(
        x = paste0("Dim.", d1),
        y = paste0("Dim.", d2),
        label = "variable"
      ),
      color = "#1B7837",
      size = 4,
      fontface = "bold",
      vjust = -0.8,
      inherit.aes = FALSE
    ) +

    theme_minimal() +

    labs(
      title = paste(
        "Variables activas, categorías ilustrativas y variables suplementarias",
        "Dim",
        d1,
        "-",
        d2
      ),
      subtitle =
        "Azul: variables activas | Rojo: categorías ilustrativas | Verde: variables cuantitativas suplementarias"
    )

  print(p_mix)

  ggsave(
    file.path(
      dir_figures,
      paste0(
        "acp_variables_suplementarias_dim",
        d1,
        "_dim",
        d2,
        "_data_original.png"
      )
    ),
    p_mix,
    width = 11,
    height = 8,
    dpi = 300
  )
}

#======================================================================
# BASE ACP SOLO CON BPN COMO ILUSTRATIVA
#======================================================================

variables_ilustrativas <- c("BPN")

df_acp_bpn <- df %>%
  select(
    all_of(
      c(
        variables_cuantitativas,
        variables_ilustrativas
      )
    )
  ) %>%
  mutate(
    BPN = factor(
      BPN,
      levels = c(0,1),
      labels = c(
        "Sin bajo peso al nacer",
        "Bajo peso al nacer"
      )
    )
  ) %>%
  drop_na()

n_cuant <- length(variables_cuantitativas)

res_acp_bpn <- PCA(
  df_acp_bpn,
  scale.unit = TRUE,
  quali.sup = n_cuant + 1,
  graph = FALSE
)
#======================================================================
# VARIABLES ACTIVAS + BPN
#======================================================================

#======================================================================
# VARIABLES ACTIVAS + BPN
#======================================================================

coord_bpn <- as.data.frame(
  res_acp_bpn$quali.sup$coord
)

coord_bpn$categoria <- rownames(coord_bpn)

lista_ejes <- list(
  c(1,2),
  c(1,3),
  c(2,3)
)

for(ejes in lista_ejes){

  d1 <- ejes[1]
  d2 <- ejes[2]

  p_bpn <- fviz_pca_var(
    res_acp_bpn,
    axes = c(d1,d2),
    repel = TRUE,
    col.var = "#2166AC"
  ) +

    geom_point(
      data = coord_bpn,
      aes_string(
        x = paste0("Dim.", d1),
        y = paste0("Dim.", d2)
      ),
      color = "#D73027",
      size = 5,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = coord_bpn,
      aes_string(
        x = paste0("Dim.", d1),
        y = paste0("Dim.", d2),
        label = "categoria"
      ),
      color = "#D73027",
      size = 4,
      fontface = "bold",
      vjust = -0.7,
      inherit.aes = FALSE
    ) +

    theme_minimal() +

    labs(
      title = paste(
        "Variables activas y BPN ilustrativo",
        "Dim",
        d1,
        "-",
        d2
      )
    )

  print(p_bpn)

  ggsave(
    file.path(
      dir_figures,
      paste0(
        "acp_variables_bpn_dim",
        d1,
        "_dim",
        d2,
        ".png"
      )
    ),
    p_bpn,
    width = 10,
    height = 8,
    dpi = 300
  )
}
