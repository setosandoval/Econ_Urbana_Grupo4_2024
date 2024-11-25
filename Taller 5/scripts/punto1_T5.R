# ========================================================================== #
# Taller 5                                                                   #
# Economía Urbana - 202420                                                   #
#                                                                            #
# Script: Punto 1                                                            #
#                                                                            #
# Grupo 4: - Sergio Sandoval                                                 #
#          - María Fernanda Blanco                                           #
# ========================================================================== #


# CONFIGURACIÓN DEL ENTORNO Y CARGA DE PAQUETES ================================
rm(list = ls())

#  Directorio 
setwd("/Users/setosandoval/Desktop/Urbana/Repositorio/Econ_Urbana_Grupo4_2024/Taller 5")

# Paquetes
library(pacman)
p_load(tidyverse, 
       ggplot2, 
       dplyr,
       sf,
       haven,
       cowplot,
       ggrepel,
       stargazer)

#Semilla
set.seed(202028276)


# BASE DE DATOS ================================================================

# Census Track
census_track <- st_read("stores/Boundaries - Census Tracts - 2010") %>%
  st_transform(crs = 4326)

# Panel data
df <- read_dta("stores/Combined_data_Panel.dta")

# Join (hay algunos tracks en data que no están en el census track
data_join <- df %>% 
  inner_join(census_track,by = c("cod_census_track" = "geoid10"))  %>% 
  mutate(prop_white = White_Pop/Total_Pop * 100,
         prop_black = Black_Pop/Total_Pop * 100, 
         prop_hispanic = Hispanic_Pop/Total_Pop * 100) %>% 
  st_as_sf()

# Filtrar observaciones con geometría vacía en otro data frame
data_sf <- data_join %>%
  filter(!st_is_empty(geometry))


# Coordenadas del CBD de Chicago
cbd_coords <- tibble(
  longitude = -87.6278,
  latitude = 41.8819
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



# MAPAS PROPORCIONES ===========================================================

# Calcular el promedio de proporciónes y mediana ingreso
data_sf_avg <- data_sf %>%
  group_by(cod_census_track) %>%
  summarise(
    prop_black_avg = mean(prop_black, na.rm = TRUE),
    prop_hispanic = mean(prop_hispanic, na.rm = TRUE),
    income = mean(Median_Inc, na.rm = TRUE)/1000
  )

# Mediana Ingreso: Promedio de los años 2000, 2015 y 2020
map_1 <- ggplot() +
  geom_sf(data = data_sf_avg, aes(fill = income)) +
  geom_sf(data = cbd_coords, color = "black", size = 3, shape = 21, fill = "black") +
  ggrepel::geom_label_repel(data = cbd_coords, aes(label = "CBD, Chicago", geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  scale_fill_gradient(
    low = "white",
    high = "red",
    name = "Ingreso",
  ) +
  theme_void()

# Proporción Población Afro-Americana: Promedio de los años 2000, 2015 y 2020
map_2 <- ggplot() +
  geom_sf(data = data_sf_avg, aes(fill = prop_black_avg)) +
  geom_sf(data = cbd_coords, color = "black", size = 3, shape = 21, fill = "black") +
  ggrepel::geom_label_repel(data = cbd_coords, aes(label = "CBD, Chicago", geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(title = "Promedio Proporción Afro-Americana") +
  scale_fill_gradient(
    low = "white",
    high = "blue",
    name = "Proporción"
  ) +
  theme_void()+
  theme(plot.title = element_text(size = 15))

# Proporción Población Hispana: Promedio de los años 2000, 2015 y 2020
map_3 <- ggplot() +
  geom_sf(data = data_sf_avg, aes(fill = prop_hispanic)) +
  geom_sf(data = cbd_coords, color = "black", size = 3, shape = 21, fill = "black") +
  ggrepel::geom_label_repel(data = cbd_coords, aes(label = "CBD, Chicago", geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(title = "Promedio Proporción  Hispana") +
  scale_fill_gradient(
    low = "white",
    high = "blue",
    name = "Proporción"
  ) +
  theme_void()+
  theme(plot.title = element_text(size = 15))


# Calcular el cambio porcentual entre 2020 y 2000
data_change <- data_sf %>%
  filter(year %in% c(2000, 2020)) %>% # Filtrar solo los años 2000 y 2020
  select(cod_census_track, year, prop_black, prop_hispanic) %>%
  pivot_wider(
    names_from = year, 
    values_from = c(prop_black, prop_hispanic),
    names_prefix = "year_"
  ) %>%
  mutate(
    change_percent_black = prop_black_year_2020 - prop_black_year_2000,
    change_percent_hispanic = prop_hispanic_year_2020 - prop_hispanic_year_2000
  )

# Cambio Porcentual en la Proporción de Población Afro-Americana 2000-2020
map_4 <- ggplot() +
  geom_sf(data = data_change, aes(fill = change_percent_black)) +
  geom_sf(data = cbd_coords, color = "black", size = 3, shape = 21, fill = "black") +
  ggrepel::geom_label_repel(data = cbd_coords, aes(label = "CBD, Chicago", geometry = geometry), 
                            stat = "sf_coordinates", 
                            size = 3.5, 
                            nudge_x = 0.15, 
                            nudge_y = 0.08) +
  labs(
    title = "Cambio Porcentual Proporción Afro-Americana",,
    fill = "Cambio (%)"
  ) +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "green",
    midpoint = 0,
    name = "Cambio (%)",
    na.value = "white"
  ) +
  theme_void()+
  theme(plot.title = element_text(size = 15))

# Cambio Porcentual en la Proporción de Población Hispana 2000-2020
map_5 <- ggplot() +
  geom_sf(data = data_change, aes(fill = change_percent_hispanic)) +
  geom_sf(data = cbd_coords, color = "black", size = 3, shape = 21, fill = "black") +
  ggrepel::geom_label_repel(data = cbd_coords, aes(label = "CBD, Chicago", geometry = geometry), 
                   stat = "sf_coordinates", 
                   size = 3.5, 
                   nudge_x = 0.15, 
                   nudge_y = 0.08) +
  labs(
    title = "Cambio Porcentual Proporción Hispana",
    fill = "Cambio (%)"
  ) +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "green",
    midpoint = 0,
    name = "Cambio (%)",
    na.value = "white"
  ) +
  theme_void()+
  theme(plot.title = element_text(size = 15))

# Mapas Combinados
figure_1 <- plot_grid(map_2, map_4, ncol = 2, align = "h")
figure_2 <- plot_grid(map_3, map_5, ncol = 2, align = "h")
figura_3 <- plot_grid(figure_1, figure_2, ncol = 1, rel_heights = c(1, 1))

# Guardar figuras
ggsave("views/P1_mapa1.pdf", map_1, width = 16, height = 12, dpi = 300)
ggsave("views/P1_mapa2.pdf", figura_3, width = 16, height = 12, dpi = 300)




# INDEX ========================================================================

# Calcular Poblaciones Totales por Año
temp <- data_join %>%
  dplyr::select(Total_Pop, White_Pop, Black_Pop, Hispanic_Pop, year, cod_census_track) %>%
  group_by(year) %>%
  mutate(
    totalpop = sum(Total_Pop, na.rm = TRUE),  # Total población por año
    totalpopW = sum(White_Pop, na.rm = TRUE), # Total población blanca por año
    totalpopB = sum(Black_Pop, na.rm = TRUE), # Total población afroamericana por año
    totalpopH = sum(Hispanic_Pop, na.rm = TRUE) # Total población hispana por año
  )

# Calcular Proporciones de Población
temp <- temp %>%
  group_by(year, cod_census_track) %>%
  mutate(
    pWC = White_Pop / totalpopW, # Proporción de blancos
    pBC = Black_Pop / totalpopB, # Proporción de afroamericanos
    pHC = Hispanic_Pop / totalpopH, # Proporción de hispanos
    WoverWH = White_Pop / (White_Pop + Hispanic_Pop), # Relación blancos vs hispanos
    BoverWB = Black_Pop / (White_Pop + Black_Pop),    # Relación afroamericanos vs blancos
    HoverWH = Hispanic_Pop / (White_Pop + Hispanic_Pop) # Relación hispanos vs blancos
  )

# Calcular Índices de Dissimilarity e Isolation
temp <- temp %>%
  group_by(year) %>%
  mutate(
    dissimilarityB = 0.5 * sum(abs(pBC - pWC), na.rm = TRUE), # Dissimilarity: Afroamericanos vs Blancos
    dissimilarityH = 0.5 * sum(abs(pHC - pWC), na.rm = TRUE), # Dissimilarity: Hispanos vs Blancos
    IsolationB = sum(pBC * BoverWB, na.rm = TRUE),            # Isolation: Afroamericanos
    IsolationH = sum(pHC * HoverWH, na.rm = TRUE)             # Isolation: Hispanos
  )

# Resultados por Año
resultados <- temp %>%
  group_by(year) %>%
  summarise(
    dissimilarityB = unique(dissimilarityB),
    dissimilarityH = unique(dissimilarityH),
    IsolationB = unique(IsolationB),
    IsolationH = unique(IsolationH)
  )

# Crear la tabla con los datos relevantes
tabla_indices <- resultados %>%
  st_drop_geometry() %>%  # Eliminar la columna de geometría
  select(year, dissimilarityB, IsolationB, dissimilarityH, IsolationH) %>%
  rename(
    Año = year,
    `Dissimilarity Afroamericanos` = dissimilarityB,
    `Isolation Afroamericanos` = IsolationB,
    `Dissimilarity Hispanos` = dissimilarityH,
    `Isolation Hispanos` = IsolationH
  )

# Exportar la tabla a LaTeX con stargazer
stargazer(
  tabla_indices,
  summary = FALSE,
  rownames = FALSE,
  title = "Índices de Dissimilarity e Isolation por Año",
  label = "tab:indices",
  header = FALSE,
  out = "views/table_3.tex",
  digits = 3
)

# Convertir los años a factor para que tengan intervalos iguales
resultados$year <- factor(resultados$year, levels = c(2000, 2015, 2020))

# Gráfica para Afroamericanos
graph_1 <- ggplot(resultados, aes(x = year)) +
  geom_line(aes(y = dissimilarityB, color = "Dissimilarity Index"), group = 1, size = 1) +
  geom_line(aes(y = IsolationB, color = "Isolation Index"), group = 1, size = 1) +
  scale_y_continuous(
    limits = c(0.78, 0.84),
    breaks = seq(0.78, 0.84, by = 0.01),
    name = "Index"
  ) +
  scale_x_discrete(
    name = "Año",
    expand = c(0, 0.2) 
  ) +
  scale_color_manual(
    values = c("Dissimilarity Index" = "blue", "Isolation Index" = "red"),
    name = "Index"
  ) +
  labs(
    title = "Afroamericanos",
    x = "Año",
    y = "Índice"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )

# Gráfica para Hispanos
graph_2 <-ggplot(resultados, aes(x = year)) +
  geom_line(aes(y = dissimilarityH, color = "Dissimilarity Index"), group = 1, size = 1) +
  geom_line(aes(y = IsolationH, color = "Isolation Index"), group = 1, size = 1) +
  scale_y_continuous(
    limits = c(0.56, 0.64),
    breaks = seq(0.56, 0.64, by = 0.01),
    name = "Index"
  ) +
  scale_x_discrete(
    name = "Año",
    expand = c(0, 0.2) 
  ) +
  scale_color_manual(
    values = c("Dissimilarity Index" = "blue", "Isolation Index" = "red"),
    name = "Index"
  ) +
  labs(
    title = "Hispanos",
    x = "Año",
    y = "Índice"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12)
  )

# Figura Combinados
figure_4 <- plot_grid(graph_1, graph_2, ncol = 2, align = "h")

# Guardar figuras
ggsave("views/P2_index.pdf", figure_4, width = 18, height = 6, dpi = 300)



# TIPPING POINTS ===============================================================


# Transformar los datos panel a formato ancho y calcular cambios
data_wide <- data_join %>%
  select(c(cod_census_track, year, Median_Inc, White_Pop, Total_Pop, Black_Pop, Hispanic_Pop, prop_white, prop_black, prop_hispanic, geometry)) %>%
  pivot_wider(
    names_from = "year",
    id_cols = c("cod_census_track", "geometry"),
    values_from = c("Total_Pop", "White_Pop", "Black_Pop", "Hispanic_Pop", "prop_white", "prop_black", "prop_hispanic")
  ) %>%
  st_as_sf() # Asegurar que los datos mantengan propiedades espaciales

# Cálculo de proporciones y cambios poblacionales
data <- data_wide %>%
  mutate(across(starts_with("prop"), ~ ./100)) %>%
  mutate(
    prop_minority_2000 = (Total_Pop_2000 - White_Pop_2000) / Total_Pop_2000,
    prop_minority_2015 = (Total_Pop_2015 - White_Pop_2015) / Total_Pop_2015,
    d_white_2000_2015 = (White_Pop_2015 - White_Pop_2000) / Total_Pop_2000,
    d_white_2000_2020 = (White_Pop_2020 - White_Pop_2000) / Total_Pop_2000,
    d_white_2015_2020 = (White_Pop_2020 - White_Pop_2015) / Total_Pop_2015
  )

# Función para encontrar tipping points
tipping <- function(df, minority, dwhite, year) {
  # Crear un vector para almacenar los valores de R^2
  rsquared_values <- numeric(500)
  
  # Bucle para probar diferentes umbrales
  for (i in 1:500) {
    df_temp <- df %>%
      mutate(dummy = ifelse(eval(parse(text = paste0("prop_", minority, "_", year))) > i / 1000, 1, 0))
    
    # Ajustar el modelo de regresión
    regression_model <- lm(eval(parse(text = dwhite)) ~ dummy, data = df_temp)
    rsquared_values[i] <- summary(regression_model)$r.squared
  }
  
  # Encontrar el máximo R^2 y el mejor porcentaje
  max_rsquared <- max(rsquared_values)
  best_percentage <- which(rsquared_values == max_rsquared) / 10
  
  # En caso de múltiples valores máximos, seleccionar el menor umbral
  if (length(best_percentage) > 1) {
    best_percentage <- min(best_percentage)
  }
  
  # Retornar el tipping point y el mejor R^2
  list(tipping_point = best_percentage, best_r2 = max_rsquared)
}

# Función para calcular tipping points para cada grupo
calculate_tipping <- function(data, group) {
  list(
    tipping_2000_2015 = tipping(data, group, "d_white_2000_2015", 2000),
    tipping_2000_2020 = tipping(data, group, "d_white_2000_2020", 2000),
    tipping_2015_2020 = tipping(data, group, "d_white_2015_2020", 2015)
  )
}

# Minorías (no blancos)
results_minority <- calculate_tipping(data, "minority")

# Afroamericanos
results_black <- calculate_tipping(data, "black")

# Hispanos
results_hispanic <- calculate_tipping(data, "hispanic")


# Crear tabla final con los resultados
results_tipping <- data.frame(
  Periodo = c("2000-2015", "2000-2020", "2015-2020"),
  `Minorías (no blancos)` = sapply(results_minority, `[[`, "tipping_point"),
  `Afroamericanos` = sapply(results_black, `[[`, "tipping_point"),
  `Hispanos` = sapply(results_hispanic, `[[`, "tipping_point"),
  `Mejor R2 Minorías` = sapply(results_minority, `[[`, "best_r2"),
  `Mejor R2 Afroamericanos` = sapply(results_black, `[[`, "best_r2"),
  `Mejor R2 Hispanos` = sapply(results_hispanic, `[[`, "best_r2")
)

# Verificar la tabla
print(results_tipping)

stargazer(
  results_tipping,
  type = "latex",
  summary = FALSE,
  title = "Resultados de Tipping Points y Mejor R2",
  label = "tab:tipping_points",
  rownames = FALSE,
  out = "views/table_4.tex",
  digits = 3
)

# Mapas 

# Crear las columnas necesarias antes de los dummies
data <- data %>%
  mutate(
    prop_minority_2000 = (Total_Pop_2000 - White_Pop_2000) / Total_Pop_2000,
    prop_minority_2015 = (Total_Pop_2015 - White_Pop_2015) / Total_Pop_2015,
    prop_black_2000 = Black_Pop_2000 / Total_Pop_2000,
    prop_black_2015 = Black_Pop_2015 / Total_Pop_2015,
    prop_hispanic_2000 = Hispanic_Pop_2000 / Total_Pop_2000,
    prop_hispanic_2015 = Hispanic_Pop_2015 / Total_Pop_2015
  )

# Tipping points existan en el entorno
nw_tip_2000_2015 <- results_minority$tipping_2000_2015$tipping_point
nw_tip_2000_2020 <- results_minority$tipping_2000_2020$tipping_point
nw_tip_2015_2020 <- results_minority$tipping_2015_2020$tipping_point

black_tip_2000_2015 <- results_black$tipping_2000_2015$tipping_point
black_tip_2000_2020 <- results_black$tipping_2000_2020$tipping_point
black_tip_2015_2020 <- results_black$tipping_2015_2020$tipping_point

hispanic_tip_2000_2015 <- results_hispanic$tipping_2000_2015$tipping_point
hispanic_tip_2000_2020 <- results_hispanic$tipping_2000_2020$tipping_point
hispanic_tip_2015_2020 <- results_hispanic$tipping_2015_2020$tipping_point


# Crear las columnas de dummy para los tipping points
data <- data %>%
  mutate(
    dum_nw_tip_2000_2015 = ifelse(prop_minority_2000 >= nw_tip_2000_2015 / 100, 1, 0),
    dum_nw_tip_2000_2020 = ifelse(prop_minority_2000 >= nw_tip_2000_2020 / 100, 1, 0),
    dum_nw_tip_2015_2020 = ifelse(prop_minority_2015 >= nw_tip_2015_2020 / 100, 1, 0),
    dum_black_tip_2000_2015 = ifelse(prop_black_2000 >= black_tip_2000_2015 / 100, 1, 0),
    dum_black_tip_2000_2020 = ifelse(prop_black_2000 >= black_tip_2000_2020 / 100, 1, 0),
    dum_black_tip_2015_2020 = ifelse(prop_black_2015 >= black_tip_2015_2020 / 100, 1, 0),
    dum_hispanic_tip_2000_2015 = ifelse(prop_hispanic_2000 >= hispanic_tip_2000_2015 / 100, 1, 0),
    dum_hispanic_tip_2000_2020 = ifelse(prop_hispanic_2000 >= hispanic_tip_2000_2020 / 100, 1, 0),
    dum_hispanic_tip_2015_2020 = ifelse(prop_hispanic_2015 >= hispanic_tip_2015_2020 / 100, 1, 0)
  )

# Función para crear mapas con parámetros personalizados
create_map <- function(data, column, title) {
  ggplot(data) +
    geom_sf(aes(fill = factor(!!sym(column))), color = "white", size = 0.1) +
    scale_fill_manual(
      values = c("1" = "#FF5733", "0" = "#4CAF50"),
      guide = FALSE  # No mostrar la leyenda
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}

# Crear los mapas
map_6 <- create_map(data, "dum_nw_tip_2000_2015", "Minorías 2000-2015")
map_7 <- create_map(data, "dum_nw_tip_2000_2020", "Minorías 2000-2020")
map_8 <- create_map(data, "dum_nw_tip_2015_2020", "Minorías 2015-2020")

map_9 <- create_map(data, "dum_black_tip_2000_2015", "Afroamericanos 2000-2015")
map_10 <- create_map(data, "dum_black_tip_2000_2020", "Afroamericanos 2000-2020")
map_11 <- create_map(data, "dum_black_tip_2015_2020", "Afroamericanos 2015-2020")

map_12 <- create_map(data, "dum_hispanic_tip_2000_2015", "Hispanos 2000-2015")
map_13 <- create_map(data, "dum_hispanic_tip_2000_2020", "Hispanos 2000-2020")
map_14 <- create_map(data, "dum_hispanic_tip_2015_2020", "Hispanos 2015-2020")


# Mapas Combinados
figure_5 <- plot_grid(map_6, map_7, map_8, ncol = 3, align = "h")
figure_6 <- plot_grid(map_9, map_10, map_11, ncol = 3, align = "h")
figure_7 <- plot_grid(map_12, map_12, map_14, ncol = 3, align = "h")
figura_8 <- plot_grid(figure_5, figure_6, figure_7, ncol = 1, rel_heights = c(1, 1))

# Guardar figura
ggsave("views/P3_mapa.pdf", figura_8, width = 16, height = 12, dpi = 300)
