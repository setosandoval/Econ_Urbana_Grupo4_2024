# ========================================================================== #
# Taller 4                                                                   #
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
setwd("/Users/setosandoval/Desktop/Urbana/Repositorio/Econ_Urbana_Grupo4_2024/Taller 4")

# Paquetes
library(pacman)
p_load(tidyverse, 
       ggplot2, 
       dplyr, 
       sf, 
       cowplot,
       units)



# BASE DE DATOS ================================================================

# Cargar el archivo de datos
load("stores/Taller4_Ejercicio1.Rdata")

# Unir la población con las geometrías de los barrios
poblacion <- poblacion %>% rename(ZONA180 = zona180)  # Cambiar a mayúsculas para coincidir
barrios_poblacion <- barrios %>%
  left_join(poblacion, by = "ZONA180")

# Contar restaurantes por zona en 2004
restaurantes_2004 <- restaurants %>%
  filter(!is.na(lat2004)) %>%  # Filtrar solo los que tienen datos en 2004
  count(zona180) %>%
  rename(restaurants_2004 = n)

# Contar restaurantes por zona en 2012
restaurantes_2012 <- restaurants %>%
  filter(!is.na(lat2012)) %>%  # Filtrar solo los que tienen datos en 2012
  count(zona180) %>%
  rename(restaurants_2012 = n)

# Restarantes por mil habitantes en ambos años
barrios_poblacion <- barrios_poblacion %>%
  left_join(restaurantes_2004, by = c("ZONA180" = "zona180")) %>%
  left_join(restaurantes_2012, by = c("ZONA180" = "zona180")) %>%
  mutate(
    restaurants_2004 = ifelse(is.na(restaurants_2004), 0, restaurants_2004),
    restaurants_2012 = ifelse(is.na(restaurants_2012), 0, restaurants_2012),
    per_capita_2004 = (restaurants_2004 / day_pop) * 1000,
    per_capita_2012 = (restaurants_2012 / day_pop) * 1000
  )

# Calcular el promedio de restaurantes per cápita en ambos años
avg_per_capita_2004 <- mean(barrios_poblacion$per_capita_2004, na.rm = TRUE)
avg_per_capita_2012 <- mean(barrios_poblacion$per_capita_2012, na.rm = TRUE)

# Calcular per capita relativo al promedio de la ciudad y tasa crecimiento
barrios_poblacion <- barrios_poblacion %>%
  mutate(per_capita_2004_rel = per_capita_2004 - avg_per_capita_2004, 
         per_capita_2012_rel = per_capita_2012 - avg_per_capita_2012,  
         growth_rate = log(per_capita_2012) - log(per_capita_2004))



# MAPAS FIGURA 1 ===============================================================

# Mapa de restaurantes por mil hábitantes en 2004 relativo al promedio de la ciudad
mapa_2004 <- ggplot(data = barrios_poblacion) +
  geom_sf(aes(fill = cut(per_capita_2004_rel, 
                         breaks = c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14), 
                         labels = c("-4 – -2", "-2 – 0", "0 – 2", "2 – 4", 
                                    "4 – 6", "6 – 8", "8 – 10", "10 – 12", "12 – 14"))),
          color = "black", size = 2) +
  scale_fill_manual(values = c("#fef4eb", "#fce4cd", "#fcd0a2", "#f9ad6b", "#f58c41", 
                               "#ef6a22", "#d74c26", "#a43922", "#a43922"),
                    guide = guide_legend(reverse = TRUE, override.aes = list(size = 14))) +
  labs(title = "Per capita number of restaurants in 2004") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",   # none porque se muestra leyenda (la misma) en el mapa 2012 
        legend.background = element_rect(color = "black", fill = NA), 
        legend.title = element_blank())

# Mostrar el mapa
print(mapa_2004)


# Mapa de restaurantes por mil hábitantes en 2012 relativo al promedio de la ciudad
mapa_2012 <- ggplot(data = barrios_poblacion) +
  geom_sf(aes(fill = cut(per_capita_2012_rel, 
                         breaks = c(-4.4, -2, 0, 2, 4, 6, 8, 10, 12, 14), 
                         labels = c("-4 – -2", "-2 – 0", "0 – 2", "2 – 4", 
                                    "4 – 6", "6 – 8", "8 – 10", "10 – 12", "12 – 14"))),
          color = "black", size = 2) +
  scale_fill_manual(values = c("#fef4eb", "#fce4cd", "#fcd0a2", "#f9ad6b", "#f58c41", 
                               "#ef6a22", "#d74c26", "#a43922", "#a43922"),
                    guide = guide_legend(reverse = TRUE, override.aes = list(size = 8)))+
  labs(title = "Per capita number of restaurants in 2012") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.background = element_rect(color = "black", fill = NA), 
        legend.title = element_blank())

# Mostrar el mapa
print(mapa_2012)


# Mapa del crecimiento porcentual de restaurantes per cápita de 2004 a 2012
growth <- ggplot(data = barrios_poblacion) +
  geom_sf(aes(fill = cut(growth_rate, 
                         breaks = c(-1.4, -0.3, -0.2, 0, 0.2, 0.3, 0.61), 
                         labels = c("-0.6 – 0.3", "-0.3 – -0.2", "-0.3 – 0", "0 – 0.2", 
                                    "0.2 – 0.3", "0.3 – 0.6"))),
          color = "black", size = 2) +
  scale_fill_manual(values = c("#e1f3fc", "#b0d7f2", "#96b5d9", "#6c93bd", 
                               "#265d95", "#1d3d6d"),
                    guide = guide_legend(reverse = TRUE, override.aes = list(size = 8)))+
  labs(title = "Percent growth in the number of per capita restaurants") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.background = element_rect(color = "black", fill = NA), 
        legend.title = element_blank())

# Mostrar el mapa
print(growth)


# Crear la figura combinada en una sola fila
figura_combined <- plot_grid(mapa_2004, mapa_2012, ncol = 2, align = "h")

# Mostrar la figura combinada
print(figura_combined)

# Crear la figura final con 3 mapas
figura_final <- plot_grid(figura_combined, growth, ncol = 1, rel_heights = c(1, 1))

# Mostrar la figura final
print(figura_final)

# Guardar la figura 1 final 
ggsave("views/P1_mapas_final.pdf", figura_final, width = 16, height = 12, dpi = 300)



# DENSIDADES  ==================================================================

# Filtrar datos para los años 2004 y 2012
df_04 <- filter(restaurants, !is.na(prezzo2004))
df_12 <- filter(restaurants, !is.na(prezzo2012))

# Densidad para el año 2004 y 2012 usando kernel Gaussiano
gau_04 <- density(df_04$prezzo2004, bw = 'nrd0', kernel = 'gaussian')
gau_12 <- density(df_12$prezzo2012, bw = 'nrd0', kernel = 'gaussian')

# Gráfico Gaussiano
plot_gaussian <- ggplot() +
  geom_area(aes(x = gau_04$x, y = gau_04$y, fill = '2004'), alpha = 0.3) + 
  geom_area(aes(x = gau_12$x, y = gau_12$y, fill = '2012'), alpha = 0.3) +
  labs(title = "Kernel Gaussiano",
       subtitle = paste("Bw para 2004 y 2012 respectivamente:", round(gau_04$bw, 2), "y", round(gau_12$bw, 2)),
       x = "Precio", y = "Densidad") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "€")) + 
  scale_fill_manual(values = c("2004" = "blue", "2012" = "red"), name = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(size = 28),
        legend.position = "none")

# Densidad para el año 2004 y 2012 usando kernel Epanechnikov
epa_04 <- density(df_04$prezzo2004, bw = 'nrd0', kernel = 'epanechnikov')
epa_12 <- density(df_12$prezzo2012, bw = 'nrd0', kernel = 'epanechnikov')

# Gráfico Epanechnikov
plot_epanechnikov <- ggplot() +
  geom_area(aes(x = epa_04$x, y = epa_04$y, fill = '2004'), alpha = 0.3) + 
  geom_area(aes(x = epa_12$x, y = epa_12$y, fill = '2012'), alpha = 0.3) +
  labs(title = "Kernel Epanechnikov",
       subtitle = paste("Bw para 2004 y 2012 respectivamente:", round(epa_04$bw, 2), "y", round(epa_12$bw, 2)),
       x = "Precio", y = "Densidad") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "€")) + 
  scale_fill_manual(values = c("2004" = "blue", "2012" = "red"), name = "Año") +
  theme_minimal() +
  theme(plot.title = element_text(size = 28))

# Crear la figura final con 3 mapas
gaussian_epanechnikov <- plot_grid(plot_gaussian, plot_epanechnikov, ncol = 2, align = "h")

# Mostrar la figura final
print(gaussian_epanechnikov)

# Guardar lambas combinadas
ggsave("views/P1_densidades.pdf", gaussian_epanechnikov, width = 16, height = 6, dpi = 300)


# Calcular el ancho de banda base usando la regla "rule-of-thumb"
bw_2004 <- bw.nrd0(df_04$prezzo2004)
bw_2012 <- bw.nrd0(df_12$prezzo2012)

# Definir factores de ajuste para el ancho de banda
adjustments <- c(0.5, 1, 2)
temp_files <- character()

# Loop para estimación de densidades con diferentes ajustes al ancho de banda
for (i in adjustments) {
  # Aplicar el ajuste multiplicando el ancho de banda base
  adjusted_bw_2004 <- bw_2004 * i
  adjusted_bw_2012 <- bw_2012 * i
  
  # Título
  title_text <- ifelse(i == 0.5, "Mitad de Rule-of-thumb",
                       ifelse(i == 1, "Rule-of-thumb",
                              "Doble de rule-of-thumb"))
  
  # Densidad Epanechnikov para 2004 y 2012
  epa_04 <- density(df_04$prezzo2004, bw = adjusted_bw_2004, kernel = 'epanechnikov')
  epa_12 <- density(df_12$prezzo2012, bw = adjusted_bw_2012, kernel = 'epanechnikov')
  
  # Gráfico para el ancho de banda ajustado actual
  plot_epa <- ggplot() +
    geom_area(aes(x = epa_04$x, y = epa_04$y, fill = '2004'), alpha = 0.3) + 
    geom_area(aes(x = epa_12$x, y = epa_12$y, fill = '2012'), alpha = 0.3) +
    labs(title = title_text,
         subtitle = paste("Bw para 2004 y 2012 respectivamente:", 
                          round(adjusted_bw_2004, 2), "y", round(adjusted_bw_2012, 2)),
         x = "Precio", y = "Densidad") +
    scale_x_continuous(labels = scales::dollar_format(prefix = "€")) + 
    scale_fill_manual(values = c("2004" = "blue", "2012" = "red"), name = "Año") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16),
          legend.position = if (i == 2) "right" else "none")  # Solo mostrar la leyenda en el último gráfico

  # Guardar el gráfico en un archivo temporal
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot_epa, width = 5, height = 4)
  temp_files <- c(temp_files, temp_file)  
}

# Cargar los gráficos y combinarlos en una figura final
plots <- lapply(temp_files, function(x) ggdraw() + draw_image(x))
rule_t_combined <- plot_grid(plotlist = plots, ncol = 3)

# Guardar la figura combinada
ggsave("views/P1_densidades2.pdf", rule_t_combined, width = 14, height = 4, dpi = 300)

# Eliminar los archivos temporales después de usarlos
file.remove(temp_files)



# TEST AGLOMERACIÓN  ===========================================================

# Filtrar los 5 barrios con mayor crecimiento
top_5_barrios <- barrios_poblacion %>%
  arrange(desc(growth_rate)) %>%
  slice(1:5)

temp_files <- character()

# Función para calcular la densidad para muestras simuladas
density_calculator <- function(dta) {
  dta <- st_distance(dta)
  dta[lower.tri(dta, diag = TRUE)] <- NA
  dta <- c(dta)
  dta <- dta[!is.na(dta)]
  dta <- set_units(dta, "km") # Asegurando unidades en kilómetros
  dta <- dta[dta <= set_units(1, "km")]
  dens <- density(as.numeric(dta), bw = "nrd", kernel = "gaussian", from = 0, to = 1)
  return(dens$y)
}

# Loop para calcular y graficar las distribuciones para cada uno de los 5 barrios
for (i in 1:nrow(top_5_barrios)) {
  barrio <- top_5_barrios[i, ]
  
  # Datos de restaurantes en 2004
  rest_en_barrio_2004 <- restaurants %>%
    filter(!is.na(lat2004) & zona180 == barrio$ZONA180) %>%
    st_as_sf(coords = c("long2004", "lat2004"), crs = 4326) %>%
    st_transform(crs = 6875)  # Proyección en metros
  
  # Datos de restaurantes en 2012
  rest_en_barrio_2012 <- restaurants %>%
    filter(!is.na(lat2012) & zona180 == barrio$ZONA180) %>%
    st_as_sf(coords = c("long2012", "lat2012"), crs = 4326) %>%
    st_transform(crs = 6875)  # Proyección en metros
  
  # Distancias observadas para 2004 y 2012
  bilat_distances_2004 <- st_distance(rest_en_barrio_2004)
  bilat_distances_2004[lower.tri(bilat_distances_2004, diag = TRUE)] <- NA
  bilat_distances_2004 <- set_units(bilat_distances_2004, "km")
  bilat_distances_2004 <- as.numeric(bilat_distances_2004[!is.na(bilat_distances_2004)])
  bilat_distances_2004 <- bilat_distances_2004[bilat_distances_2004 <= 1]
  
  bilat_distances_2012 <- st_distance(rest_en_barrio_2012)
  bilat_distances_2012[lower.tri(bilat_distances_2012, diag = TRUE)] <- NA
  bilat_distances_2012 <- set_units(bilat_distances_2012, "km")
  bilat_distances_2012 <- as.numeric(bilat_distances_2012[!is.na(bilat_distances_2012)])
  bilat_distances_2012 <- bilat_distances_2012[bilat_distances_2012 <= 1]
  
  dens_obs_2004 <- density(bilat_distances_2004, bw = "nrd", kernel = "gaussian", from = 0, to = 1)
  dens_obs_2012 <- density(bilat_distances_2012, bw = "nrd", kernel = "gaussian", from = 0, to = 1)
  
  # Simulaciones para intervalos de confianza
  boot <- list()
  for (j in 1:100) {
    boot[[j]] <- st_sample(barrio, size = nrow(rest_en_barrio_2004), type = "random")
  }
  boot_dens <- lapply(boot, density_calculator)
  density_matrix <- do.call(cbind, boot_dens)
  
  # Calcular los percentiles para los intervalos de confianza
  q05 <- apply(density_matrix, 1, function(x) quantile(x, probs = 0.05))
  q95 <- apply(density_matrix, 1, function(x) quantile(x, probs = 0.95))
  
  # Data frame para el gráfico
  densidad_combined <- data.frame(
    locations = dens_obs_2004$x,
    observada_2004 = dens_obs_2004$y,
    observada_2012 = dens_obs_2012$y,
    q05 = q05,
    q95 = q95
  )
  
  # Graficar las densidades observadas y los intervalos de confianza
  plot <- ggplot(densidad_combined) +
    geom_line(aes(x = locations, y = observada_2004, color = "2004"), size = 1) +
    geom_line(aes(x = locations, y = observada_2012, color = "2012"), size = 1) +
    geom_line(aes(x = locations, y = q05), linetype = "dashed", color = "black") +
    geom_line(aes(x = locations, y = q95), linetype = "dashed", color = "black") +
    scale_color_manual(values = c("2004" = "red", "2012" = "blue"), name = "Año") +
    labs(
      title = paste("Zona", barrio$ZONA180),
      x = "Distancia (km)", y = "Densidad"
    ) +
    theme_minimal() + 
    theme(plot.title = element_text(size = 16))
  
  # Guardar el gráfico en un archivo temporal
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot, width = 5, height = 4)
  temp_files <- c(temp_files, temp_file)  
}

# Combinación tests 5 zonas
plots_test <- lapply(temp_files, function(x) ggdraw() + draw_image(x))

plot_combined1 <- plot_grid(plots_test[[1]], plots_test[[2]], ncol = 2, align = "h")
plot_combined2 <- plot_grid(plots_test[[3]], plots_test[[4]], ncol = 2, align = "h")
plot_combined3 <- plot_grid(plot_combined1, plot_combined2, ncol = 1, align = "h")
plot_combined4 <- plot_grid(plot_combined3,plots_test[[5]], ncol = 1, rel_heights = c(2, 1), align = "h")

# Mostrar la figura combinada
print(plot_combined4)

# Guardar plot
ggsave("views/P1_tests.pdf", plot_combined4, width = 12, height = 16, dpi = 300)
