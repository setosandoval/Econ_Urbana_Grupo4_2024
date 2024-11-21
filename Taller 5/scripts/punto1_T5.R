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
       haven)
       #cowplot,
       #units)



# BASE DE DATOS ================================================================

# Census Track
census_track <- st_read("stores/Boundaries - Census Tracts - 2010") %>%
  st_transform(crs = 4326)

# Panel data
data <- read_dta("stores/Combined_data_Panel.dta")

# Join (hay algunos tracks en data que no están en el census track
data_sf <- data %>% 
  full_join(census_track,by = c("cod_census_track" = "geoid10"))  %>% 
  mutate(prop_white = White_Pop/Total_Pop * 100,
         prop_black = Black_Pop/Total_Pop * 100, 
         prop_hispanic = Hispanic_Pop/Total_Pop * 100) %>% 
  st_as_sf()


# Wide dataframe 

         
         
         
         