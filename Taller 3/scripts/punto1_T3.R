# ========================================================================== #
# Taller 3                                                                   #
# Economía Urbana - 202420                                                   #
#                                                                            #
# Script: Punto 1                                                            #
#                                                                            #
# Grupo 4: - Sergio Sandoval                                                 #
#          - María Fernanda Blanco                                           #
# ========================================================================== #


# CONFIGURACIÓN DEL ENTORNO Y CARGA DE PAQUETES ==============================
rm(list = ls())

#  Directorio 
setwd("/Users/setosandoval/Desktop/Urbana/Repositorio/Econ_Urbana_Grupo4_2024/Taller 3")

# Paquetes
library(pacman)
p_load(
  tidyverse,  # Carga ggplot2, dplyr, tidyr, readr, etc.
  rio,        # Lectura y escritura de datos en diversos formatos
  osmdata,    # Acceso a datos de OpenStreetMap
  sf,         # Manipulación de datos espaciales en formato 'simple features'
  tmaptools,  # Herramientas para la manipulación de datos espaciales y mapas
  stargazer,  # Generación de tablas de regresión estéticas
  lmtest,     # Pruebas para modelos lineales
  sandwich,   # Estimación de varianzas robustas, 
  SFD,        # Spatial First Difference 
  conleyreg,   # Errores conley
  fixest   # Estimación efectos fijos
)

# Base de datos
df <- readRDS("stores/db_ejercicio1.Rds")
db <- as_tibble(df)








