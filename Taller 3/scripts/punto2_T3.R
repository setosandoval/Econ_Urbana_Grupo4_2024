# ========================================================================== #
# Taller 3                                                                   #
# Economía Urbana - 202420                                                   #
#                                                                            #
# Script: Punto 2                                                            #
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
  sfd,        # Spatial First Difference 
  conleyreg,  # Errores conley
  fixest      # Estimación efectos fijos
)


# BASE DE DATOS ==============================================================

# Se carga la base de datos
df <- readRDS("stores/db_ejercicio2.Rds")
db <- as_tibble(df)

# Se crean variables logaritmo de salario y renta
db <- db %>%
  mutate(log_w = log(salario)) %>% 
  mutate(log_r = log(arriendo))


# ESTIMACIONES ===============================================================

# Modelos log con la amenidad
model_1 <- lm(log_w ~ amenity, data = db)
model_2 <- lm(log_r ~ amenity, data = db)

# Impotar estimaciones a Latex
stargazer(model_1, model_2,
          type = "text",
          omit.stat = c("f", "adj.rsq", "ser"),
          dep.var.labels = c("Log Salario", "Log Arriendo"),
          covariate.labels = c("Amenidad"), 
          digits = 3,
          out = "views/P2_reg.tex") 


# ERRORES BOOTSTRAP =========================================================

# Semilla
set.seed(202028276) 

# Número de repeticiones 
B <- 1000

# Definir función para calcular los coeficientes de interés
bootstrap_coef <- function(data, indices) {
  # Muestra con reemplazo
  data_boot <- data[indices, ]
  
  # Reestimación de los modelos
  model_1_boot <- lm(log_w ~ amenity, data = data_boot)
  model_2_boot <- lm(log_r ~ amenity, data = data_boot)
  
  # Extraer coeficientes de amenity
  beta_amenity <- coef(model_1_boot)["amenity"]
  alpha_amenity <- coef(model_2_boot)["amenity"]
  
  return(c(beta_amenity, alpha_amenity))
}

# Ejecutar el bootstrap
results_boot <- boot::boot(data = db, statistic = bootstrap_coef, R = B)

# Extraer las estimaciones de los coeficientes
beta_boot <- results_boot$t[, 1]  # Coeficientes del modelo de salario
alpha_boot <- results_boot$t[, 2]  # Coeficientes del modelo de arriendo

# Calcular la covarianza entre las dos estimaciones
cov_boot <- cov(beta_boot, alpha_boot)

# Mostrar el resultado
print(cov_boot)

