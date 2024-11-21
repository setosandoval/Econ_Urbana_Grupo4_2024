# ========================================================================== #
# Taller 4                                                                   #
# Economía Urbana - 202420                                                   #
#                                                                            #
# Script: Punto 2                                                            #
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
       stargazer)



# FUNCIONES ====================================================================

# Cálculo W 
calcular_w <- function(b, A) {
  w <- A ^ (1 / b) * b * ((1 - b))^((1 - b) / b)
  return(w)
}

# Cálculo parámetro escala
calcular_p_escala <- function(theta, B, W, p, a, T) {
  expresion <- B * W * p^(a-1) * (a^a) * (1 - a)^(1 - a)
  resultado <- theta * (expresion^T)
  return(resultado)
}

# Simulación de equilibrio
simular_equilibrio <- function(H, pi_0, B, A, k, a, b, z, T, theta, error = 1e-5, ajuste = 0.3) {
  # Parámetros iniciales y configuraciones
  W <- calcular_w(b, A)
  N <- pi_0 * H
  diff <- 1
  iter <- 1
  
  # Matrices para almacenar los resultados
  N_mat <- matrix(0, nrow = length(N), ncol = 0)
  p_mat <- matrix(0, nrow = length(N), ncol = 0)
  
  while (diff > error) {
    # Distribución espacial de la población y almacenar resultados
    N <- pi_0 * H
    N_mat <- cbind(N_mat, N)
    
    # Calcular la renta
    p <- z + k * N
    p_mat <- cbind(p_mat, p)
    
    # Calcular la nueva distribución de la población
    param <- calcular_p_escala(theta, B, W, p, a, T)
    pi_1 <- param / sum(param)
    
    # Verificar la convergencia
    diff <- max(abs(pi_0 - pi_1))
    
    # Actualizar la distribución de la población con ajuste
    pi_0 <- (1 - ajuste) * pi_0 + ajuste * pi_1
    
    # Incrementar el contador de iteraciones
    iter <- iter + 1
  }
  
  # Resultado final
  list(distribucion_poblacion = N, precios_vivienda = p, iteraciones = iter)
}



# SIMULACIÓN INICIAL ===========================================================

# Parámetros iniciales
H <- c(1, 1, 1)            # Tamaño de la población total
pi_0 <- c(1/3, 1/3, 1/3)   # Distribución inicial homogénea
B <- c(1, 1, 1)            # Amenidades
A <- c(1, 1, 1)            # TFP
k <- c(1, 0.5, 1.5)        # Elasticidad de la oferta de vivienda
a <- 2/3                   # Alpha
b <- 2/3                   # Beta
z <- 1                     # Costo base de la vivienda
T <- 1                     # Escala de Frechet
theta <- 0.5               # Shape de Frechet

# Función con los parámetros iniciales
resultado_inicial <- simular_equilibrio(H, pi_0, B, A, k, a, b, z, T, theta)

# Crear un data frame con los resultados
tabla_resultados <- data.frame(
  Ciudad = c("A", "B", "C"),
  W = round(calcular_w(b, A), 2),
  PH = round(resultado_inicial$precios_vivienda, 2),
  N = round(resultado_inicial$distribucion_poblacion, 2)
)

# Exportar la tabla en formato LaTeX usando stargazer
stargazer(tabla_resultados, type = "latex", summary = FALSE, 
          rownames = FALSE,
          digits = 2,
          out = "views/P2_eq1.tex")



# CONTRAFACTUALES ==============================================================

# Caso A
H <- c(1, 1, 1)            # Tamaño de la población total
pi_0 <- c(1/3, 1/3, 1/3)   # Distribución inicial homogénea
B <- c(1, 0.5, 1)          # Amenidades
A <- c(1, 1, 1)            # TFP
k <- c(1, 0.5, 1.5)        # Elasticidad de la oferta de vivienda
a <- 2/3                   # Alpha
b <- 2/3                   # Beta
z <- 1                     # Costo base de la vivienda
T <- 1                     # Escala de Frechet
theta <- 0.5               # Shape de Frechet

# Función con los parámetros iniciales
resultado_inicial <- simular_equilibrio(H, pi_0, B, A, k, a, b, z, T, theta)

# Crear un data frame con los resultados
tabla_resultados2 <- data.frame(
  Ciudad = c("A", "B", "C"),
  W = round(calcular_w(b, A), 2),
  PH = round(resultado_inicial$precios_vivienda, 2),
  N = round(resultado_inicial$distribucion_poblacion, 2)
)

# Exportar la tabla en formato LaTeX usando stargazer
stargazer(tabla_resultados2, type = "latex", summary = FALSE, 
          rownames = FALSE,
          digits = 2,
          out = "views/P2_eq2.tex")


# Caso B
H <- c(1, 1, 1)            # Tamaño de la población total
pi_0 <- c(1/3, 1/3, 1/3)   # Distribución inicial homogénea
B <- c(1, 0.5, 1)          # Amenidades
A <- c(1, 2, 1.5)          # TFP
k <- c(1, 0.5, 1.5)        # Elasticidad de la oferta de vivienda
a <- 2/3                   # Alpha
b <- 2/3                   # Beta
z <- 1                     # Costo base de la vivienda
T <- 1                     # Escala de Frechet
theta <- 0.5               # Shape de Frechet

# Función con los parámetros iniciales
resultado_inicial <- simular_equilibrio(H, pi_0, B, A, k, a, b, z, T, theta)

# Crear un data frame con los resultados
tabla_resultados3 <- data.frame(
  Ciudad = c("A", "B", "C"),
  W = round(calcular_w(b, A), 2),
  PH = round(resultado_inicial$precios_vivienda, 2),
  N = round(resultado_inicial$distribucion_poblacion, 2)
)

# Exportar la tabla en formato LaTeX usando stargazer
stargazer(tabla_resultados3, type = "latex", summary = FALSE, 
          rownames = FALSE,
          digits = 2,
          out = "views/P2_eq3.tex")


# Caso C
H <- c(1, 1, 1)            # Tamaño de la población total
pi_0 <- c(1/3, 1/3, 1/3)   # Distribución inicial homogénea
B <- c(1, 0.5, 1)          # Amenidades
A <- c(1, 2, 1.5)          # TFP
k <- c(0.5, 0.5, 1.5)      # Elasticidad de la oferta de vivienda
a <- 2/3                   # Alpha
b <- 2/3                   # Beta
z <- 1                     # Costo base de la vivienda
T <- 1                     # Escala de Frechet
theta <- 0.5               # Shape de Frechet

# Función con los parámetros iniciales
resultado_inicial <- simular_equilibrio(H, pi_0, B, A, k, a, b, z, T, theta)

# Crear un data frame con los resultados
tabla_resultados4 <- data.frame(
  Ciudad = c("A", "B", "C"),
  W = round(calcular_w(b, A), 2),
  PH = round(resultado_inicial$precios_vivienda, 2),
  N = round(resultado_inicial$distribucion_poblacion, 2)
)

# Exportar la tabla en formato LaTeX usando stargazer
stargazer(tabla_resultados4, type = "latex", summary = FALSE, 
          rownames = FALSE,
          digits = 2,
          out = "views/P2_eq4.tex")
