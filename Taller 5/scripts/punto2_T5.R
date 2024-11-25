# ========================================================================== #
# Taller 5                                                                   #
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
setwd("/Users/setosandoval/Desktop/Urbana/Repositorio/Econ_Urbana_Grupo4_2024/Taller 5")

# Paquetes
library(pacman)
p_load(tidyverse, 
       ggplot2, 
       dplyr,
       DeclareDesign,
       fabricatr,
       estimatr)

#Semilla
set.seed(202028276)



#  MODELO ======================================================================

model <- declare_model(
  # Población
  N = 2500,
  # Controles
  rooms_raw = rnorm(N, mean = 2.5, sd = 1),
  rooms = pmax(pmin(round(rooms_raw), 10), 1), 
  area_raw = rnorm(N, mean = 75, sd = 25),
  area = pmax(pmin(area_raw, 180), 25),
  rent_raw = rnorm(N, mean = 12000, sd = 5000),
  rent = pmax(pmin(rent_raw, 25000), 1000),
  rent_1000 = rent/1000,
  company = rbinom(N, size = 1, prob = 0.096),
  metrop = rbinom(N, size = 1, prob = 0.448),
  female = rbinom(N, size = 1, prob = 0.364),
  immigrant = rbinom(N, size = 1, prob = 0.078),
  # Error idiosicrático
  u = rnorm(N, mean = 0, sd = 1)
)

# Resultados potenciales
potential_outcomes <- declare_potential_outcomes(
  formula = Y ~ pnorm(0.41 + 0.126 * (Z == 1) - 0.261 * (Z == 2) 
                      + 0.007 * rooms + 0.001 * area + 0.002 * rent_1000 
                      + 0.550 * company - 0.251 * metrop 
                      - 0.023 * female + 0.227 * immigrant + u),
  conditions = c(0, 1, 2),
  assignment_variables = "Z"
)
  


#  INQUIRY =====================================================================
  
# ATEs 
estimand <- declare_inquiries(
  ate_Y_1 = mean(Y_Z_1 - Y_Z_0),
  ate_Y_2 = mean(Y_Z_2 - Y_Z_0)
)

#  ESTRATEGIA DATOS ============================================================
  
# Muestra 500
sampling <- declare_sampling(
  S = complete_rs(N = 2500, n = 500)
)

# Asignación de tratamiento
assignment <- declare_assignment(
  Z = complete_ra(N = N, num_arms = 3, conditions = c(0,1,2))
)

# Revelación de resultados
reveal_Y <- declare_reveal(
  outcome_variables = "Y",
  assignment_variables = "Z"
)



#  ESTRATEGIA RESPUESTA ========================================================



estimator_1 <- declare_estimator(
  Y ~ Z,
  .method = glm,
  family = binomial("probit"),
  inquiry = "ATE_Erik_Mohammed",
  term = "Z",
  label = "ATE: Erik vs Mohammed"
)


# Estimadores ATE
estimator <- declare_estimator(handler = function(data) {
  estimates <- rbind.data.frame(
    ate_Y_2 = difference_in_means(formula = Y ~ Z, data = data,
                                  condition1 = 0, condition2 = 2),
    ate_Y_1 = difference_in_means(formula = Y ~ Z, data = data,
                                  condition1 = 0, condition2 = 1)
  )
  
  # Renombrar la columna N
  names(estimates)[names(estimates) == "N"] <- "N_DIM"
  
  # Añadir columnas adicionales
  estimates$estimator <- c("DIM(Z_2 - Z_0)", "DIM(Z_1 - Z_0)")
  estimates$inquiry <- rownames(estimates)
  estimates$estimate <- estimates$coefficients
  estimates$term <- NULL
  
  return(estimates)
})



#  DIAGNÓSITCO DISEÑO ==========================================================
design <- model +
  sampling +
  potential_outcomes +
  assignment +
  reveal_Y +
  estimand +
  estimator 

# Evaluamos el diseño utilizando simulaciones.
diagnosis <- diagnose_design(design, sims = 100)

# Visualización de los resultados del diagnóstico
# Convertimos los resultados del diagnóstico en un data frame para visualizarlos.
diagnosis_results <- as.data.frame(diagnosis$diagnosands_df)
View(diagnosis_results)



#  HETEROGENEIDAD LANDLORD =====================================================

# Cambian resultados potenciales
potential_outcomes_2 <- declare_potential_outcomes(
  formula = Y ~ pnorm(0.41 + 0.148 * (Z == 1) - 0.213 * (Z == 2) 
                      + 0.007 * rooms + 0.001 * area + 0.014 * rent_1000 
                      + 0.550 * company - 0.304 * metrop 
                      - 0.048 * female + 0.238 * immigrant 
                      + 0.160 * (Z == 2) * metrop 
                      + 0.059 * (Z == 2) * female 
                      - 0.076 * (Z == 2) * immigrant 
                      - 0.031 * (Z == 2) * rent_1000 
                      + 0.050 * (Z == 1) * metrop 
                      + 0.026 * (Z == 1) * female 
                      + 0.036 * (Z == 1) * immigrant 
                      - 0.013 * (Z == 1) * rent_1000 
                      + u),
  conditions = c(0, 1, 2),
  assignment_variables = "Z"
)



# Cambian Inquiry por CATEs
estimand_2 <- declare_inquiries(
  ate_Y_1 = mean(Y_Z_1 - Y_Z_0),
  ate_Y_2 = mean(Y_Z_2 - Y_Z_0),
  cate_Y_1_immigrant = mean(Y_Z_1[immigrant == 1] - Y_Z_0[immigrant == 1]),
  cate_Y_1_non_immigrant = mean(Y_Z_1[immigrant == 0] - Y_Z_0[immigrant == 0]),
  cate_Y_2_immigrant = mean(Y_Z_2[immigrant == 1] - Y_Z_0[immigrant == 1]),
  cate_Y_2_non_immigrant = mean(Y_Z_2[immigrant == 0] - Y_Z_0[immigrant == 0])
)

# Cambian estimador para ATEs y CATEs
estimator_2 <- declare_estimator(handler = function(data) {
  # ATEs
  ate_estimates <- rbind.data.frame(
    ate_Y_2 = difference_in_means(formula = Y ~ Z, data = data,
                                  condition1 = 0, condition2 = 2),
    ate_Y_1 = difference_in_means(formula = Y ~ Z, data = data,
                                  condition1 = 0, condition2 = 1)
  )
  
  # CATEs para inmigrantes
  cate_Y_2_immigrant <- difference_in_means(
    formula = Y ~ Z, data = subset(data, immigrant == 1),
    condition1 = 0, condition2 = 2
  )
  cate_Y_1_immigrant <- difference_in_means(
    formula = Y ~ Z, data = subset(data, immigrant == 1),
    condition1 = 0, condition2 = 1
  )
  
  # CATEs para no inmigrantes
  cate_Y_2_non_immigrant <- difference_in_means(
    formula = Y ~ Z, data = subset(data, immigrant == 0),
    condition1 = 0, condition2 = 2
  )
  cate_Y_1_non_immigrant <- difference_in_means(
    formula = Y ~ Z, data = subset(data, immigrant == 0),
    condition1 = 0, condition2 = 1
  )
  
  # Combinar todos los estimadores en un único data.frame
  estimates <- rbind.data.frame(
    ate_estimates,
    cate_Y_2_immigrant = cate_Y_2_immigrant,
    cate_Y_1_immigrant = cate_Y_1_immigrant,
    cate_Y_2_non_immigrant = cate_Y_2_non_immigrant,
    cate_Y_1_non_immigrant = cate_Y_1_non_immigrant
  )
  
  # Renombrar la columna N
  names(estimates)[names(estimates) == "N"] <- "N_DIM"
  
  # Añadir columnas adicionales
  estimates$estimator <- c(
    "DIM(Z_2 - Z_0)", "DIM(Z_1 - Z_0)",
    "CATE(Z_2 - Z_0 | immigrant = 1)", "CATE(Z_1 - Z_0 | immigrant = 1)",
    "CATE(Z_2 - Z_0 | immigrant = 0)", "CATE(Z_1 - Z_0 | immigrant = 0)"
  )
  estimates$inquiry <- rownames(estimates)
  estimates$estimate <- estimates$coefficients
  estimates$term <- NULL
  
  return(estimates)
})

# Diseño con ATEs y CATEs
design_2 <- model +
  sampling +
  potential_outcomes_2 +
  assignment +
  reveal_Y +
  estimand_2 +
  estimator_2 

# Diagnóstico del diseño con simulaciones
diagnosis_2 <- diagnose_design(design_2, sims = 100)

# Visualización de resultados
diagnosis_results_2 <- as.data.frame(diagnosis_2$diagnosands_df)
View(diagnosis_results_2)



