# ============================================================================ #
# Final Project                                                                #
# Urban Economics - 202420                                                     #
#                                                                              #
# Script                                                                       #
#                                                                              #
# Group 4: - Sergio Sandoval                                                   #
#          - María Fernanda Blanco                                             #
# ============================================================================ #


# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/sergiosandovalcamargo/Documents/Urbana_Repo/Final Project")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Load necessary libraries using pacman
library(pacman)
p_load(tidyverse,
       DeclareDesign, 
       sf,
       fabricatr,
       estimatr,
       ggplot2,
       knitr,
       stargazer, 
       cowplot, 
       survey)    



# ==============================================================================
# INCOME OUTCOME  ==============================================================
# ============================================================================== 

# DATA =========================================================================

# Census data 2018
data <-read_rds("stores/20230620EM2021.rds")
data2 <-read_rds("stores/20230220_Variables_adicionales_em2021 3.rds")

data_clean <- data %>%
  select(DIRECTORIO_PER, COD_LOCALIDAD, FEX_C) %>%
  rename(directorio_per = DIRECTORIO_PER) 

data_merge <- merge(data_clean, data2, by = "directorio_per")

data_merge <-  data_merge %>% filter(COD_LOCALIDAD == 19)

data_merge2 <-  data_merge %>% filter(N_deficit_cualitativo == 1)

data_merge3 <- data_merge2 %>%
  group_by(directorio_hog) %>%
  summarise(N_ingpcugarr = first(N_ingpcugarr),
            FEX_C = first(FEX_C))
data_merge3$constant <- 1  
design <- svydesign(ids = ~1, data = data_merge3, weights = ~FEX_C)

# Income values
cbind(
  "wMEAN" = svymean(~N_ingpcugarr, design),
  "wSD"   = sqrt(svyvar(~N_ingpcugarr, design)[1]),
  "wN"    = svytotal(~constant, design),
  confint(svymean(~N_ingpcugarr, design), df=degf(design)))



# MODEL ========================================================================

# Parameters for simulation
N_clusters <- 112         # Number of barrios (clusters)
N_i_in_cluster <- 80      # Number of households per barrio
icc <- 0.1                # Intra-cluster correlation (ICC)
ate <- 0.1                # ATE %
p_treated <- 0.5          # Treated
p_compliance <- 0.9       # Compliance %

# Define mean and standard deviation for income
income_mean <- 318640
income_sd <- 178172

log_mean <- log(income_mean^2 / sqrt(income_sd^2 + income_mean^2)) # Mean in log scale
log_sd <- sqrt(log(1 + (income_sd^2 / income_mean^2)))             # SD in log scale

# Generate population with intra-cluster correlation using DeclareDesign
fixed_pop <- declare_population(
  barrio = add_level(N = N_clusters),  # Barrios (clusters)
  household = add_level(
    N = N_i_in_cluster,
    latent_income = draw_normal_icc(
      mean = log_mean,                         
      sd = log_sd,                       
      N = N,
      clusters = barrio,
      ICC = icc
    )
  )
)()

# Declare population for analysis
population <- declare_population(data = fixed_pop)

# Declare potential outcomes based on the treatment
potential_outcomes <- declare_potential_outcomes(
  Y ~ latent_income + log(1+ate) * X,   
  assignment_variables = "X"
)



# INQUIRY  =====================================================================

# Declare the inquiry (estimand)
estimand <- declare_inquiry(
  ate = mean(Y_X_1 - Y_X_0),  # Average Treatment Effect (ATE)
  label = "ATE"
)



# DATA STRATEGY ================================================================

# Define  assignment with randomization
assignment <- declare_assignment(
  Z = conduct_ra(N = N, clusters = barrio, prob = p_treated) 
)

# Treatment
treatment <- declare_step(
  handler = fabricate,
  X = ifelse(Z == 1, rbinom(N, 1, prob = p_compliance), 0)
)

# Reveal outcomes based on treatment assignment
reveal_Y <- declare_reveal(
  outcome_variables = "Y",
  assignment_variables = "X"
)



# ANSWER STRATEGY ==============================================================

# Define estimator using clustered standard errors
clustered_estimator <- declare_estimator(
  Y ~ X,
  model = lm_robust,
  clusters = barrio,  # Adjust for clustering by barrio
  inquiry = estimand,
  label = "OLS: Clustered Standard Errors"
)

iv_clustered_estimator <- declare_estimator(
  Y ~ X | Z,
  model = iv_robust,
  clusters = barrio,  # Adjust for clustering by barrio
  inquiry = estimand,
  label = "IV: Clustered Standard Errors"
)



# DESIGN  ======================================================================

# Combine all components into a single design
design1 <- population + 
  potential_outcomes + 
  assignment +
  treatment +
  reveal_Y + 
  estimand + 
  clustered_estimator 

design1_iv <- population + 
  potential_outcomes + 
  assignment +
  treatment +
  reveal_Y + 
  estimand + 
  iv_clustered_estimator

# Diagnose or inspect the design
diagnosis1 <- diagnose_design(design1, sims = 100)
diagnosis1_iv <- diagnose_design(design1_iv, sims = 100)
print(diagnosis1)
print(diagnosis1_iv)

# Export
diagnosis_1 <- diagnosis1$diagnosands_df
diagnosis_1_iv <- diagnosis1_iv$diagnosands_df
diagnosis_1_comb <- rbind(diagnosis_1,diagnosis_1_iv)
stargazer(diagnosis_1_comb, summary = FALSE, type = "latex", out = "views/diagnosis_1.tex")


# REDESIGN  ====================================================================

# Statistical Power Across Different ATE and *parameters*

# ATE vs N in cluster (Barrios)
change_the_parameters <- 
  design1 %>% 
  redesign(N_i_in_cluster = list(60, 80, 100), 
           ate = list(0.05, 0.1, 0.15)) %>% 
  diagnose_design(sims = 100) 

change_the_parameters %>% 
  get_diagnosands() %>% 
  kable()

# Plot
g_1 <- get_diagnosands(change_the_parameters) %>%
  mutate(N_i_in_cluster = factor(N_i_in_cluster),
         ate = factor(ate * 100)) %>%
  ggplot(aes(x = N_i_in_cluster, y = power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = power - 1.96 * `se(power)`,
                    ymax = power + 1.96 * `se(power)`),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Cluster Sample Size",
       x = "Cluster Size (N)",
       y = "Statistical Power",
       fill = "ATE (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# ATE vs ICC
change_the_parameters <- 
  design1 %>% 
  redesign(icc = list(0.05, 0.1, 0.15), 
           ate = list(0.05, 0.1, 0.15)) %>% 
  diagnose_design(sims = 100) 

change_the_parameters %>% 
  get_diagnosands() %>% 
  kable()

# Plot
g_2 <- get_diagnosands(change_the_parameters) %>%
  mutate(icc = factor(icc * 100),
         ate = factor(ate * 100)) %>%
  ggplot(aes(x = icc, y = power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = power - 1.96 * `se(power)`,
                    ymax = power + 1.96 * `se(power)`),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Intra-cluster correlation (ICC)",
       x = "ICC (%)",
       y = "Statistical Power",
       fill = "ATE (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# ATE vs Treat Rate
change_the_parameters <- 
  design1 %>% 
  redesign(p_treated = list(0.4, 0.5, 0.6), 
           ate = list(0.05, 0.1, 0.15)) %>% 
  diagnose_design(sims = 100) 

change_the_parameters %>% 
  get_diagnosands() %>% 
  kable()

# Plot
g_3 <- get_diagnosands(change_the_parameters) %>%
  mutate(p_treated = factor(p_treated * 100),
         ate = factor(ate * 100)) %>%
  ggplot(aes(x = p_treated, y = power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = power - 1.96 * `se(power)`,
                    ymax = power + 1.96 * `se(power)`),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Treat Rate",
       x = "Treat Rate (%)",
       y = "Statistical Power",
       fill = "ATE (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )


# ATE vs Compliance Rate
change_the_parameters <- 
  design1 %>% 
  redesign(p_compliance = list(0.85, 0.9, 0.95), 
           ate = list(0.05, 0.1, 0.15)) %>% 
  diagnose_design(sims = 100) 

change_the_parameters %>% 
  get_diagnosands() %>% 
  kable()

# Plot
g_4 <- get_diagnosands(change_the_parameters) %>%
  mutate(p_compliance = factor(p_compliance * 100),
         ate = factor(ate * 100)) %>%
  ggplot(aes(x = p_compliance, y = power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = power - 1.96 * `se(power)`,
                    ymax = power + 1.96 * `se(power)`),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Compliance Rate",
       x = "Compliance Rate (%)",
       y = "Statistical Power",
       fill = "ATE (%)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


# Combined Figure
figure_1 <- plot_grid(g_1, g_2, ncol = 2, align = "h")
figure_2 <- plot_grid(g_3, g_4, ncol = 2, align = "h")
figure_3 <- plot_grid(figure_1, figure_2, ncol = 1, rel_heights = c(1, 1))

# Save
ggsave("views/f1_redesign1.pdf", figure_3, width = 20, height = 15, dpi = 300)



# ==============================================================================
# SCHOOL ATTENDANCE OUTCOME ====================================================
# ============================================================================== 

# MODEL ========================================================================

# Parameters for simulation
ate <- -28           

# Define mean and standard deviation for income
ab_mean <- 35.5
ab_sd <- 27.1

# Generate population with intra-cluster correlation using DeclareDesign
fixed_pop2 <- declare_population(
  barrio = add_level(N = N_clusters),  # Barrios (clusters)
  household = add_level(
    N = N_i_in_cluster,
    latent_ab = draw_normal_icc(
      mean = ab_mean,                         
      sd = ab_sd,                       
      N = N,
      clusters = barrio,
      ICC = icc
    )
  )
)()

# Declare population for analysis
population2 <- declare_population(data = fixed_pop2)

# Declare potential outcomes based on the treatment
potential_outcomes2 <- declare_potential_outcomes(
  Y ~ latent_ab + ate * X,   
  assignment_variables = "X"
)


# DESIGN  ======================================================================

# Combine all components into a single design
design2 <- population2 + 
  potential_outcomes2 + 
  assignment +
  treatment +
  reveal_Y + 
  estimand + 
  clustered_estimator 

design2_iv <- population2 + 
  potential_outcomes2 + 
  assignment +
  treatment +
  reveal_Y + 
  estimand + 
  iv_clustered_estimator

# Diagnose or inspect the design
diagnosis2 <- diagnose_design(design2, sims = 100)
diagnosis2_iv <- diagnose_design(design2_iv, sims = 100)
print(diagnosis2)
print(diagnosis2_iv)

# Export
diagnosis_2 <- diagnosis2$diagnosands_df
diagnosis_2_iv <- diagnosis2_iv$diagnosands_df
diagnosis_2_comb <- rbind(diagnosis_2,diagnosis_2_iv)
stargazer(diagnosis_2_comb, summary = FALSE, type = "latex", out = "views/diagnosis_2.tex")
