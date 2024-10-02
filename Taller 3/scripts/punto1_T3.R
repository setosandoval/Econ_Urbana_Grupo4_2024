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
  sfd,        # Spatial First Difference 
  conleyreg,  # Errores conley
  fixest      # Estimación efectos fijos
)



# BASES DE DATOS =============================================================

# Base de datos
df <- readRDS("stores/db_ejercicio1.Rds")
db <- as_tibble(df)

# Transforma el año de venta a factor con etiquetas descriptivas
db <- db  %>% mutate(years = factor(year,levels=c(2000,2001,2002,2003,2004,2005,2006,2007,
                                                 2008,2009,2010,2011,2012,2013,2014,2015,
                                                 2016,2017,2018,2019,2020),
                                         labels=c("d2000","d2001","d2002","d2003","d2004",
                                                  "d2005","d2006","d2007","d2008","d2009",
                                                  "d2010","d2011","d2012","d2013","d2014",
                                                  "d2015","d2016","d2017","d2018","d2019",
                                                  "d2020")))

# Añade el logaritmo del precio de venta
db <- db  %>% mutate(log_price= log(sale_price))

# Creación de indicadores binarios para cada año
db <- db  %>% mutate(d2000=ifelse(year==2000,1,0),
                     d2001=ifelse(year==2001,1,0),
                     d2002=ifelse(year==2002,1,0),
                     d2003=ifelse(year==2003,1,0),
                     d2004=ifelse(year==2004,1,0),
                     d2005=ifelse(year==2005,1,0),
                     d2006=ifelse(year==2006,1,0),
                     d2007=ifelse(year==2007,1,0),
                     d2008=ifelse(year==2008,1,0),
                     d2009=ifelse(year==2009,1,0),
                     d2010=ifelse(year==2010,1,0),
                     d2011=ifelse(year==2011,1,0),
                     d2012=ifelse(year==2012,1,0),
                     d2013=ifelse(year==2013,1,0),
                     d2014=ifelse(year==2014,1,0),
                     d2015=ifelse(year==2015,1,0),
                     d2016=ifelse(year==2016,1,0),
                     d2017=ifelse(year==2017,1,0),
                     d2018=ifelse(year==2018,1,0),
                     d2019=ifelse(year==2019,1,0),
                     d2020=ifelse(year==2020,1,0))



# INDICE HEDÓNICO ===========================================================

# Estimación índice hedónico
model_hed <-lm(log_price ~ d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010+
                                d2011+d2012+d2013+d2014+d2015+d2016+d2017+d2018+d2019+d2020+
                                year_built + building_sqft + land_sqft + num_bedrooms + num_rooms + 
                                num_full_baths + num_half_baths + num_fireplaces + type_of_residence + 
                                construction_quality + garage_attached + garage_size + central_heating +
                                recent_renovation + central_air, data = db)
               
# Resumen de los términos estadísticos del modelo hedónico
hed <- broom::tidy(model_hed, conf.int = TRUE)

# Filtra términos que comienzan con 'd' seguido de un dígito
hed <- hed  %>% filter(grepl("d\\d", term))

# Años para gráfico
hed$years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
              2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

# Agregar columna para el índice hedónico
hed <- hed %>%
  mutate(hed_index = exp(estimate)*100,
         lower = exp(conf.low)*100,
         upper = exp(conf.high)*100)


# IPVU =======================================================================

## Primera Etapa

# Crea una variable auxiliar y calcula el número de veces que cada casa fue vendida
db <- db %>% 
  group_by(pin) %>% 
  mutate(one_aux = 1) %>%
  mutate(times_sold = cumsum(one_aux), .groups = "drop") %>% 
  select(-one_aux)

# Filtra las casas vendidas más de una vez
db <- db %>% 
  mutate(houses_sold_multiple_times = ifelse(max(times_sold) > 1, 1, 0)) %>% 
  filter(houses_sold_multiple_times == 1)

# Prepara datos para análisis de ventas repetidas
prices_house_sales <- db %>% 
  group_by(pin) %>%
  mutate(price0 = lag(sale_price)) %>%
  ungroup() %>%
  filter(!is.na(price0)) %>%
  rename(price1 = sale_price) %>%
  select(price1, price0)

times_house_sales <- db %>% 
  group_by(pin) %>%
  mutate(time0 = lag(year)) %>%
  ungroup() %>%
  filter(!is.na(time0)) %>%
  rename(time1 = year) %>%
  select(time1, time0)

# Combina los datos de precios y tiempos de venta
rep_sales <- cbind(prices_house_sales, times_house_sales)

# Cálculos adicionales para análisis
price1 <- log(rep_sales$price1)
price0 <- log(rep_sales$price0)
time1 <- rep_sales$time1
time0 <- rep_sales$time0

dv <- price1 - price0
timevar <- levels(factor(c(time0, time1)))

nt = length(timevar)
n = length(dv)
xmat <- array(0, dim = c(n, nt - 1))

# Construye matriz para el modelo
for (j in seq(2, nt)) {
  xmat[, j - 1] <- ifelse(time1 == timevar[j], 1, xmat[, j - 1])
  xmat[, j - 1] <- ifelse(time0 == timevar[j], -1, xmat[, j - 1])
}

xmat

# Estima el modelo con variables de tiempo
fit <- lm(dv ~ xmat + 0) 
summary(fit)


## Segunda Etapa
e <- residuals(fit)
xvar <- time1 - time0

# Estima pesos para el modelo
fit <- lm(e^2 ~ xvar)
wgt <- fitted(fit)
samp <- wgt > 0
wgt <- ifelse(samp == TRUE, 1 / wgt, 0)


## Tercera Etapa
fit <- lm(dv ~ xmat - 1, weights = wgt)

# Resumen de los términos estadísticos del modelo IPVU
IPVU <- broom::tidy(fit, conf.int =TRUE)

# Años para gráfico
IPVU$years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
               2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

# Agregar columna para el índice hedónico
IPVU <- IPVU %>%
  mutate(IPVU_index = exp(estimate)*100,
         lower = exp(conf.low)*100,
         upper = exp(conf.high)*100)



# EFECTOS FIJOS ==============================================================
model_fe <- feols(log(sale_price) ~ factor(year) +
                                year_built + building_sqft + land_sqft + num_bedrooms + num_rooms + 
                                num_full_baths + num_half_baths + num_fireplaces + type_of_residence + 
                                construction_quality + garage_attached + garage_size + central_heating +
                                recent_renovation + central_air | pin, data = db, vcov = ~pin)

# Resumen de los términos estadísticos del modelo hedónico
fe <- broom::tidy(model_fe, conf.int = TRUE)

# Filtra términos que comienzan con 'factor(year)' seguido de un dígito
fe <- fe %>% filter(grepl("factor\\(year\\)\\d+", term))

# Años para gráfico
fe$years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
               2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

# Agregar columna para el índice hedónico
fe <- fe %>%
  mutate(fe_index = exp(estimate)*100,
         lower = exp(conf.low)*100,
         upper = exp(conf.high)*100)



# GRÁFICO ====================================================================

# Gráfico coeficientes con intervalo confianza
plot1 <- ggplot() + 
  # Indice Hedónico
  geom_point(data = hed, aes(x = years, y = estimate), color = "red", alpha = 0.5) +
  geom_line(data = hed, aes(x = years, y = estimate, color = "Índice Hedónico"), show.legend = TRUE) + 
  geom_errorbar(data = hed, aes(x = years, y = estimate, ymin = conf.low, ymax = conf.high), color = "red", width = 0.2) +
  # IPVU
  geom_point(data = IPVU, aes(x = years, y = estimate), color = "blue", alpha = 0.5) +
  geom_line(data = IPVU, aes(x = years, y = estimate, color = "IPVU"), show.legend = TRUE) + 
  geom_errorbar(data = IPVU, aes(x = years, y = estimate, ymin = conf.low, ymax = conf.high), color = "blue", width = 0.2)+
  # FE
  geom_point(data = fe, aes(x = years, y = estimate), color = "darkgreen", alpha = 0.5) +
  geom_line(data = fe, aes(x = years, y = estimate, color = "Efectos Fijos"), show.legend = TRUE) + 
  geom_errorbar(data = fe, aes(x = years, y = estimate, ymin = conf.low, ymax = conf.high), color = "darkgreen", width = 0.2)+
  # Gráfico
  scale_x_continuous(breaks = seq(min(hed$years), max(hed$years), by = 1)) + 
  scale_y_continuous(breaks = seq(min(-0.1), max(0.6), by = 0.1)) +
  labs(x = "Años", y = "Coeficiente", color = "Modelo") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_color_manual(values = c("Índice Hedónico" = "lightpink", "IPVU" = "lightblue", "Efectos Fijos" = "lightgreen"))
ggsave("views/P1_coef.pdf", plot1)

# Gráfico índices
plot2 <- ggplot() + 
  # Indice Hedónico
  geom_line(data = hed, aes(x = years, y = hed_index, color = "Índice Hedónico"), show.legend = TRUE) + 
  geom_ribbon(data = hed, aes(x = years, ymin = lower, ymax = upper), alpha = 0.4, fill = "lightpink") +
  # IPVU
  geom_line(data = IPVU, aes(x = years, y = IPVU_index, color = "IPVU"), show.legend = TRUE) + 
  geom_ribbon(data = IPVU, aes(x = years, ymin = lower, ymax = upper), alpha = 0.4, fill = "lightblue") +
  # FE
  geom_line(data = fe, aes(x = years, y = fe_index, color = "Efectos Fijos"), show.legend = TRUE) + 
  geom_ribbon(data = fe, aes(x = years, ymin = lower, ymax = upper), alpha = 0.4, fill = "lightgreen") +
  # Gráfico
  scale_x_continuous(breaks = seq(min(hed$years), max(hed$years), by = 1)) + 
  #scale_y_continuous(breaks = seq(min(-0.1), max(0.6), by = 0.1)) +
  labs(x = "Años", y = "Índice", color = "Modelo") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_color_manual(values = c("Índice Hedónico" = "red", "IPVU" = "blue", "Efectos Fijos" = "green"))
ggsave("views/P1_ind.pdf", plot2)
