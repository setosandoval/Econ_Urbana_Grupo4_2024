# ========================================================================== #
# Taller 3                                                                   #
# Economía Urbana - 202420                                                   #
#                                                                            #
# Script: Punto 3                                                            #
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
  fixest,     # Estimación efectos fijos
  nominatimlite,
  modelsummary)


# BASES DE DATOS =============================================================

## Base viviendas en venta / alquiler

# Leer y convertir los datos de propiedades a objeto espacial (sf)
data <- st_as_sf(readRDS("stores/dataTaller2.Rds"), coords = c("lon", "lat"), crs = 4326)

# Filtrar solo para propiedades de vivienda y con precio positivo en millones
data <- data %>%
  filter(grepl("(?i)(apartamento|casa|apartaestudio|habitación|cuarto)", title, perl = TRUE) &  # Incluir solo viviendas
        !grepl("(?i)(finca|local|lote)", title, perl = TRUE) &  # Excluir fincas, locales y lotes
         price > 0) %>% 
         mutate(price_m = price / 1000000,
                log_price = log(price))


## UPZ

# Leer el shapefile UPZ
UPZ <- st_read("stores/upz-bogota/upz-bogota.shp")  %>%
  st_transform(crs = 4326)  %>%
  select(codigo_upz, geometry)

# Datos propiedades se le asigna su UPZ
data <- st_join(data, UPZ, join = st_nearest_feature)


## OMS data 

# Datos Bogotá
nominatim_polygon <- nominatimlite::geo_lite_sf(address = "Bogota, Colombia", points_only = FALSE)
bog_bbox <- sf::st_bbox(nominatim_polygon)

# Parques 
parques<- opq(bbox = bog_bbox) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
                              
# Plazas
plazas<- opq(bbox = bog_bbox) %>%
  add_osm_feature(key = "place" , value = "square") 
plazas_sf <- osmdata_sf(plazas)

# Incluir tanto polígonos como multipolígonos de parques y plazas por separado
sf_polygons_parques <- st_set_crs(parques_sf$osm_polygons, 4326) %>%
  st_transform(crs = 4326) %>%
  select(osm_id, name) 
sf_multipolygons_parques <- st_set_crs(parques_sf$osm_multipolygons, 4326) %>%
  st_transform(crs = 4326) %>%
  select(osm_id, name) 

sf_polygons_plazas <- st_set_crs(plazas_sf$osm_polygons, 4326) %>%
  st_transform(crs = 4326) %>%
  select(osm_id, name) 
sf_multipolygons_plazas <- st_set_crs(plazas_sf$osm_multipolygons, 4326) %>%
  st_transform(crs = 4326) %>%
  select(osm_id, name) 

# Convertir todos los polígonos a multipolígonos
sf_polygons_parques <- st_cast(sf_polygons_parques, "MULTIPOLYGON")
sf_polygons_plazas <- st_cast(sf_polygons_plazas, "MULTIPOLYGON")

# Combinar polígonos y multipolígonos
parques_clean <- bind_rows(sf_polygons_parques, sf_multipolygons_parques)
plazas_clean <- bind_rows(sf_polygons_plazas, sf_multipolygons_plazas)


## Distancias

# Hacer válidas las geometrías 
data <- st_make_valid(data)
parques_clean <- st_make_valid(parques_clean)
plazas_clean <- st_make_valid(plazas_clean)

# Calcular la distancia entre cada propiedad y los parques y plazas por separado
dist_parque <- st_distance(x = data, y = parques_clean)
dist_plaza <- st_distance(x = data, y = plazas_clean)

# Obtener la distancia mínima de cada propiedad al parque/plaza más cercano
dmin_parque <- apply(dist_parque, 1, min)
dmin_plaza <- apply(dist_plaza, 1, min)

# Agregar la distancia mínima al dataframe de propiedades
data$dmin_parque <- dmin_parque
data$dmin_plaza <- dmin_plaza

# Crear la variable dummy indicadora si hay parque/plaza a menos de 200 metros
data$near_200 <- ifelse(data$dmin_parque <= 200 | data$dmin_plaza <= 200, 1, 0)

# Verificar el resultado
table(data$near_200)

# Crear la variable dummy que indica si el espacio más cercano es un parque
data$park <- ifelse(data$near_200 == 1 & data$dmin_parque < data$dmin_plaza, 1, 0)

# Verificar el resultado
table(data$near_200)
table(data$park)


## Objetos diferenciados venta y alquiler

# Precio: Venta y Alquiler
data_venta <- data %>%
  filter(operation == "Venta")
data_alquiler <- data %>%
  filter(operation == "Alquiler")

# Outliers para venta y alquiler: 
Q95_v <- quantile(data_venta$price, 0.95, na.rm = TRUE)
data_venta_clean <- data_venta %>%
  filter(price <= Q95_v)

Q95_a <- quantile(data_alquiler$price, 0.95, na.rm = TRUE)
data_alquiler_clean <- data_alquiler %>%
  filter(price <= Q95_a)

# Observar datos
summary(data_alquiler_clean$price_m)
summary(data_venta_clean$price_m)
table(data_alquiler_clean$near_200)
table(data_venta_clean$near_200)
table(data_alquiler_clean$park)
table(data_venta_clean$park)



# ESTIMACIONES ===============================================================

## OLS
ols_model <- log_price ~ near_200 + near_200:park + rooms + bathrooms + surface_total + surface_covered

ols_venta <- lm(ols_model, data = data_venta_clean)
ols_alquiler <- lm(ols_model, data = data_alquiler_clean)

# OLS cluster
cluster_venta <- feols(ols_model, data = data_venta_clean, cluster = ~codigo_upz)
cluster_alquiler <- feols(ols_model, data = data_alquiler_clean, cluster = ~codigo_upz)


## Errores Conley
conley_venta <- conleyreg(ols_model,
                          data = data_venta_clean,
                          ncores=8,
                          dist_cutoff = 1,
                          crs = st_crs(4326),
                          st_distance = F)

conley_alquiler <- conleyreg(ols_model,
                          data = data_alquiler_clean,
                          ncores=8,
                          dist_cutoff = 1,
                          crs = st_crs(4326),
                          st_distance = F)

# Formato
conley_venta_tidy <- tidy(conley_venta)
conley_alquiler_tidy <- tidy(conley_alquiler)


## Diferencias Espaciales
diff_model <- diff(log_price) ~ diff(near_200) + diff(near_200*park) + diff(rooms) + diff(bathrooms) + diff(surface_total) + diff(surface_covered)

diff_venta <- lm(diff_model, data = data_venta_clean)
diff_alquiler <- lm(diff_model, data = data_alquiler_clean)


## Exportar Resultados a Latex

# Crear el mapeo de las variables con nombres más intuitivos
coef_interes <- c(
  "near_200" = "Espacio Abierto Cerca (200m)",
  "near_200:park" = "Efecto Heterogéneo Parque Cerca (200m)",
  "diff(near_200)" = "Diferencia Espacio Abierto Cerca (200m)",
  "diff(near_200 * park)" = "Diferencia Efecto Heterogéneo Parque Cerca (200m)"
)

# Especificar que solo se muestren R2 y el número de observaciones
gof_interes <- c("r.squared", "nobs")

# Generar la tabla con modelsummary sin Conley
modelsummary(
  list(ols_venta, ols_alquiler, cluster_venta, cluster_alquiler, diff_venta, diff_alquiler),
  fmt = fmt_decimal(digits = 3),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.001),
  coef_map = coef_interes,
  gof_map = gof_interes,
  output = "views/P3.tex")

# Generar la tabla con modelsummary de Conley
modelsummary(
  list(conley_venta, conley_alquiler),
  fmt = fmt_decimal(digits = 3),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.001),
  coef_map = coef_interes,
  output = "views/P3_Conley.tex")
