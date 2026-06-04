
##### LIBRERÍAS #####
library(sf)
library(dplyr)
library(readxl)
#install.packages("patchwork")
library(patchwork) 
library(ggplot2)
library(spatstat)
library(spdep)
library(rgeoda)
library(tmap)
library(RColorBrewer)


##### CARGA DE LOS DATOS ######
# Carga del censo de arbolado urbano de Valencia ciudad
ruta_arbolado <- "arbolado"
censo_arboles <- sf::st_read(ruta_arbolado, quiet = TRUE)

# Carga del archivo de renta 
ruta_renta <- "renta_hogar.xlsx" 

renta_bruta <- readxl::read_excel(
  path = ruta_renta,
  sheet = 1,                 
  skip = 8,                  
  col_names = FALSE,        
  col_types = c("text", "numeric")
)

# Nos aseguramos de quedarnos con las dos primeras columnas esenciales y les damos nombre limpio
renta_bruta <- renta_bruta[, 1:2]
names(renta_bruta) <- c("seccion_texto", "renta_medio_hogar")

#Filtrado estricto: Nos quedamos SOLO con las secciones de Valencia Ciudad
renta_valencia_ciudad <- renta_bruta %>%
  dplyr::filter(!is.na(seccion_texto)) %>%
  dplyr::filter(base::substr(seccion_texto, 1, 5) == "46250") %>%
  dplyr::mutate(
    # Limpiamos el código para quedarnos solo con el número de 11 dígitos
    codigo_seccion = base::substr(seccion_texto, 1, 11)
  ) %>%
  dplyr::select(codigo_seccion, renta_medio_hogar)


##### PREPARACIÓN GEOGRÁFICA Y UNIÓN DE DATOS SOCIOECONÓMICOS #####
# Filtramos los registros vacíos (NA) 
renta_valencia_limpia <- renta_valencia_ciudad %>%
  dplyr::filter(!is.na(renta_medio_hogar))

# Carga del Shapefile oficial del INE
ruta_ine <- "secciones_ine/España_Seccionado2023_ETRS89H30/SECC_CE_20230101.shp" 
secciones_espana <- sf::st_read(ruta_ine, quiet = TRUE)

# Filtramos el mapa de España para quedarnos solo con Valencia ciudad (46250)
secciones_valencia <- secciones_espana %>%
  dplyr::filter(CUMUN == "46250")

# Control y unificación del Sistema de Referencia de Coordenadas (CRS)
crs_arboles <- sf::st_crs(censo_arboles)
secciones_valencia <- sf::st_transform(secciones_valencia, crs = crs_arboles)

# Unión de la geometría con la renta limpia quitando el espacio en blanco intruso
secciones_economicas <- secciones_valencia %>%
  dplyr::inner_join(
    renta_valencia_limpia %>% dplyr::mutate(codigo_seccion = base::trimws(codigo_seccion)), 
    by = c("CUSEC" = "codigo_seccion")
  )


##### CÁLCULO DE ÁREAS Y DENSIDAD DEL ARBOLADO URBANO #####

# Recuento de árboles dentro de cada polígono de sección censal
secciones_con_arboles <- secciones_economicas %>%
  dplyr::mutate(
    num_arboles = base::lengths(sf::st_intersects(secciones_economicas, censo_arboles))
  )

# Cálculo de la superficie de cada sección en kilómetros cuadrados
secciones_con_arboles <- secciones_con_arboles %>%
  dplyr::mutate(
    superficie_m2 = sf::st_area(.),
    superficie_km2 = base::as.numeric(superficie_m2) / 1000000
  )

# Cálculo de la variable final: Densidad de arbolado por km2
secciones_finales <- secciones_con_arboles %>%
  dplyr::mutate(
    densidad_arboles_km2 = num_arboles / superficie_km2
  ) %>%
  dplyr::select(CUSEC, renta_medio_hogar, num_arboles, superficie_km2, densidad_arboles_km2)


##### ANÁLISIS DESCRIPTIVO ESPACIAL #####

# Configuración de etiquetas
formato_es <- scales::label_comma(big.mark = ".", decimal.mark = ",")

# Mapa A: Renta Media por Hogar
mapa_renta <- ggplot(data = secciones_finales) +
  geom_sf(aes(fill = renta_medio_hogar), color = "white", size = 0.005) +
  scale_fill_viridis_c(
    option = "viridis", 
    name = "Renta (€)", 
    labels = formato_es
  ) +
  labs(
    title = "Renta Media por Hogar (2023)",
    subtitle = "Secciones censales de Valencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Mapa B: Densidad de Arbolado Viario
mapa_verde <- ggplot(data = secciones_finales) +
  geom_sf(aes(fill = densidad_arboles_km2), color = "white", size = 0.005) +
  scale_fill_viridis_c(
    option = "viridis",          
    name = "Árboles / km²",
    labels = formato_es,
    limits = c(0, 400),          
    oob = scales::squish         
  ) +
  labs(
    title = "Densidad de Arbolado Viario",
    subtitle = "Estandarizado por km²"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Combinamos ambos mapas dejando espacio suficiente
mapa_renta + mapa_verde


##### ANÁLISIS DE PROCESOS PUNTUALES ##### 

# Preparación geométrica y paso a formato ppp 
valencia_proyectada <- sf::st_transform(secciones_economicas, crs = 25830)
censo_arboles_proyectado <- sf::st_transform(censo_arboles, crs = 25830)

valencia_contorno <- sf::st_union(valencia_proyectada)
valencia_owin <- as.owin(sf::st_geometry(valencia_contorno))

coordenadas <- sf::st_coordinates(censo_arboles_proyectado)
arboles_ppp_completo <- ppp(x = coordenadas[, 1], y = coordenadas[, 2], window = valencia_owin, check = FALSE)

# Fijamos una semilla para que el resultado sea reproducible al compilar
set.seed(12345) 
n_muestra <- base::round(arboles_ppp_completo$n * 0.10)
arboles_ppp <- arboles_ppp_completo[base::sample(1:arboles_ppp_completo$n, n_muestra)]

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Función G (Vecino más cercano)
env_g <- envelope(arboles_ppp, fun = Gest, nsim = 19, quiet = TRUE)
plot(env_g, main = "Función G (Vecino más cercano)", xlab = "Distancia r (m)", ylab = "G(r)", las = 1)

# Función K de Ripley multiescala
env_k <- envelope(arboles_ppp, fun = Kest, nsim = 19, quiet = TRUE, verbose=FALSE)
plot(env_k, main = "Función K de Ripley", xlab = "Distancia r (m)", ylab = "K(r)", las = 1)


##### FUNCIONES AUXILIARES PARA LA CARTOGRAFÍA LISA #####
match_palette <- function(patterns, classifications, colors){
  classes_present <- base::unique(patterns)
  mat <- matrix(c(classifications,colors), ncol = 2)
  logi <- classifications %in% classes_present
  pre_col <- matrix(mat[logi], ncol = 2)
  pal <- pre_col[,2]
  return(pal)
}

lisa_map <- function(df, lisa, alpha = .05) {
  clusters <- lisa_clusters(lisa,cutoff = alpha)
  labels <- lisa_labels(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]
  
  pal <- match_palette(lisa_patterns,labels,colors)
  labels <- labels[labels %in% lisa_patterns]
  
  df["lisa_clusters"] <- clusters
  tm_shape(df) +
    tm_fill("lisa_clusters", labels = labels, palette = pal, style = "cat")
}


##### AUTOCORRELACIÓN ESPACIAL global Y TABLA DE RESULTADOS ##### 

vecinos_queen <- spdep::poly2nb(secciones_finales, queen = TRUE)
pesos_lista   <- spdep::nb2listw(vecinos_queen, style = "B")

test_moran_renta <- spdep::moran.test(secciones_finales$renta_medio_hogar, listw = pesos_lista)
test_moran_verde <- spdep::moran.test(secciones_finales$densidad_arboles_km2, listw = pesos_lista)

# GENERACIÓN DE LA TABLA 
datos_tabla <- data.frame(
  Variable = c("Renta Media por Hogar", "Densidad de Arbolado Viario"),
  Indice   = c(test_moran_renta$estimate[1], test_moran_verde$estimate[1]),
  Esperanza = c(test_moran_renta$estimate[2], test_moran_verde$estimate[2]),
  Varianza  = c(test_moran_renta$estimate[3], test_moran_verde$estimate[3]),
  Z         = c(test_moran_renta$statistic, test_moran_verde$statistic),
  P_valor   = c("< 2.2e-16", "< 2.2e-16")
)

# Renderizado simple en formato markdown. 
knitr::kable(
  datos_tabla, 
  format = "markdown", 
  digits = 5,
  col.names = c("Variable Analizada", "Índice I de Moran", "Esperanza (E[I])", "Varianza (Var[I])", "Desviación Estándar (Z)", "p-valor"),
  align = c("l", "c", "c", "c", "c", "c")
)


##### AUTOCORRELACIÓN ESPACIAL LOCAL #####
# Matriz de pesos y cálculo de indicadores LISA con rgeoda
pesos_geoda <- rgeoda::queen_weights(secciones_finales)

lisa_renta <- rgeoda::local_moran(pesos_geoda, secciones_finales['renta_medio_hogar'])
lisa_verde <- rgeoda::local_moran(pesos_geoda, secciones_finales['densidad_arboles_km2'])

# CARTOGRAFÍA: Definimos los mapas individuales
mapa_lisa_renta <- lisa_map(secciones_finales, lisa_renta) +
  tmap::tm_layout(title = "LISA: Renta Media", legend.outside = TRUE)

mapa_lisa_verde <- lisa_map(secciones_finales, lisa_verde) +
  tmap::tm_layout(title = "LISA: Densidad Arbolado", legend.outside = TRUE)

tmap::tmap_arrange(mapa_lisa_renta, mapa_lisa_verde, ncol = 2)