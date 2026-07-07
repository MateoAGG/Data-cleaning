
rm(list=ls())
# Mapa --------------------------------------------------------------------

setwd("C:/Users/Mateo/Desktop/Code/r")


# Packages ----------------------------------------------------------------


# Instalar y cargar los paquetes necesarios
install.packages(c("sf", "ggplot2", "dplyr"))
library(sjlabelled)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(dplyr)
library(readxl)
install.packages("ggrepel")
library(ggrepel)
install.packages("tidyr")  # Si no tienes instalada la librería
library(tidyr)
library(haven)
# Falto el SHX
Sys.setenv("SHAPE_RESTORE_SHX" = "YES")


# Data --------------------------------------------------------------------

dt <- read_excel("Base_Datos_Territorio_Final.xlsx", sheet = "Base_T")
str(dt)

# Redondear solo las columnas específicas a 2 decimales
dt <- dt %>%
  mutate_if(is.numeric, ~round(., 2))


dir()
dt <- read_spss("CPV_Poblacion_2022_Nacional_12_2_2025_CANT_GEN.sav")
cantones <- st_read("C:/Users/Mateo/Desktop/Code/r/nxcantones.shp")



names(dt)
names(cantones)

class(dt$CANTON)
class(cantones$DPA_CANTON)
get_label(dt)
get_labels(dt)

# Si estás usando haven y tienes etiquetas
dt$CANTON <- as.character(dt$CANTON)


# Suponiendo que 'cantones' es tu shapefile cargado con sf
cantones <- cantones %>%
  left_join(dt, by = c("DPA_CANTON" = "CANTON"))

cantonesf <- cantones %>%
  group_by(DPA_CANTON)

class(dt$CONDACT)
length(unique(dt$CANTON))
length(unique(cantones$DPA_CANTON))



cantones_eloro <- cantones %>% filter(str_to_lower(DPA_DESPRO) == "el oro")
cantones_eloro$TCU <- dt$`Tasa de crecimiento poblacional Urbana`
cantones_eloro$TCR<- dt$`Tasa de crecimiento poblacional Rural`
cantones_eloro$TCP <- dt$`Tasa de crecimiento Poblacional`


# Ordenar los cantones por la variable DPA_DESCAN (nombre del cantón)
cantones_eloro <- cantones_eloro %>%
  arrange(DPA_DESCAN)  # Orden alfabético por nombre de cantón


# Calcular el centroide de cada cantón (coordenadas x, y)
cantones_eloro_centroides <- cantones_eloro %>%
  st_centroid() %>%
  st_as_sf()  # Asegurarse de que los centroides sean un objeto 'sf'

# Unir los centroides con los datos de 'AGSyP'
cantones_eloro_centroides <- cantones_eloro_centroides %>%
  mutate(x = st_coordinates(.)[, 1],  # Extraer coordenadas x
         y = st_coordinates(.)[, 2])  # Extraer coordenadas y

names(cantones_eloro)
names(cantones_eloro_centroides)
#####Tasa de Urbanizacion###############


ggplot(data = cantones_eloro) +
  # Capa principal del mapa con TCU
  geom_sf(aes(fill = TCU), color = "black") +
  scale_fill_viridis_c(option = "C", name = "Tasa de Crecimiento Urbano") +
  
  # Etiquetas con líneas más visibles
  geom_label_repel(data = cantones_eloro_centroides, 
                   aes(x = x, y = y, label = paste(DPA_DESCAN, "\n", round(TCU, 2))), 
                   size = 3, 
                   color = "black", 
                   fill = "white", 
                   box.padding = 0.7, 
                   point.padding = 0.5, 
                   segment.color = "gray30",  # Línea más visible
                   segment.size = 1,         # Aumentar grosor de línea
                   fontface = "bold",
                   max.overlaps = 50) +  # Permitir más solapamientos de etiquetas
  
  # Configuración de estilo
  theme_minimal() +
  labs(title = "Tasa de Crecimiento Urbano Cantonal del El Oro",
       caption = "Fuente: INEC") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centrado y tamaño reducido del título
    plot.title.position = "plot",
    panel.grid = element_blank(),                      # Eliminar cuadrículas internas
    axis.ticks = element_blank(),                      # Eliminar marcas de los ejes
    axis.text = element_blank(),                       # Eliminar números en los ejes
    axis.title = element_blank(),                      # Eliminar títulos de los ejes
    plot.margin = margin(20, 20, 20, 20)               # Ajustar márgenes del gráfico
  ) +
  coord_sf()  # Ajustar automáticamente a la extensión de la provincia


#####Tasa de Ruralidad###########

ggplot(data = cantones_eloro) +
  # Capa principal del mapa con TCU
  geom_sf(aes(fill = TCR), color = "black") +
  scale_fill_viridis_c(option = "C", name = "Tasa de Crecimiento Rural") +
  
  # Etiquetas con líneas más visibles
  geom_label_repel(data = cantones_eloro_centroides, 
                   aes(x = x, y = y, label = paste(DPA_DESCAN, "\n", round(TCR, 2))), 
                   size = 3, 
                   color = "black", 
                   fill = "white", 
                   box.padding = 0.7, 
                   point.padding = 0.5, 
                   segment.color = "gray30",  # Línea más visible
                   segment.size = 1,         # Aumentar grosor de línea
                   fontface = "bold",
                   max.overlaps = 50) +  # Permitir más solapamientos de etiquetas
  
  # Configuración de estilo
  theme_classic() +
  labs(title = "Tasa de Crecimiento Rural Cantonal del El Oro",
       caption = "Fuente: INEC") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centrado y tamaño reducido del título
    plot.title.position = "plot",
    panel.grid = element_blank(),                      # Eliminar cuadrículas internas
    axis.ticks = element_blank(),                      # Eliminar marcas de los ejes
    axis.text = element_blank(),                       # Eliminar números en los ejes
    axis.title = element_blank(),                      # Eliminar títulos de los ejes
    plot.margin = margin(20, 20, 20, 20)               # Ajustar márgenes del gráfico
  ) +
  coord_sf()  # Ajustar automáticamente a la extensión de la provincia


########################
ggplot(data = cantones_eloro) +
  # Capa principal del mapa con TCU
  geom_sf(aes(fill = TCP), color = "black") +
  scale_fill_viridis_c(option = "C", name = "Tasa de Crecimiento Poblacional") +
  
  # Etiquetas con líneas más visibles
  geom_label_repel(data = cantones_eloro_centroides, 
                   aes(x = x, y = y, label = paste(DPA_DESCAN, "\n", round(TCP, 2))), 
                   size = 3, 
                   color = "black", 
                   fill = "white", 
                   box.padding = 0.7, 
                   point.padding = 0.5, 
                   segment.color = "gray30",  # Línea más visible
                   segment.size = 1,         # Aumentar grosor de línea
                   fontface = "bold",
                   max.overlaps = 50) +  # Permitir más solapamientos de etiquetas
  
  # Configuración de estilo
  theme_classic() +
  labs(title = "Tasa de Crecimiento Poblacional del El Oro",
       caption = "Fuente: INEC") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centrado y tamaño reducido del título
    plot.title.position = "plot",
    panel.grid = element_blank(),                      # Eliminar cuadrículas internas
    axis.ticks = element_blank(),                      # Eliminar marcas de los ejes
    axis.text = element_blank(),                       # Eliminar números en los ejes
    axis.title = element_blank(),                      # Eliminar títulos de los ejes
    plot.margin = margin(20, 20, 20, 20)               # Ajustar márgenes del gráfico
  ) +
  coord_sf()  # Ajustar automáticamente a la extensión de la provincia


