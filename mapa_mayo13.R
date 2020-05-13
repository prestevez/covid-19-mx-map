library(tidyverse)
library(sf)
library(magrittr)

# Carga el archivo shapefile a R
mex_map <- st_read("datos_covid/01_32_mun.shp")

datos_mun <- read_csv("datos_covid/Casos_Diarios_Municipio_Confirmados_20200512.csv")

datos_mun

casos_municipio <- datos_mun %>%
  # pasamos del formato ancho al largo
  pivot_longer(-c(cve_ent, poblacion, nombre),
               names_to = "día",
               values_to = "casos") %>%
  # agrupamos por municpio
  group_by(cve_ent, poblacion, nombre) %>%
  # sumamos los casos
  summarise(total_casos = sum(casos))

casos_municipio

mex_map_covid <- mex_map %>%
  # unir tablas
  left_join(casos_municipio,
            # indicar explícitamente las columnas índice,
            # necesario cuando no tienen el mismo nombre
            by = c("CVEGEO" = "cve_ent"))

mex_map_covid

mex_map_covid %>%
  # seleccionamos a los municipios con por lo menos un caso
  filter(total_casos > 0) -> mex_map_covid_contagio

mex_map_covid %>%
  # Econtramos los vecinos
  st_intersects(mex_map_covid_contagio) -> vecinos_covid

vecinos_covid

sin_vecinos_o_covid <- lengths(vecinos_covid) < 1

mex_map_covid <-
  mex_map_covid %>%
  # creamos una columna nueva llamada covid, en la que el 1 indica contagios
  mutate(covid = ifelse(total_casos > 0, 1, 0))

# Cambiamos a 2 en la columna covid los municipios sin vecinos o contagios
mex_map_covid[sin_vecinos_o_covid, ]$covid <- 2

# Cambiamos la columna covid de número a factor con categorías.
mex_map_covid %>%
  mutate(covid = factor(covid,
                        labels = c("Vecinos de contagios",
                                   "Con contagios",
                                   "Sin contagios y sin vecindad"))) -> mex_map_covid

mex_map_covid %>%
  ggplot(aes(fill = covid)) +
  geom_sf(colour = "grey75", size = 0.05) +
  scale_fill_manual("Distribución municipal",
                    values = c("yellow", "red", "green")) +
  labs(title = "México: distribución de casos confirmados de covid-19",
       subtitle = "Actualizado al 13 de mayo, 2020, 16:55 pm",
       caption = "CC-BY @prestevez \nDatos: https://www.gob.mx/salud") +
  theme_bw() +
  theme(legend.position = "bottom") -> mapa_vecinos_mayo


# Guargdar en formato PNG
ggsave("casos_vecindad_mayo12.png",
       mapa_vecinos_mayo,
       width = 7.5,
       height = 5)

# En formato PDF en tamaño carta
ggsave("casos_vecindad_mayo12.pdf",
       mapa_vecinos_mayo,
       width = 11,
       height = 8.5,
       device = cairo_pdf)

# Población en municipios "verdes"
mex_map_covid %>%
  filter(covid == "Sin contagios y sin vecindad") %$%
  sum(poblacion) -> pob_verde

mex_map_covid %$%
  sum(poblacion, na.rm = TRUE) -> pob_total

pob_verde/pob_total * 100

mex_map_covid_2 %>%
  filter(dist_mun == "Sin contagios y sin vecindad") %>% nrow
