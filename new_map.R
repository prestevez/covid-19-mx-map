library(tidyverse)
library(magrittr)
library(sf)

datos_mun <- read_csv("datos_covid/Casos_Diarios_Estado_Nacional_Confirmados.csv")

datos_mun %>%
  pivot_longer(-c(1:3)) %>%
  group_by(cve_ent, poblacion, nombre) %>%
  summarise(casos = sum(value)) %>%
  ungroup -> datos_mun_long

# Total de casos
(datos_mun_long %$% sum(casos) -> casos_total)

# Total de poblacion
(datos_mun_long %$% sum(poblacion) -> pob_total)

# Total de municipios
(datos_mun_long %>% nrow -> mun_total)

# Load map

mex_map2 <- read_sf("datos_covid/01_32_mun.shp")

mex_map2 %>%
  left_join(datos_mun_long, by = c("CVEGEO" = "cve_ent")) -> mex_map_covid

brk <- function(x){
  
  lvs <- c( "0 casos",
            "1 a 5",
            "6 a 10",
            "11 a 20",
            "21 a 50",
            "51 a 100",
            "101 o más")
  
  ifelse(x == 0,
         lvs[1],
         ifelse(x < 6,
                lvs[2],
                ifelse(x < 11,
                       lvs[3],
                       ifelse(x < 21,
                              lvs[4],
                              ifelse(x < 50,
                                     lvs[5],
                                     ifelse(x < 101,
                                            lvs[6],
                                            lvs[7])))))) -> y
  
  factor(y, levels = lvs)

  }

mex_map_covid %>%
  mutate("Número de casos" = brk(casos)) %>%
  ggplot(aes(fill=`Número de casos`)) +
  geom_sf(colour = "white", size = 0.05) +
  labs(title = "México: Distribución de casos confirmados de Covid-19",
       subtitle = "Actualizado al 17 de abril, 2020, 9:54 am",
       caption = "CC-BY @prestevez. \nDatos: https://www.gob.mx/salud") +
  theme_bw() + 
  # scale_fill_gradient(high = "red", low = "white")
  scale_fill_brewer(palette = "Reds") -> casos_covid

ggsave("casos_covid.png",
       casos_covid,
       width = 7,
       height = 5)

ggsave("casos_covid.pdf",
       casos_covid,
       width = 11,
       height = 8.5,
       device = cairo_pdf)

# mapa de "riesgo"

 
# mex_map_covid[no_neighbours,]$covid <- 2 

mex_map_covid %>%
  filter(casos > 0) -> mex_map_covid_positive


mex_map_covid %>%
  st_intersects(mex_map_covid_positive) -> vecinos_covid

sin_vecinos_o_covid <- lengths(vecinos_covid) < 1

mex_map_covid %>%
  mutate(covid = ifelse(casos > 0, 1, 0)) -> mex_map_covid_2

mex_map_covid_2[sin_vecinos_o_covid, ]$covid <- 2

mex_map_covid_2 %>%
  mutate(dist_mun = factor(covid,
                           labels = c("Vecinos de contagios",
                                      "Con contagios",
                                      "Sin contagios y sin vecindad"))) -> mex_map_covid_2

mex_map_covid_2 %>%
  ggplot(aes(fill = dist_mun)) +
  geom_sf(colour = "grey75", size = 0.05) +
  scale_fill_manual("Distribución municipal",
                    values = c("yellow", "red", "green")) +
  labs(title = "México: distribución de casos confirmados de covid-19",
       subtitle = "Actualizado al 17 de abril, 2020, 10:25 am",
       caption = "CC-BY @prestevez \nDatos: https://www.gob.mx/salud") +
  theme_bw() +
  theme(legend.position = "bottom") -> casos_vecindad

ggsave("casos_vecindad.png",
       casos_vecindad,
       width = 7.5,
       height = 5)

ggsave("casos_vecindad.pdf",
       casos_vecindad,
       width = 11,
       height = 8.5,
       device = cairo_pdf)

