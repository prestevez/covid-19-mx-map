library(tidyverse)
library(magrittr)
library(sf)

datos <- read_csv("datos_covid/200415COVID19MEXICO.csv")

datos %>% 
  filter(RESULTADO == 1) %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES) %>%
  summarise(casos = n()) -> casos_confirmados_municipio

casos_confirmados_municipio %$% sum(casos) ## misma cantidad que en reporte diario

pob1 <- read_csv("datos_covid/base_municipios_final_datos_01.csv")
pob2 <- read_csv("datos_covid/base_municipios_final_datos_02.csv")

pob1 %>%
  bind_rows(pob2) -> pob

rm(list = c("pob1", "pob2"))
gc()

pob %>%
  filter(ANO == 2020) %>%
  group_by(CLAVE, CLAVE_ENT) %>%
  summarise(POB = sum(POB)) -> pob_mun_2020

as.num <- function(x){
  as.numeric(as.character(x))
}

casos_confirmados_municipio %>%
  mutate(CLAVE = as.num(paste0(as.num(ENTIDAD_RES), MUNICIPIO_RES))) %>%
  right_join(pob_mun_2020) -> pob_covid

# proporcion de poblacion en municipios con casos confirmados

pob_tot <- sum(pob_mun_2020$POB)
pob_tot

pob_en_covid <- pob_covid %>%
  drop_na %$% sum(POB)

pob_en_covid

pob_en_covid/pob_tot * 100

mex_map <- st_read("datos_covid/mex_admbnda_adm2_govmex.shp")

# mex_map %>%
#   ggplot() + geom_sf()

pob_covid %>%
  mutate(covid = ifelse(is.na(casos), 0, 1),
    ADM2_PCODE = ifelse(CLAVE < 10000, paste0("MX0", CLAVE), paste0("MX",CLAVE))) -> pob_covid_2

mex_map %>%
  left_join(pob_covid_2) -> mex_map_covid

mex_map_covid %$% sum(casos, na.rm = TRUE)

mex_map_covid %>%
  filter(covid > 0) %>%
  ggplot() +
  geom_sf()


casos_confirmados_municipio %>%
  mutate(CLAVE = as.num(paste0(as.num(ENTIDAD_RES), MUNICIPIO_RES))) %$%
  unique(CLAVE) -> ssalud_muns_ids

pob_mun_2020 %$% 
  unique(CLAVE) -> conapo_muns

ssalud_muns_ids[!ssalud_muns_ids %in% conapo_muns] -> not_in_conapo

datos %>%
  mutate(CLAVE = as.num(paste0(as.num(ENTIDAD_RES), MUNICIPIO_RES))) %>%
  filter(CLAVE %in% not_in_conapo) %>%
  filter(RESULTADO == 1)

## Missing cases due to not established residence  

pob_covid_2 %$% sum(casos, na.rm = TRUE)
casos_confirmados_municipio %$% sum(casos)

pob_covid_2 %$% 
  unique(ADM2_PCODE) -> muns_pob_covid2

mex_map %$%
  unique(ADM2_PCODE) -> muns_map_code

# conapo municipalities not in map

muns_pob_covid2[!muns_pob_covid2 %in% muns_map_code]

mex_map_covid %>%
  filter(covid > 0) -> map_covid_positive

mex_map_covid %>% 
  st_intersects(map_covid_positive) -> covid_neighours

# sel_logical = lengths(sel_sgbp) > 0

no_neighbours <- lengths(covid_neighours) < 1

mex_map_covid[no_neighbours,]$covid <- 2 

mex_map_covid %>%
  mutate(dist_mun = factor(covid,
                           labels = c("Vecinos de contagios",
                                      "Con contagios",
                                      "Sin contagios y sin vecindad"))) -> mex_map_covid_2

mex_map_covid_2 %>%
  ggplot(aes(fill = dist_mun)) +
  geom_sf(colour = "black", size = 0.1) +
  scale_fill_manual("Distribución municipal",
    values = c("yellow", "red", "green")) +
  labs(title = "México: distribución de casos confirmados de covid-19",
       subtitle = "Actualizado al 16 de abril, 2020, 11:30 am",
       caption = "CC-BY @prestevez \nDatos: https://www.gob.mx/salud") +
  theme_bw() + 
  theme(legend.position = "bottom")

# poblacion en vecinos

mex_map_covid_2 %>%
  filter(covid == 0) %$% sum(POB) -> pob_en_vecinos

pob_en_vecinos/pob_tot * 100

100-(pob_en_covid + pob_en_vecinos)/pob_tot * 100

pob_tot-(pob_en_covid + pob_en_vecinos)

pob_brks <- c(0, 1000, 10000, 50000, 100000, 500000,
              1000000, 2000000)
pob_lbls <- c("< 1 mil habs.", 
              "< 10 mil habs.",
              "< 50 mil habs.",
              "< 100 mil habs.",
              "< 500 mil habs.",
              "< 1 millón habs.",
              "< 2 millón habs.")

mex_map_covid_2 %>%
  mutate(Población = cut(POB,
                         breaks = pob_brks, 
                         right = FALSE, 
                         labels = pob_lbls)) %>%
  ggplot(aes(fill = Población)) +
  geom_sf(colour = "black", size = 0.1, alpha = .5) +
  labs(title = "México: Población",
       subtitle = "Proyección demográfica para 2020",
       caption = "CC-BY @prestevez. \nCon datos abiertos de CONAPO") +
  theme_bw() + 
  #scale_fill_gradient(high = "red", low = "white") +
  scale_fill_brewer(palette = "Greens")

mex_map_covid_2 %>%
  count(dist_mun)

nrow(mex_map_covid_2)

nrow(pob_mun_2020)
