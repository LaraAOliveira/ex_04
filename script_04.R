##quarto exercício

pacman::p_load(terra, spData)
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)


#importando os dados 
my_rast = rast("brasil_coverage_2020.tif")
plot(my_rast)

#pegando os municípios
munic_t = read_municipality(year =2020)
rio_de_janeiro = munic_t %>% 
  filter(abbrev_state == "RJ") 

# crop, mask e extract
cp = crop(my_rast, rio_de_janeiro)
mk = mask(cp, vect(rio_de_janeiro))
ext = extract(mk, vect(rio_de_janeiro))

plot(cp)
plot(mk)

#somando a cobertura total por município do RJ
cobertura <- ext %>%
  group_by(ID) %>%
  summarise(cobertura = n())

#pegando somente a cobertura vegetal
cobertura_vegetal <- ext %>%
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  summarise(cobertura_v = n())

#juntando as coberturas em um df
cobertura_rio <- merge(cobertura, cobertura_vegetal, by=c("ID"), all.x=TRUE)

#dividindo cobertura vegetal pela total
cobertura_rio <- cobertura_rio %>%
  mutate(p_v = cobertura_v/cobertura)

#criando coluna ID no df original e juntando
rio_de_janeiro <- rio_de_janeiro %>%
  mutate(ID = c(1:92), .after = code_muni) %>%
  left_join(cobertura_rio, by = "ID")

#plotando mapa do RJ
plot_rio_cobertura <- rio_de_janeiro %>%
  ggplot() +
  geom_sf(aes(fill = p_v), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Porcentagem") +
  labs(title = "Porcentagem de Cobertura Vegetal por Município", subtitle = "Estado do Rio de Janeiro")
plot_rio_cobertura

#forest share por município
forest_share_m <- rio_de_janeiro %>%
  subset(select = c(name_muni, p_v))

