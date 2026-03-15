##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    SOLUCION EJERCICIOS CLASE 2. Manipulación de datos                        #
#                                                                              #
##----------------------------------------------------------------------------##


## EJERCICIO IMPORTACION, EXPLORACION Y EXPORTACION ----

# (1) Importar el archivo excel en la carpeta data llamado "datauru.xlsx"

# setear la ruta o crear un proyecto previamente
library(rio)
data_uru <- import("data/datauru.xlsx")

# (2) Explorar el dataframe con dim(), names(), head()

dim(data_uru)

names(data_uru)

head(data_uru)

# (3) Explorar el dataframe con la función glimpse()

glimpse(data_uru)

# (4) Explorar el dataframe con la función str()

str(data_uru)

# (5) Explorar el dataframe con la función summary()

summary(data_uru)


# (6) Exportar (guardar) el archivo excel en la carpeta _resultados_ con el nombre "datauru.xlsx"

export(data_uru, "resultados/datauru.xlsx")



## EJERCICIO TRANSFORMACION DE DATOS ----

# (1) Abran un script nuevo y tomen el dataframe de gapminder (importarlo de la 
#carpeta data o descargarlo desde el paquete gapminder como debajo, y filtrar 
#para la base para quedarnos solamente con las observaciones América y Oceania
library(gapminder)
data_gapminder <- gapminder

#(2) En ese mismo dataframe con solamente las observaciones de América y 
# Oceanía y luego seleccionar solamente 3 variables: continent, country y lifeExp

data_gapminder <- data_gapminder %>% 
                  filter(continent == "Americas" | continent == "Oceania") 


data_gapminder <- data_gapminder %>% 
                  select(continent, country, lifeExp)
  
# (3) En ese mismo dataframe, crear una nueva variable que divida la expectativa
#de vida en 3 categorías: 
#"Alta" (mayor o igual a 75 años); 
# "Media" (mayor o igual a 65 y menor a 75 y 
# "Baja" (menor a 65 años)

data_gapminder <- data_gapminder %>% 
                  mutate(lifeExp_ordinal = case_when(
                         pop >= 75 ~ "Alta",
                         pop >= 65 & pop < 75 ~ "Media",
                         pop < 65 ~ "Baja",
                         TRUE ~ "Error"))

                    
# (4) Asegurarse de hacer los primeros 3 pasos en un mismo pipeline

data_gapminder <- gapminder %>% 
                  filter(continent == "Americas" | continent == "Oceania") %>% 
                  select(continent, country, lifeExp) %>% 
                  mutate(lifeExp_ordinal = case_when(
                    lifeExp >= 75 ~ "Alta",
                    lifeExp >= 65 & pop < 75 ~ "Media",
                    lifeExp < 65 ~ "Baja",
                    TRUE ~ "Error"))


# Extra: convertir la variable creada a factor modificando el mismo código con funciones anidadas

data_gapminder <- gapminder %>% 
  filter(continent == "Americas" | continent == "Oceania") %>% 
  select(continent, country, lifeExp) %>% 
  mutate(lifeExp_ordinal = as.factor(case_when(
    lifeExp >= 75 ~ "Alta",
    lifeExp >= 65 & pop < 75 ~ "Media",
    lifeExp < 65 ~ "Baja",
    TRUE ~ "Error")))
