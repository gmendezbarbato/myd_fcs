##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2026 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 3.                                                      #
#                                                                              #
##----------------------------------------------------------------------------##

library(tidyverse)
library(gapminder)


# Repaso de Transformar datos ----

# Filtrar con filter()

gapminder <- gapminder
# Tenemos datos de muchos años:
table(gapminder$year)

# Filtremos para con los datos a partir de 2007
gapminder_07 <- filter(gapminder, year == 2007)
table(gapminder_07$year)

# Todas los años menos 2007
gapminder_pre07 <- filter(gapminder, year != 2007)
table(gapminder_pre07$year)

# Solo los siguientes años: 1952, 1992 y 2007
anios_especificos <- c(1952, 1992, 2007)
gapminder_esp <- filter(gapminder, year %in% anios_especificos)
table(gapminder_esp$year)


# Seleccionar con select()

# Selccionar un conjunto de variables (país, año, población)
select(gapminder, country, year, pop)
# Selccionar todas las variables menos las especificadas
select(gapminder, -continent)
# Seleccionar un rango de variables según orden
select(gapminder, country:lifeExp)
select(gapminder, 1:3) # Orden numérico


# Pipeline %>% 
gapminder_07_america <- gapminder %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-continent)
print(gapminder_07_america)

# Otras funciones

# count con 1 variable
gapminder %>% 
  count(continent) %>% 
  head(3) #imprimo solo las primeras tres filas

#count con 2 variables
gapminder %>% 
  count(continent, year) %>% 
  head(3) #imprimo solo las primeras tres filas

# rename
gapminder %>%
  count(continent) %>%
  rename(Continente = continent, Total = n)

#count() + rename() + arrange()
gapminder %>%
  count(continent) %>%
  rename(
    "Continente" = continent,
    "Cantidad de países" = n
  ) %>%
  arrange(`Cantidad de países`)


# Factores ----

library(forcats)
# creo un vector character de cinco elementos
nominal <- c("Femenino", "Masculino", "Masculino", "Femenino", "Masculino")
# convierto el vector a factor con la función as.factor()
nominal <- as.factor(nominal)
# chequeo clase de objeto
class(nominal)

# chequeo tipo de dato
typeof(nominal)

# chequeo niveles
levels(nominal)

# hago un recuento de frecuencia absoluta de cada categoría
fct_count(nominal)

# hago un recuento de frecuencia absoluta ordanada y relativa (proporción)
fct_count(nominal, sort = TRUE, prop = TRUE)

# modifico manualmente el orden de los factores
nominal <- fct_relevel(nominal, c("Masculino", "Femenino"))

# chequeo los niveles
levels(nominal)

# invierto el orden de los factores
nominal <- fct_rev(nominal)
# chequeo los niveles
levels(nominal)

# ordeno los niveles por frecuencia (primero el más frecuente)
nominal <- fct_infreq(nominal)
# chequeo los niveles
levels(nominal)

# Cambio el nombre de los niveles 
nominal <- fct_recode(nominal, "F" = "Femenino", "M" = "Masculino")

# chequeo los niveles
levels(nominal)

# agrego un nivel nuevo
nominal <- fct_expand(nominal, "Sin Dato")
# chequeo los niveles
levels(nominal)

# elimino los niveles no usados
nominal <- fct_drop(nominal)
# chequeo los niveles
levels(nominal)

# Crear y recodificar variables ----

data_gapminder <- gapminder
# Variable de caracteres
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo") 
# Variable numérica
data_gapminder <- mutate(data_gapminder, var2 = 7) 
head(data_gapminder, 3)

## Podemos escribir lo mismo de distinta manera:
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo",
                         var2 = 7)


## Podemos recodificar usando variables y operadores aritméticos
# Calculemos el pbi total (pbi per capita * población)
d_gap <- mutate(gapminder, gdp = gdpPercap * pop)
head(d_gap, 3)


# Podemos calcular el logaritmo 
d_gap <- mutate(d_gap, gdp_log = log(gdp))
head(d_gap, 2)


# Exploro tipo de variables
glimpse(d_gap)


# Variable continente a caracteres y año a factor
d_gap <- d_gap %>% 
  mutate(continent = as.character(continent),
         year = as.factor(year))
glimpse(d_gap)


# Recodificaciones condicionales

# ifelse
# Crear un vector de números
numeros <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Usar ifelse para clasificar números como "par" o "impar"
# el operador %% calcula el residuo de una división
clasificacion <- ifelse(numeros %% 2 == 0, "par", "impar")
clasificacion # chequeo


# cargo de nuevo la base gapminder, del paquete gapminder y la asigno al objeto gapminder
gapminder <- gapminder 

# Crear una columna que clasifique países por esperanza de vida
gapminder %>% 
  mutate(categoria_vida = ifelse(lifeExp > 60, "Alta", "Baja")) %>% 
  select(country, lifeExp, categoria_vida) %>% 
  head(5)

# creo un nuevo objeto de nombre d_gap con la base gapminder (para variar je)
d_gap <- gapminder
# Creemos una variable que indique si el país es Uruguay o no
d_gap <- d_gap %>% 
  mutate(uruono = case_when(
    country == "Uruguay" ~ "Si",
    TRUE ~ "No")
  )
table(d_gap$uruono)


d_gap <- gapminder
d_gap <- d_gap %>% 
  mutate(mercosur = case_when(country == "Uruguay" ~ 1,
                              country == "Argentina" ~ 1,
                              country == "Paraguay" ~ 1,
                              country == "Brazil" ~ 1, 
                              TRUE ~ 0))
table(d_gap$mercosur)


d_gap <- d_gap %>% 
  mutate(mercosur = case_when(
    country %in% c("Argentina", "Paraguay", "Brazil", "Uruguay") ~ 1,
    TRUE ~ 0)
  ) 
d_gap <- d_gap %>% 
  mutate(mercosur2 = case_when(
    country == "Argentina" | country == "Paraguay" |
      country == "Brazil" | country == "Uruguay" ~ 1,
    TRUE ~ 0)
  )
identical(d_gap$mercosur, d_gap$mercosur2)


d_gap <- d_gap %>% 
  mutate(pob_rec = case_when(
    pop >= 20000000 ~ "Grande",
    pop >= 5000000 & pop < 20000000 ~ "Mediana",
    pop < 5000000 ~ "Pequeña",
    TRUE ~ "Error")
  ) 
table(d_gap$pob_rec)

d_gap <- d_gap %>% 
  mutate(var1 = case_when(gdpPercap > 20000 ~ 1,
                          lifeExp > 75 ~ 1,
                          TRUE ~ 0))
table(d_gap$var1)

# Funciones: group_by() y summarise() ----


gapminder <- gapminder





# Agrupar por continente y calcular el promedio de expectativa de vida
promedio_vida <- gapminder %>%
  group_by(continent) %>%
  summarise(promedio_vida = mean(lifeExp, na.rm = TRUE))

promedio_vida # imprimo


# Filtrar por año 2007, agrupar por continente y calcular la población total
poblacion_total <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(poblacion_total = sum(pop, na.rm = TRUE))

# Mostrar los resultados
poblacion_total # imprimo


# Agrupar por país y calcular el máximo PIB per cápita
max_pib_per_capita <- gapminder %>%
  group_by(country) %>%
  summarise(max_gdpPercap = max(gdpPercap, na.rm = TRUE))

# Mostrar los resultados
max_pib_per_capita # imprimo


# Tablas cruzadas ----

library(rio)
# carguen el archivo d_gap.csv que está en eva
d_gap <- import("data/d_gap.csv") 

# Frecuencia simple
d_gap %>% 
  count(continent)

# Frecuencia proporción
d_gap %>% 
  count(continent) %>% 
  mutate(per = n/sum(n)*100) %>% # Porcentaje
  mutate(prop = n/sum(n)) # Proporción


# Tabla cruzada en formato largo
d_gap %>% 
  count(continent, pob_rec)



# Tabla cruzada en formato ancho
d_gap %>% 
  count(continent, pob_rec) %>% 
  spread(pob_rec, n)



# Porcentaje total
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100) %>% 
  spread(pob_rec, n)



# % de paises en cada tamaño de población por continente 
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100, .by = continent) %>% 
  spread(pob_rec, n)

# % de continente por tamaño de población  
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100, .by = pob_rec) %>% 
  spread(pob_rec, n)


gapminder %>% 
  summarise(media = mean(lifeExp, na.rm=T))

# Por ahora no hay mucha diferencia con
mean(gapminder$lifeExp, na.rm = TRUE)


gapminder %>% 
  group_by(year) %>% 
  summarise(media = mean(lifeExp, na.rm = T)) 


resumen_1 <- gapminder %>% 
  filter(continent %in% c("Americas", "Europe")) %>% 
  filter(year >= 1997) %>% 
  group_by(continent, year) %>% 
  summarise(media = mean(lifeExp, na.rm = TRUE)) 
resumen_1


resumen_2 <- gapminder %>% 
  filter(continent %in% c("Americas", "Europe")) %>%
  filter(year == 2007) %>%
  group_by(continent) %>% 
  summarise(media = mean(gdpPercap),
            desvio = sd(gdpPercap),
            suma = sum(gdpPercap),
            max = max(gdpPercap),
            min = min(gdpPercap),
            paises = n()) 

resumen_2












