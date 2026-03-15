##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 2. INTRODUCCIÓN A R Y RSTUDIO                           #
#                                                                              #
##----------------------------------------------------------------------------##

# Directorios de trabajo ----

# Puedo fijar el directorio de trabajo con la función setwd()
# Fijar la carpeta donde vamos a importar y exportar los archivos:
setwd("micompu/micarpeta") # ojo con la / no es \
getwd() # Con está función puedo consultar el directorio



# Ahora, si quiero leer un archivo que esté en "micompu/micarpeta" simplemente 
# escribo su nombre dentro de la función, en el lugar del "path".
# Supongamos que tengo dentro de la carpeta "micarpeta" un excel con datos
# de desempleo en Uruguay:
library(readxl)
desempleo_uru <- read_excel("data/desempleo.xlsx")
head(desempleo_uru, 4)

# Cambiar la ruta por la ruta en la cual está su carpeta
setwd("C:/Usuario/User/Desktop/cursos/myd/carpeta_del_curso")

datauru <- read_excel("data/datauru.xlsx")

# here::here 
install.packages("here")
help(here)


# Importar datos  ----

# Con la función read_csv() del paquete readr importamos archivos .csv
library(tidyverse)
gapminder_csv <- read_csv("data/gapminder.csv")
# Con la función read_excel() del paquete readxl importamos archivos excel
library(readxl)
gapminder_excel <- read_excel("data/gapminder.xlsx")
# Vemos que los dataframes son iguales, tienen la mismas filas y columnas
dim(gapminder_csv)
## [1] 1704    6
dim(gapminder_excel)
## [1] 1704    6

#install.packages("gapminder")
library(gapminder)
data_gapminder <- gapminder
head(data_gapminder)

library(haven)
# SPSS
gapminder_spss <- read_spss("data/gapminder.sav") 
# STATA
gapminder_stata <- read_stata("data/gapminder.dta")

# SPSS
gapminder_spss <- haven::read_spss("data/gapminder.sav") 
# STATA
gapminder_stata <- haven::read_stata("data/gapminder.dta")



# Para esto no necesitamos cargar paquetes. 
# Guardar un objeto como .rds:
saveRDS(object = data_gapminder,
        file = "resultados/data_gapminder.rds") 
# Leemos un archivo .rds
miobjeto_rds <- readRDS(file = "resultados/data_gapminder.rds")
# Con .rda se pueden guardar varios objetos al mismo tiempo!
# Exportamos un archivo .Rdata
save(data_gapminder, miobjeto_rds,
     file = "resultados/dos_dataframes.Rdata") 
# Importamos un archivo .Rdata
load("resultados/dos_dataframes.Rdata")


# Exportar datos  ----

# Guardar .csv
library(gapminder)
data_gapminder <- gapminder
write_excel_csv(data_gapminder, "resultados/gapminder.csv")
# Guardar excel
library(writexl)
write_xlsx(data_gapminder, "resultados/gapminder.xlsx")
# Guardar .dta (Stata)
library(haven)
write_dta(data_gapminder, "resultados/gapminder.dta")
# Guardar .sav (SPSS)
write_sav(data_gapminder, "resultados/gapminder.sav")
# Guardar .sas (SAS)
write_sas(data_gapminder, "resultados/gapminder.sas")


# Paquete rio  ----

# chequeo si está instalado, sino lo instalo y lo cargo
if(!require(rio)){
  install.packages("rio")
  library(rio)
}

# Import
# varios paquetes
# Importar csv
# Con la función read_csv() del paquete readr importamos archivos .csv
library(tidyverse) # carga readr
gapminder_csv <- read_csv("data/gapminder.csv")
# Importar Excel
# Con la función read_excel() del paquete readxl importamos archivos excel
library(readxl)
gapminder_excel <- read_excel("data/gapminder.xlsx")
# Importar SPSS
# Con la función read_spss() del paquete haven importamos archivos SPSS
library(haven)
gapminder_spss <- read_spss("data/gapminder.sav")

# rio
# Con la función import() de rio importamos, csv, excel, SPSS y más...
library(rio)
# Importar csv
gapminder_csv <- import("data/gapminder.csv")
# Importar Excel
gapminder_excel <- import("data/gapminder.xlsx")
# Importar SPSS
gapminder_spss <- import("data/gapminder.sav")


# Export
# varios paquetes
# Guardar .csv
# Con la función write_excel_csv del paquete readr exportamos archivos csv
write_excel_csv(gapminder_csv, "resultados/gapminder.csv")
# Guardar excel
# Con la función write_xlsx del paquete writexl exportamos archivos excel
library(writexl)
write_xlsx(gapminder_excel, "resultados/gapminder.xlsx")
# Guardar .sav (SPSS)
# Con la función write_sav() del paquete haven exportamos archivos SPSS
library(haven)
write_sav(gapminder_spss, "resultados/gapminder.dta")

# rio
# Con la función export() de rio exportamos, csv, excel, SPSS y más...
library(rio)
# Exportar (guardar) csv
export(gapminder_csv, "resultados/gapminder.csv")
# Exportar (guardar) Excel
export(gapminder_excel, "resultados/gapminder.xlsx")
# Exportar (guardar) SPSS
export(gapminder_spss, "resultados/gapminder.sav")


# Resumen visual de un dataframe  ----

view(data_gapminder)
dim(data_gapminder) # Número de filas y columnas
## [1] 1704    6
names(data_gapminder) # Nombre de variables
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
head(data_gapminder, 3) # Imprime primeras filas (3 en este caso)

# Resumen visual más completo:
glimpse(gapminder)


# Tablas simples  ----

# Para obtener una tabla de frecuencias de una variable usamos la función
# table() de R Base
tabla_1 <- table(data_gapminder$continent) # Frecuencia simple
tabla_1
## 
##   Africa Americas     Asia   Europe  Oceania 
##      624      300      396      360       24
prop.table(tabla_1) # Proporciones
## 
##     Africa   Americas       Asia     Europe    Oceania 
## 0.36619718 0.17605634 0.23239437 0.21126761 0.01408451
addmargins(tabla_1) # Totales
## 
##   Africa Americas     Asia   Europe  Oceania      Sum 
##      624      300      396      360       24     1704
addmargins(prop.table(tabla_1)) # Proporciones y totales
## 
##     Africa   Americas       Asia     Europe    Oceania        Sum 
## 0.36619718 0.17605634 0.23239437 0.21126761 0.01408451 1.00000000

# Estadistica descriptiva  ----

# Medidas de tendencia central
mean(data_gapminder$lifeExp) # Media
## [1] 59.47444
median(data_gapminder$lifeExp) # Mediana
## [1] 60.7125
sd(data_gapminder$lifeExp) # Desvío estandar
## [1] 12.91711

# Rangos
range(data_gapminder$lifeExp) # Rango
## [1] 23.599 82.603
max(data_gapminder$lifeExp)
## [1] 82.603
min(data_gapminder$lifeExp)
## [1] 23.599

#Histogramas
hist(data_gapminder$lifeExp,
       main = "Distribución de expectativa de vida (Gapminder)")

# Gráfico de dispersión (scatterplot)
plot(data_gapminder$lifeExp, data_gapminder$gdpPercap,
     main = "Relación entre expectativa de vida y PBI per cápita")


# Ejercicio (abrir clase_2_ejercicios.R)  ----

# Transformar datos  ----

## Filtrar  ----

library(tidyverse)
# Tenemos datos de muchos años:
table(gapminder$year)
## 
## 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 2002 2007 
##  142  142  142  142  142  142  142  142  142  142  142  142
# Filtremos para con los datos a partir de 2007
gapminder_07 <- filter(gapminder, year == 2007)
table(gapminder_07$year)

# Todas los años menos 2007
gapminder_pre07 <- filter(gapminder, year != 2007)
table(gapminder_pre07$year)
## 
## 1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 2002 
##  142  142  142  142  142  142  142  142  142  142  142
# Solo los siguientes años: 1952, 1992 y 2007
anios_especificos <- c(1952, 1992, 2007)
gapminder_esp <- filter(gapminder, year %in% anios_especificos)
table(gapminder_esp$year)
## 
## 1952 1992 2007 
##  142  142  142

## Seleccionar variables  ----

# Selccionar un conjunto de variables (país, año, población)
select(gapminder, country, year, pop)
# Selccionar todas las variables menos las especificadas
select(gapminder, -continent)
# Seleccionar un rango de variables según orden
select(gapminder, country:lifeExp)
select(gapminder, 1:3) # Orden numérico


## Pipeline %>%  ----

gapminder_07_america <- gapminder %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-continent)
print(gapminder_07_america)

## Crear y recodificar variables  ----

# Crear variables con mutate()
data_gapminder <- gapminder
# Variable de caracteres
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo") 
# Variable numérica
data_gapminder <- mutate(data_gapminder, var2 = 7) 
head(data_gapminder, 3)

## Podemos escribir lo mismo de distinta manera:
data_gapminder <- mutate(data_gapminder, var1 = "Valor fijo",
                         var2 = 7)


## Recodificar variables con mutate()  ----

## Podemos recodificar usando variables y operadores aritméticos
# Calculemos el pbi total (pbi per capita * población)
d_gap <- mutate(gapminder, gdp = gdpPercap * pop)
head(d_gap, 3)


# Podemos calcular el logaritmo 
d_gap <- mutate(d_gap, gdp_log = log(gdp))
head(d_gap, 2)


# Transformaciones de tipo

# Exploro tipo de variables
glimpse(d_gap)

# Variable continente a caracteres y año a factor
d_gap <- d_gap %>% 
  mutate(continent = as.character(continent),
         year = as.factor(year))
glimpse(d_gap)


## Recodificaciones condicionales  ----

# Recodificación condicional con case_when y mutate
data %>% 
  mutate(var_nueva = case_when(var_original == "Valor 1" ~ "Valor A",
                               var_original == "Valor 2" ~ "Valor B",
                               TRUE ~ "Otros"))

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


# También podríamos usar operadores para simplificar esto
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

# recodificación con valores numericos
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


# Ejercicio (abrir clase_2_ejercicios.R)  ----
 