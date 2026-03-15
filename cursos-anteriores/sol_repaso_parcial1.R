##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    EJERCICIOS REPASO PARCIAL 1.                                              #
#                                                                              #
##----------------------------------------------------------------------------##


# VECTORES ----

# 1. Crear un vector numérico con tres elementos y asignarlo al objeto vec1 

vec1 <- c(2, 3, 5)

# 2. Crear un vector character que contenga los numeros 1, 2, 3 y asignarlo al objeto vec 2.
vec2 <- c("1", "2", "3")

# 3. Sumar los vectores vec1 y vec 2 y asignarlos al vector de nombre vec3. 
# Realizar las operaciones necesarias para que los vectores puedan sumarse.
vec2 <- as.numeric(vec2)

vec3 <- vec1 + vec2


# PROYECTOS ----

# 1. Crear un proyecto en una carpeta existente de nombre repaso_myd

# Se crea una carpeta en la pc de nombre repaso_myd
# Se ingresa a RStudio y se va a File / New Project, se crea el proyecto en un directorio existente (repaso_myd)

# 2. Crear un proyecto en una carpeta nueva de nombre repaso_myd2

# Se ingresa a RStudio y se va a File -> New Project, se crea el proyecto en un nuevo directorio con el nombre repaso_myd2


# CARGAR PAQUETES ----

# 1. Cargar paquetes tidyverse y rio

library(tidyverse)
library(rio)


# DESCARGAR BASE DE DATOS ----

# 1. Descargar la base de de datos pob_demo.xlsx de la sección de eva: 
# Repaso primer parcial y colocarla en la carpeta del proyecto dentro 
# de una carpeta de nombre data

# Se ingresa a eva y se descarga el archivo pob_demo.xlsx.
# Se ubica el archivo en descargas y se lo mueve hacia la carpeta data 
# (que deberá crear previamente) dentro de la carpeta 
# repaso_myd (donde se ubica el proyecto).


# IMPORTAR BASES DE DATOS EN R ----

# 1. Importar pob_demo.xlsx y asignarlo al objeto pob_demo

pob_demo <- import("data/pob_demo.xlsx")


# EXPLORAR BASES DE DATOS ----

# 1. Explorar la base de datos pob_demo utilizando las siguientes funciones:

# dim()
# names()
# head()
# glimpse()
# str()
# summary()

dim(pob_demo)
names(pob_demo)
head(pob_demo)
glimpse(pob_demo)
str(pob_demo)
summary(pob_demo)


# FILTRAR Y SLECCIONAR ----

# 1. Filtrar la base y quedarse con los datos que correspondan al continente Sudamerica y asigrnarlas al objeto pob_demo_sud.

pob_demo_sud <- filter(pob_demo, continente == "Sudamerica") 

# 2. Filtrar nuevamente la base y quedarse con los datos a partir del año 1900 en adelante.

pob_demo_sud <- filter(pob_demo_sud, anio >= 1900) 

# 3. En el objeto objeto pob_demo_sud, seleccionar con asignación las variables continente, anio, pob, pob_democracias_electorales, pob_democracias_liberales.

pob_demo_sud <- select(pob_demo_sud, continente, anio, pob, pob_democracias_electorales, pob_democracias_liberales) 


# PIPE ----

# 1. Realizar los pasos anteriores en un sólo bloque de código utilizando el pipe %>% y asignarlo al objeto  pob_demo_sud2

pob_demo_sud2 <- pob_demo %>% 
  filter(continente == "Sudamerica") %>% 
  filter(anio >= 1900) %>% 
  select(continente, anio, pob, pob_democracias_electorales, pob_democracias_liberales) 

# 2. Verificar que pob_demo_sud y pob_demo_sud2 sean identicos

identical(pob_demo_sud, pob_demo_sud2)


#   CREAR NUEVAS VARIABLES ----

# 1. En el objeto pob_demo_sud, crear una nueva variable que contenga el dato de la suma de población que vive en democracias electorales o democracias liberales en sudamerica, denominar esa variable pob_democracias.

pob_demo_sud <- pob_demo_sud %>% 
  mutate(pob_democracias = pob_democracias_electorales + pob_democracias_liberales)

# 2. En el objeto pob_demo_sud, crear una nueva variable que contenga el dato del porcentaje de población que vive en democracias (electorales o liberales) en sudamerica, denominar esa variable pob_democracias_porc.

pob_demo_sud <- pob_demo_sud %>% 
  mutate(pob_democracias_por = (pob_democracias / pob)*100)


# CREAR UNA VARIABLE RECODIFICANDO OTRA VARIABLE ----

# 1. En el objeto pob_demo_sud, crear una nueva variable de nombre cobertura_democracia a partir de recodificar la variable pob_democracias_por de la siguiente manera:

# - Alta: mayor o igual a 75 
# - Media: entre 50 y 74
# - Baja: entre 25 y 49
# - Muy baja: menor a 25

pob_demo_sud <- pob_demo_sud %>% 
  mutate(cobertura_democracia = case_when(
    pob_democracias_por >= 75 ~ "Alta",
    pob_democracias_por >= 50 ~ "Media",
    pob_democracias_por >= 25 ~ "Baja",
    pob_democracias_por < 25 ~ "Muy baja",
    TRUE ~ "Error"))


# TABLAS ----

# 1. A partir del objeto pob_demo_sud, realizar tablas de frecuencias absoluta y relativa (proporción y porcentaje) de la siguiente variable cobertura_democracia: 

# frecuencia absoluta
tabla <- table(pob_demo_sud$cobertura_democracia)

# imprimo en consola
tabla

# frecuencia relativa (proporción)
tabla_prop <- prop.table(tabla)

# imprimo en consola
tabla_prop

# frecuencia relativa (porcentaje)
tabla_porc <- prop.table(tabla)*100

# imprimo en consola
tabla_porc


# MEDIA ----

# 1. A partir del objeto pob_demo_sud, calcular la media de la variable pob_democracias_por

mean(pob_demo_sud$pob_democracias_por)


# GRAFICO ----

# 1. A partir del objeto pob_demo_sud, hacer un gráfico de dispersión de las las variables anio y pob_democracias_por

plot(pob_demo_sud$anio, pob_demo_sud$pob_democracias_por,
     main = "Porcentaje de población viviendo en democracias y año")


# EXPORTAR ----

# 1.Exportar el objeto **pob_demo_sud** en excel (.xlsx).

export(pob_demo_sud, "pob_demo_sud.xlsx")
    
