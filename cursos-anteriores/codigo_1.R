##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 1. INTRODUCCIÓN A R Y RSTUDIO                           #
#                                                                              #
##----------------------------------------------------------------------------##

## Para correr código es:
### Cursor en la línea o selección y ctrl + enter (o botón Run)

## ANOTACIONES ----

# Esta línea es una anotación. 
# R ignora todo lo que está acá adentro (tiene que empezar con #)
# Podemos escribir nombres de funciones u objetos y R no las va a interpetar
# Usar anotaciones es clave para poder entender qué fue lo que hicimos anteriormente


## AYUDA ----

help() ## documentación de la función help
help(mean) ## ayuda de la función mean (media) de Rbase
?mean ## otra forma de hacer lo mismo


## R COMO CALCULADORA ----

# Operaciones sencillas

2 + 2
## [1] 4

20 - 10

## [1] 10

10 / 2
## [1] 5

10 * 10
## [1] 100

## OBJETOS EN R ----

# Una operación sencilla:
17*17*7 # Se imprime el resultado

anio <- 17*17*7 # Se crea un objeto

(anio <- 17*17*7) # Se crea un objeto y se imprime


## ALGUNAS FUNCIONES BÁSICAS ----

ls() # Lista los objetos en el ambiente
rm(anio) # Borra objeto del ambiente
rm(list=ls()) # Borra todos los objetos del ambiente
help(ls) # Buscar ayuda sobre una función


## TIPOS Y CLASES ----

obj_1 <- "10"

typeof(obj_1)
## [1] "character"

obj_1 + 20 # Da error
## Error in obj_1 + 20: argumento no-numérico para operador binario


obj_1 <- 10
typeof(obj_1)
## [1] "double"

obj_1 + 20 # Funciona
## [1] 30

## COERCIONADORES ----

obj_1 <- "10"
typeof(obj_1)
## [1] "character"

obj_1 <- as.numeric(obj_1)
typeof(obj_1)
## [1] "double"

is.numeric(obj_1) # Podemos verificarlo directamente también
## [1] TRUE


## VECTORES ----

mi_primer_vector <- c(1, 3, 5, 7, 143) 
print(mi_primer_vector)

## [1]   1   3   5   7 143


## Otras formas de crear vectores con : y seq()

v1 <- c(1:5) # Todos los números de 1 a 5
v1
## [1] 1 2 3 4 5
v2 <- seq(0, 50, 10) # De 0 a 50 de a 10 números
v2
## [1]  0 10 20 30 40 50


# Indexación:
mi_primer_vector
## [1]   1   3   5   7 143

mi_primer_vector[1] # El primer elemento dentro del vector
## [1] 1

# Nos sirve por ejemplo para extraer partes del vector:
v3 <- mi_primer_vector[1:3] # Creo nuevo vector con los elementos del 1 al 3
v3
## [1] 1 3 5

## EJERCICIO (abrir/ver archivo ejercicios_1.R)


## DATA FRAMES

# Usamos la función data.frame
encuesta <- data.frame(edad = c(18,24,80,40,76), 
                       ideologia = c("Izquierda", "Izquierda", "Derecha", 
                                     "Centro", "Derecha"),
                       voto = c("Partido A", "Partido A", "Partido C",
                                "Partido B", "Partido B"))
class(encuesta)

encuesta


# Valor de fila 1 y columna 1
encuesta[1, 1]
## [1] 18

# Valor de toda la columna 1 (no fijamos filas entonces nos devuelve todas)
encuesta[, 1]
## [1] 18 24 80 40 76

# Valor de toda la fila 1 (no fijamos columnas entonces nos devuelve todas)
encuesta[1, ]
##   edad ideologia      voto
## 1   18 Izquierda Partido A


# Primero escribimos el nombre del dataframe, seguido por el símbolo $ y 
# el nombre de la variable (sin comillas)
encuesta$edad # Esto imprime todos los valores de esa variable
## [1] 18 24 80 40 76

# En un dataframe cada variable es un vector y podemos fijarnos su clase
class(encuesta$edad)
## [1] "numeric"

mean(encuesta$edad) # Podemos aplicarle funciones (la media en este caso)
## [1] 47.6


## EJERCICIO (abrir/ver archivo ejercicios_1.R)


# Supongamos que queremos calcular la media de: 12,24,36,48,60 
(12 + 24 + 36 + 48 + 60)/5 # Calculo directamente la media
data_ej <- c(12, 24, 36, 48, 60) # Genero el vector con los 5 números 
sum(data_ej) / length(data_ej) # Calculo con dos funciones su media
mean(data_ej) # Calculo la media directamente con la función mean()

# También se puede ingresar data directamente en el argumento x
mean(c(12, 24, 36, 48, 60)) 


media_fun <- mean(data_ej) # Sin explicitar argumento x

media_fun_x <- mean(x = data_ej) # Explicitando argumento x

identical(media_fun, media_fun_x) # El mismo resultado


identical(media_fun, media_fun_x) # por posición
identical(x = media_fun, y = media_fun_x) # por especificación


# Dataframe con el resultado de Uruguuay en los últimos 6 mundiales
uru_mundial <- data.frame(year = c(2002, 2006, 2010, 2014, 2018, 2022),
                          posicion = c(26, NA, 4, 12, 5, 20))
# Veamos la posición promedio:
mean(uru_mundial$posicion) 
# Como tenemos un dato perdido, la función nos devuelve NA

# Si especificamos el argumento na.rm (no tener en cuenta los datos perdidos):
mean(uru_mundial$posicion, na.rm = TRUE)


help(mean)


# Para descargar paquetes de CRAN, utilizamos la siguiente función:
install.packages("nombre_del_paquete")
install.packages("dplyr") # Ejemplo

# Existen otros paquetes no alojados en CRAN, que se instalan con
# el paquete "devtools" 


library(nombre_del_paquete)
library(dplyr) # Ejemplo

## DIALECTOS ----

library(readxl)
library(tidyverse)
library(data.table)
datauru <- read_excel("data/datauru.xlsx")
data <- select(datauru, anio, pbi, inflacion, desempleo, presidente) 

data

# R base
as.data.frame(t(sapply(X = split(
  x = data[which(data$presidente %in% c("Vázquez", "Sanguinetti")),
           which(colnames(data) %in% c("pbi", "inflacion"))],
  f = data$presidente[which(data$presidente %in% c("Vázquez", "Sanguinetti"))],
  drop = TRUE),
  FUN = function(x) {apply(x, 2, mean)})))

# Data table

data_dt <- data 
setDT(data_dt) 
data_dt[presidente %in% c("Vázquez", "Sanguinetti"),
        c("presidente", "pbi", "inflacion")  ][ 
          , lapply(.SD, mean), by = presidente] 

# Tidyverse

data %>%
  filter(presidente %in% c("Vázquez", "Sanguinetti")) %>% 
  select(presidente, pbi, inflacion) %>%
  group_by(presidente) %>% 
  summarise_all(mean) 



encuesta <- data.frame(edad = c(18,24,80), 
                       ideologia = c("Izquierda", "Izquierda", "Derecha"),
                       voto = c("Partido A", "Partido A", "Partido C"))


install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)
