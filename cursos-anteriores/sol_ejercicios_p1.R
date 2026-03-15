##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    SOLUCION EJERCICIOS R  PARCIAL 1. | 8 de mayo 2024                        #
#                                                                              #
##----------------------------------------------------------------------------##

# EJERCICIOS R ----

# El parcial tiene dos partes:
# 1. Un cuestionario de múltiple opción  (Máximo 75 puntos)
# 2. Dos ejercicios en R cuya consigna está disponible en la plataforma Eva en la sección inicial con el título: "Ejercicios R Primer Parcial"  (Máximo 25 puntos).

# Para aprobar la evaluación se deberá obtener un mínimo de 50 puntos (3 en escala UdelaR).

# La hora de comienzo es 18.00 y la de finalización 20.00. 

# A continuación se presentan los dos ejercicios para realizar en R utilizando RStudio. 

# Los ejercicios están en el archivo ejercicios_p1.R con el texto en modo comentarios. 
# Se sugiere responder a cada ejercicio luego de la consigna.
# Recuerden guardar el script a medida que van trabajando para no perder información.

# Al finalizar los ejercicios deberán:
# - renombrar el o los scripts (.R) utilizados con su apellido y nombre 
# - subir a eva los scripts utilizados
# - subir a eva la base de datos exportada (en caso de corresponder)

# Se recomienda trabajar en un proyecto, pero no es estrictamente necesario. 
# En caso de hacerlo, pueden subir la carpeta del proyecto en un archivo .zip, que deberá contener el o los scripts con los que trabajaron, las bases de datos (en caso de corresponder) y el archivo .Rproj.

# En esta etapa de la evaluación pueden usar materiales de apoyo.

# Los ejercicios son individuales.



# EJERCICIO 1: VECTORES ----
# (Máximo 10 puntos | 2 puntos por respuesta correcta | sin detracción)

# 1. Crear dos vectores numéricos que contengan 4 elementos cada uno y asignarlos a los objetos num1 y num2.

num1 <- c(1, 2, 3, 4)
num2 <- c(1, 2, 3, 4)

# 2. Sumar los vectores num1 y num2 y asignar el resultado al objeto sum.

sum <- num1 + num2

# 3. Multiplicar los vectores num1 y num2 y asignar el resultado al objeto product.

product <- num1 * num2

# 4. Crear un vector de de tipo character con 4 elementos  y asignarlo a un objeto de nombre vec1.

vec1 <- c("Uruguay", "Argentina", "Brasil", "Paraguay")

# 5. Convertir el objeto vec1 en factor y asignarlo al objeto fact1.

fact1 <- as.factor(vec1)


# EJERCICIO 2: DATA FRAMES ----

#(Máximo 15 puntos | 1 punto por respuesta correcta | sin detracción)


# 1. Cargar los paquetes tidyverse y rio, descargar la base de datos "data.xlsx" disponible en esta sección, importarla asignandola a un objeto de nombre data.

library(tidyverse)
library(rio)

data <- gapminder::gapminder
export(data, "data.xlsx")

# 2. Explorar la base de datos data utilizando al menos tres de las funciones vistas en clase.

dim(data)
head(data)
glimpse(data)
str(data)

# 3. Calcular la media de la variable gdpPercap.

mean(data$gdpPercap)


# 4. Calcular la mediana y la desviación estandar de la variable gdpPercap.

median(data$gdpPercap)

sd(data$gdpPercap)


# 5. Filtrar la base de datos data para quedarse con los casos de países del continente americano y asignarla al objeto data_america.

data_america <- filter(data, continent == "Americas")


# 6. En el objeto data_america seleccionar las variables country, year, lifeExp, pop, gdpPercap y asignarla al objeto data_america.

data_america <- select(data_america, country, year, lifeExp, pop, gdpPercap)


# 7. Partiendo de la base de datos del objeto data (bae de datos inicial), realizar los puntos 5 y 6 en un sólo bloque de código utilizando el pipe y asignar el resultado al objeto data_america2.

data_america2 <- data %>% 
                filter(continent == "Americas") %>% 
                select(country, year, lifeExp, pop, gdpPercap)


# 8. Verificar si los objetos data_america y data_america2 son idénticos.

identical(data_america, data_america2)


# 9. En el objeto data_america crear en una nueva variable de nombre pbi a partir de las variables gdpPercap y pop.

data_america <- data_america %>% 
                mutate(pbi = gdpPercap * pop)

# 10. En el objeto data_america crear en una nueva variable de nombre gdpPercap_recod recodificando la variable gdpPercap de la siguiente manera: 
# - "Alto": mayor o igual a 7800
# - "Mediano": entre 3500 y 7799
# - "Bajo": menos de 3500

data_america <- data_america %>% 
  mutate(gdpPercap_recod = case_when(
    gdpPercap >= 7800 ~ "Alto",
    gdpPercap >= 3500 ~ "Mediano",
    gdpPercap < 3500 ~ "Bajo",
    TRUE ~ "Error")
  ) 


# 11. En el objeto data_america calcular una tabla de frecuencias absoluta (proporción y porcentaje) de la variable gdpPercap_recod y asignarla al objeto tabla.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data y la variable gdpPercap.

tabla <- table(data_america$gdpPercap_recod)


# 12. En el objeto data_america calcular una tabla de frecuencias relativa (proporción y porcentaje) de la variable gdpPercap y asignarlas a los objetos tabla2 y tabla3.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data y la variable gdpPercap.

tabla2 <- prop.table(tabla)

tabla3 <- prop.table(tabla)*100

# 13. En el objeto data_america, relizar un gráfico de dispersión de las variables lifeExp y gdpPercap.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data y las variable lifeExp y gdpPercap.

plot(data_america$lifeExp, data_america$gdpPercap)


# 14. En el objeto data_america, realizar un histograma de la variable gdpPercap.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data y la variable gdpPercap.

hist(data_america$gdpPercap)

# 15. Exportar el objeto data_america en formato .csv.

export(data_america, "data_america.csv")
