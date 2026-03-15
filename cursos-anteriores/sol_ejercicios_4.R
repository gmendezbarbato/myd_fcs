##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    EJERCICIOS CLASE 4. FACTORES                                              #
#                                                                              #
##----------------------------------------------------------------------------##

## EJERCICIO FACRTORES ----

# (1) Crear un factor de nombre nivel_edu, que tenga 10 elementos y que sus categorías sean "Alto", "Medio" y "Bajo"

# creo vector
nivel_edu <- c("Bajo", "Alto", "Alto", "Alto",  "Alto", "Bajo", "Alto", "Alto", "Bajo", "Medio")

# convierto a factor
nivel_edu <- as.factor(nivel_edu)

# lo mismo en una línea
nivel_edu2 <- as.factor(c("Alto", "Alto", "Alto", "Bajo", "Alto", "Bajo", "Alto", "Alto", "Bajo", "Medio"))

# chequeo que sean iguales
identical(nivel_edu, nivel_edu2)

# (2) Crear una tabla de frecuencia con la función fct_count() de forcats

library(forcats)

fct_count(nivel_edu)

# (3) Crear una tabla de frecuencia con la función fct_count() de forcats, que además ordene según frecuencia y calcule la frecuencia relativa (proporción)

fct_count(nivel_edu, sort = TRUE, prop = TRUE)


# (4) Cambiar los nombres de los niveles por "Más de 12 años", "Entre 6 y 12 años", "Menos de 6 años"). Si tiene dudas utilice foros o la documentación del paquete

#chequeo niveles
levels(nivel_edu)

# cambio orden de los factores
nivel_edu <- fct_recode(nivel_edu, "Más de 12 años" = "Alto", "Entre 6 y 12 años" = "Medio", "Menos de 6 años" = "Bajo")

#chequeo niveles
levels(nivel_edu)



## EXTRA

# grafico
plot(nivel_edu, 
     main = "Nivel Educativo")

nivel_edu <- fct_relevel(nivel_edu, c("Menos de 6 años", "Entre 6 y 12 años", "Más de 12 años"))

plot(nivel_edu, 
     main = "Nivel Educativo")
