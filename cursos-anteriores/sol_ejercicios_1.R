##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    SOLUCION EJERCICIOS CLASE 1. INTRODUCCIÓN A R Y RSTUDIO                   #
#                                                                              #
##----------------------------------------------------------------------------##

## EJERCICIO VECTORES ----

# (1) Crear un vector de nombre num que contenga los números del 1 al 10 

num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

num <- c(1:10) # más fácil

num <- seq(1, 10, 1) # otra forma


# (2) Aplicar al menos una operación de suma, resta, multiplicación y división al vector num 
# y guardar los resultados en cuatro objetos diferentes

num_sum <- num + 1

num_rest <- num - 1

num_mult <- num * 5

num_div <- num / 2

# (3) Listar los objetos creados en el ambiente (Global Environment)

ls()

# (4) Eliminar todos los objetos creados 

rm(num)
rm(num_rest)
rm(num_mult)
rm(num_div)

rm(list = ls()) # más simple: remuevo la lista con todos los objetos del environment (ls)


# (5) Crear un vector con la edad de los integrantes de tu hogar

edades <- c(39, 32, 7, 1)

# (6) Realizar una operación sobre ese vector para calcular la edad de cada integrante en 2030

edades_2030 <- edades + 2030 - 2024

edades_2030 <- edades + (2030-2024) # lo mismo

edades_2030 <- edades + 6 # una aleternativa menos elegante



## EJERCICIO DATA FRAMES ----

# (1) Crear la tabla de datos de la imagen de arriba en R usando la función data.frame()

tabla <- data.frame(departamento = c("Montevideo", "Colonia", "Rivera"),
                    nombre = c("Carolina Cosse", "Carlos Moreira", "Richard Sander"),
                    partido = c("Frente Amplio", "Nacional", "Colorado"),
                    edad = c(62, 77, 59))

tabla # chequeo


# (2) Agregar una variable con el sexo de cada candidato

tabla$sexo <- c("Femenino", "Masculino", "Masculino")

tabla # chequeo


# otra forma
sexo <- c("Femenino", "Masculino", "Masculino") #creo un vector con los datos
tabla[ ,5] <- sexo # agrego el vector sexo al data.frame utilizando la indexación [ , ] para indicar que es la columna 5
colnames(tabla)[5] <- "sexo" # renombro la columna con el nombre de la variable sexo

tabla # chequeo

# (3) Convertir en factor la variable partido

tabla$partido <- as.factor(tabla$partido)

class(tabla$partido) # chequeo que haya convertido a factor
