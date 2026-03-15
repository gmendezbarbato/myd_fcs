##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    EJERCICIOS R  PARCIAL 1. | 8 de mayo 2024                                 #
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


# 2. Sumar los vectores num1 y num2 y asignar el resultado al objeto sum.


# 3. Multiplicar los vectores num1 y num2 y asignar el resultado al objeto product.


# 4. Crear un vector de de tipo character con 4 elementos y asignarlo a un objeto de nombre vec1.


# 5. Convertir el objeto vec1 en factor y asignarlo al objeto fact1.



# EJERCICIO 2: DATA FRAMES ----

#(Máximo 15 puntos | 1 punto por respuesta correcta | sin detracción)
# 
# 1. Descargar la base de datos "data.xlsx" disponible en esta sección y asignarla a un objeto de nombre data.


# 2. Explorar la base de datos data utilizando al menos tres de las funciones vistas en clase.



# 3. Calcular la media de la variable gdpPercap.


# 4. Calcular la mediana y la desviación estandar de la variable gdpPercap.

 
# 5. Filtrar la base de datos data para quedarse con los casos de países del continente americano y asignarla al objeto data_america.



# 6. En el objeto data_america seleccionar las variables country, year, lifeExp, pop, gdpPercap y asignarla al objeto data_america.



# 7. Partiendo de la base de datos del objeto data (bae de datos inicial), realizar los puntos 5 y 6 en un sólo bloque de código utilizando el pipe y asignar el resultado al objeto data_america2.



# 8. Verificar si los objetos data_america y data_america2 son idénticos.



# 9. En el objeto data_america crear en una nueva variable de nombre pbi a partir de las variables gdpPercap y pop.



# 10. En el objeto data_america crear en una nueva variable de nombre pbi_recodificada recodificando la variable pbi de la siguiente manera: 
# - alto más de X
# - medio entre X y X
# - bajo menos de X



# 11. En el objeto data_america calcular una tabla de frecuencias absoluta (proporción y porcentaje) de la variable gdpPercap.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data. 



# 12. En el objeto data_america calcular una tabla de frecuencias relativa (proporción y porcentaje) de la variable gdpPercap.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data. 



# 13. En el objeto data_america, relizar un gráfico de dispersión de las variables gdpPercap y lifeExp.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data. 



# 14. En el objeto data_america, realizar un histograma de la variable gdpPercap.
# Nota: En caso de no haber creado el objeto data_america, trabaje con la base inicial del objeto data. 



# 15. Exportar el objeto data_america en formato .csv.

