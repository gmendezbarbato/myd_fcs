##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    EJERCICIOS REPASO PARCIAL 1.                                              #
#                                                                              #
##----------------------------------------------------------------------------##


# VECTORES ----

# 1. Crear un vector numérico con tres elementos y asignarlo al objeto vec1 

# 2. Crear un vector character que contenga los numeros 1, 2, 3 y asignarlo al objeto vec 2.

# 3. Sumar los vectores vec1 y vec 2 y asignarlos al vector de nombre vec3. 
# Realizar las operaciones necesarias para que los vectores puedan sumarse.


# PROYECTOS ----

# 1. Crear un proyecto en una carpeta existente de nombre repaso_myd

# 2. Crear un proyecto en una carpeta nueva de nombre repaso_myd2


# CARGAR PAQUETES ----

# 1. Cargar paquetes tidyverse y rio


# DESCARGAR BASE DE DATOS ----

# 1. Descargar la basde de datos pob_demo.xlsx de la sección de eva: Repaso primer parcial y colocarla en la carpeta del proyecto dentro de una carpeta de nombre data


# IMPORTAR BASES DE DATOS EN R ----

# 1. Importar pob_demo.xlsx y asignarlo al objeto pob_demo


# EXPLORAR BASES DE DATOS ----

# 1. Explorar la base de datos pob_demo utilizando las siguientes funciones:

# dim()
# names()
# head()
# glimpse()
# str()
# summary()


# FILTRAR Y SLECCIONAR ----

# 1. Filtrar la base y quedarse con los datos que correspondan al continente Sudamerica y asigrnarlas al objeto pob_demo_sud.

# 2. Filtrar nuevamente la base y quedarse con los datos a partir del año 1900 en adelante.

# 3. En el objeto objeto pob_demo_sud, seleccionar con asignación las variables continente, anio, pob, pob_democracias_electorales, pob_democracias_liberales.

 
# PIPE ----

# 1. Realizar los pasos anteriores en un sólo bloque de código utilizando el pipe %>% y asignarlo al objeto  pob_demo_sud2

# 2. Verificar que pob_demo_sud y pob_demo_sud2 sean identicos


#   CREAR NUEVAS VARIABLES ----

# 1. En el objeto pob_demo_sud, crear una nueva variable que contenga el dato de la suma de población que vive en democracias electorales o democracias liberales en sudamerica, denominar esa variable pob_democracias.

# 2. En el objeto pob_demo_sud, crear una nueva variable que contenga el dato del porcentaje de población que vive en democracias (electorales o liberales) en sudamerica, denominar esa variable pob_democracias_porc.


# CREAR UNA VARIABLE RECODIFICANDO OTRA VARIABLE ----

# 1. En el objeto pob_demo_sud, crear una nueva variable de nombre cobertura_democracia a partir de recodificar la variable pob_democracias_por de la siguiente manera:

# - Alta: mayor o igual a 75 
# - Media: entre 50 y 74
# - Baja: entre 25 y 49
# - Muy baja: menor a 25


# TABLAS ----

# 1. A partir del objeto pob_demo_sud, realizar tablas de frecuencias absoluta y relativa (proporción y porcentaje) de la siguiente variable cobertura_democracia: 


# MEDIA ----

# 1. A partir del objeto pob_demo_sud, calcular la media de la variable pob_democracias_por


# GRAFICO ----

# 1. A partir del objeto pob_demo_sud, hacer un gráfico de dispersión de las las variables anio y pob_democracias_por


# EXPORTAR ----

# 1.Exportar el objeto **pob_demo_sud** en excel (.xlsx).
