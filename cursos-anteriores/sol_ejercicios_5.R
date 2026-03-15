##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    SOLUCION EJERCICIOS CLASE 5. LABORATORIO METODO COMPARADO                 #
#                                                                              #
##----------------------------------------------------------------------------##

# Paquetes necesarios para todos los ejercicios
library(rio)
library(tidyverse)
library(SetMethods)
library(QCA)


# EJERCICIO 1: ----
## Método de la similitud y de la diferencia ----

# EJERCICIO 1a ----

### __Importar en R la hoja 1 del archivo "data_ej_comparado.xlsx" ----

# importo la base
data_1a <- rio::import("data_ej_comparado.xlsx", which = 1)

# pongo la variable país como nombre de filas, para que no la cuente como variable
data_1a <- data_1a %>% 
  remove_rownames %>% 
  column_to_rownames(var="pais") 

### __P1. Identifique el método ----

# Método de la similitud 

### __P2.  Realice un análisis de necesidad y suficiencia ----

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_1a, data_1a$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_1a <- as.data.frame(QCAfit(data_1a, data_1a$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>% # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_1a) 

# preparo data para análisis de suficiencia
data_analisis_suf_1a <- data_1a %>% 
  filter(X3 == 1) %>% # selecciono las filas con X3 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X3)
  select(Y, X3)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X3 = 1
analisis_suf_1a <- ifelse(nrow(data_analisis_suf_1a) > 0, print("No suficiente"), print("Suficiente"))


### __P3. ¿Cuál es la principal dificultad de este método? ----

# Sesgo de selección de la variable dependiente. Var dep. sin varianza     



# EJERCICIO 1b ----

### __Importar en R la hoja 2 del archivo "data_ej_comparado.xlsx" ----

# importo la base
data_1b <- rio::import("data_ej_comparado.xlsx", which = 2)

data_1b <- data_1b %>% 
  remove_rownames %>% 
  column_to_rownames(var="pais") 

### __P1. Identifique el método ----

# Método de la diferencia indirecta

### __P2.  Realice un análisis de necesidad y suficiencia ----

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_1b, data_1b$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_1b <- as.data.frame(QCAfit(data_1b, data_1b$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>%  # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_1b) 

# preparo data para análisis de suficiencia X3
data_analisis_suf_1b <- data_1b %>% 
  filter(X3 == 1) %>% # selecciono las filas con X3 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X3)
  select(Y, X3)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X3 = 1
analisis_suf_1b <- ifelse(nrow(data_analisis_suf_1b) > 0, print("No suficiente"), print("Suficiente"))


# preparo data para análisis de suficiencia ~X4 (No X4)
data_analisis_suf_1b_2 <- data_1b %>% 
  filter(X4 == 0) %>% # selecciono las filas con X4 = 0 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con ausencia de X4)
  select(Y, X4)  # selecciono únicamente las variables Y y X4

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X4 = 0
analisis_suf_1b_2 <- ifelse(nrow(data_analisis_suf_1b_2) > 0, print("No suficiente"), print("Suficiente"))

### __P3. ¿Cuál es la principal dificultad de este método? ----

# El mayor problema es que no lidia bien con la equifinalidad. 
# ¿Cuál es la respuesta del análisis anterior?
# Si ~X4 es suficiente, pero X3 es necesaria (o no puedo descartar que no lo sea) ¿qué hacer?



# EJERCICIO 2 ----
## Análisis configuracional con QCA (csQCA) ----

### __Importar en R la hoja 3 del archivo "data_ej_comparado.xlsx" ----

# importo la base
data_2 <- rio::import("data_ej_comparado.xlsx", which = 3)

data_2 <- data_2 %>% 
  remove_rownames %>% 
  column_to_rownames(var="pais") 

### __P1. Realice un análisis de necesidad y suficiencia ----

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_2, data_2$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_2 <- as.data.frame(QCAfit(data_2, data_2$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>%  # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_2) 


# preparo data para análisis de suficiencia ~X4 (No X4)
data_analisis_suf_2 <- data_2 %>% 
  filter(X4 == 0) %>% # selecciono las filas con X4 = 0 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con ausencia de X4)
  select(Y, X4)  # selecciono únicamente las variables Y y X4

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X4 = 0
analisis_suf_2 <- ifelse(nrow(data_analisis_suf_2) > 0, print("No suficiente"), print("Suficiente"))

### __P2. Construya una tabla de verdad ----

# Tabla de verdad para analizar configuraciones causales con la función truthTable() del paquete QCA
tabla_de_verdad <- truthTable(data_2, outcome = "Y", show.cases = TRUE, complete = TRUE)
print(tabla_de_verdad)


### __P3. Identifique las configuraciones causales suficientes, las no suficientes (distinguiendo las contradictorias) y los contrafácticos.  ----

# configuraciones causales suficientes
conf_suf <- tabla_de_verdad$tt %>% # seleeciono el primer elemento de la lista de tabla de verdad que es el data.frame tt ("Truth table")
  filter(incl == 1)    

#imprimo
conf_suf

# configuraciones causales no suficientes
conf_no_suf <- tabla_de_verdad$tt %>% # seleeciono el primer elemento de la lista de tabla de verdad que es el data.frame tt ("Truth table")
  filter(incl < 1 & incl != "-")

#imprimo
conf_no_suf

# contrafácticos
conf_contr <- tabla_de_verdad$tt %>% # seleeciono el primer elemento de la lista de tabla de verdad que es el data.frame tt ("Truth table")
  filter(incl == "-")    

#imprimo
conf_contr


### __P4. ¿Qué alternativas sugiere para el tratamiento de los contrafácticos?  ----

# Buscar más casos, casos análogos (en otros lugares o momentos históricos), 
# recurrir a la teoría para descartar algunos, plausibilidad, o reportar ambas alternativas

### __P5. ¿Puede realizarse una minimización lógica para lograr una explicación más parsimoniosa? ----


# minimizacion lógica tratando residuos como falsos (Solucion Compleja)
min_sc <- minimize(tabla_de_verdad, details=TRUE, show.cases=TRUE)
print(min_sc)

# minimizacion lógica tratando residuos como verdaderos (Solucion Parsimoniosa)
min_sp <- minimize(tabla_de_verdad, include="?", details=TRUE, show.cases=TRUE)
print(min_sp)

### __P6. ¿Cuál es el resultado del análisis en términos sustantivos? ----

# Los países con PBI per cápita mayor a USD 6000 y cuya economía no depende del petróleo, son democracias.
# El PBI per cápita y la dependencia del petróleo constituyen condiciones suficientes (aunque pueden no ser necesarias) para explicar la democracia (enfoque INUS).


### __Extra realice el mismo ejercicio con los datos de la hoja 4 del archivo "data_ej_comparado.xlsx"




