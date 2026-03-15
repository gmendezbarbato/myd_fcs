##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    LIVE CODING CLASE 5. LABORATORIO METODO COMPARADO                         #
#                                                                              #
##----------------------------------------------------------------------------##


library(rio)
library(tidyverse)
# install.packages("SetMethods")
library(SetMethods)
# install.packages("QCA")
library(QCA)


# Ejemplos del texto de Perez-Liñan (2010) ----



# METODO SIMILITUD (TABLA 1) ----

# importo la base
data_1 <- rio::import("ej_pl.xlsx", which = 1)

#imprimo
data_1

# pongo la variable caso como nombre de filas, para que no la cuente como variable
data_1 <- data_1 %>% 
  remove_rownames %>% 
  column_to_rownames(var="caso") 

# imprimo
data_1

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_1, data_1$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_1 <- as.data.frame(QCAfit(data_1, data_1$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>% # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_1) 

# preparo data para análisis de suficiencia
data_analisis_suf_1 <- data_1 %>% 
  filter(X1 == 1) %>% # selecciono las filas con X1 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X1)
  select(Y, X1)  # selecciono únicamente las variables Y y X1

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X1 = 1
analisis_suf_1 <- ifelse(nrow(data_analisis_suf_1) > 0, print("No suficiente"), print("Suficiente"))



# PROBLEMAS METODO SIMILITUD (TABLA 2) ----

# importo la base
data_2 <- rio::import("ej_pl.xlsx", which = 2)

#imprimo
data_2

# pongo la variable caso como nombre de filas, para que no la cuente como variable
data_2 <- data_2 %>% 
  remove_rownames %>% 
  column_to_rownames(var="caso") 

#imprimo
data_2

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_2, data_2$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_2 <- as.data.frame(QCAfit(data_2, data_2$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>% # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_2) 


# preparo data para análisis de suficiencia
data_analisis_suf_2 <- data_2 %>% 
  filter(X1 == 1) %>% # selecciono las filas con X1 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X1)
  select(Y, X1)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X1 = 1
analisis_suf_2 <- ifelse(nrow(data_analisis_suf_2) > 0, print("No suficiente"), print("Suficiente"))


# METODO DIFERENCIA INDIRECTA (TABLA 3) ----

# importo la base
data_3 <- rio::import("ej_pl.xlsx", which = 3)

# imprimo
data_3

# pongo la variable caso como nombre de filas, para que no la cuente como variable
data_3 <- data_3 %>% 
  remove_rownames %>% 
  column_to_rownames(var="caso") 

# imprimo
data_3

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_3, data_3$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_3 <- as.data.frame(QCAfit(data_3, data_3$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>% # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_3) 

# preparo data para análisis de suficiencia X1
data_analisis_suf_3 <- data_3 %>% 
  filter(X1 == 1) %>% # selecciono las filas con X1 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X1)
  select(Y, X1)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X1 = 1
analisis_suf_3 <- ifelse(nrow(data_analisis_suf_3) > 0, print("No suficiente"), print("Suficiente"))

# preparo data para análisis de suficiencia no X3
data_analisis_suf_3b <- data_3 %>% 
  filter(X3 == 0) %>% # selecciono las filas con X1 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X1)
  select(Y, X3)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X1 = 1
analisis_suf_3b <- ifelse(nrow(data_analisis_suf_3b) > 0, print("No suficiente"), print("Suficiente"))

# En resumen: por un lado, X1 y ~X3 (No X3) son condiciones necesarias; por otro, sólo ~X3 es suficiente. ¿?
# Es necesario un análisis configuracional


# ANALISIS CONFIGURACIONAL CON QCA (TABLA 6) ----


# importo la base, con argumento which puedo seleccionar diferentes hojas del excel
data_4 <- rio::import("ej_pl.xlsx", which = 4)

data_4 <- data_4 %>% 
  remove_rownames %>% 
  column_to_rownames(var="caso") 

# hago un análisis de necesidad con la función QCAfit del paquete SetMethods
# Pasos:
# LLamar a la función, data, variable dependiente (Y), necessity = TRUE
QCAfit(data_4, data_4$Y, necessity = TRUE)

# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
# Si quiero guardar el resultado en un data frame, me quedo sólo con la columna 1
analisis_nec_4 <- as.data.frame(QCAfit(data_4, data_4$Y, necessity = TRUE)) %>% 
  select(Cons.Nec) %>% # selecciono la columna del análisis de necesidad
  filter(Cons.Nec == 1) %>% # filtro las cons. Nec. consistentes = 1 "Verdaderas"
  filter(row.names(.) != "Y") # elimino de la tabla la variable Y (dep)

print(analisis_nec_4)

# preparo data para análisis de suficiencia
data_analisis_suf_4 <- data_4 %>% 
  filter(X1 == 1) %>% # selecciono las filas con X3 = 1 (presunta variable necesaria)
  filter(Y == 0) %>% # selecciono las filas con Y = 0 (para identificar casos de ausencia de Y, con presencia de X3)
  select(Y, X1)  # selecciono únicamente las variables Y y X3

# testeo una condición con ifelse para saber si hay casos que tengan Y = 0 y X3 = 1
analisis_suf_4 <- ifelse(nrow(data_analisis_suf_4) > 0, print("No suficiente"), print("Suficiente"))


library(QCA)

# Tabla de verdad para analizar configuraciones causales paquete QCA

tabla_de_verdad <- truthTable(data_4, outcome = "Y", show.cases = TRUE, complete = TRUE)
print(tabla_de_verdad)

# minimizacion lógica tratando residuos como falsos (Solucion Compleja)
min_sc <- minimize(tabla_de_verdad, details=TRUE, show.cases=TRUE)
print(min_sc)

# minimizacion lógica tratando residuos como verdaderos (Solucion Parsimoniosa)
min_sp <- minimize(tabla_de_verdad, include="?", details=TRUE, show.cases=TRUE)
print(min_sp)
