##----------------------------------------------------------------------------##
#                                                                              #
#    Medición y Diseño 2024 | Carneiro y Méndez                                #
#    EJEMPLO PRACTICO TEOREMA CENTRAL DEL LIMITE                               #
#                                                                              #
##----------------------------------------------------------------------------##

################

### CON DISTRIBUCION NORMAL ----

# Configuración
set.seed(123) # Para reproducibilidad

# Paso 1: Crear una población con distribución normal
media_poblacional <- 50
desviacion_estandar_poblacional <- 10
poblacion <- rnorm(100000, mean = media_poblacional, sd = desviacion_estandar_poblacional) # Distribución normal

# Graficar la distribución de la población
hist(poblacion, probability = TRUE, main = "Distribución normal de la Población",
     xlab = "Valores", col = "skyblue", breaks = 30)

# Curva normal
curve(dnorm(x, mean = media_poblacional, sd = desviacion_estandar_poblacional), 
      col = "red", lwd = 2, add = TRUE)

abline(v = mean(poblacion), col = "blue", lwd = 2, lty = 2) # Media de la población

legend("topright", legend = c("Curva Normal Teórica", "Media Poblacional"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)

text(x = media_poblacional, y = 0.03, labels = paste("Media Poblacional =", media_poblacional), col = "blue", pos = 4)

# Paso 2: Tomar múltiples muestras y calcular sus medias

# Parámetros
n_muestras <- 2000  # Número de muestras
tamano_muestra <- 2000  # Tamaño de cada muestra

medias_muestrales <- numeric(n_muestras) # Vector para almacenar las medias muestrales

for (i in 1:n_muestras) {
  muestra <- sample(poblacion, tamano_muestra, replace = FALSE)
  medias_muestrales[i] <- mean(muestra)
}


medias_dataframe <- data.frame(Muestra = 1:n_muestras, Media = medias_muestrales)

# Paso 3: Graficar la distribución de las medias muestrales
hist(medias_muestrales, breaks = 30, probability = TRUE, 
     main = "Distribución de las Medias Muestrales", 
     xlab = "Medias Muestrales", col = "skyblue")

# Superponer una curva normal teórica
curve(dnorm(x, mean = mean(medias_muestrales), sd = sd(medias_muestrales)), 
      col = "red", lwd = 2, add = TRUE)

# Mostrar la media de la población y la media de las medias muestrales
abline(v = mean(poblacion), col = "blue", lwd = 2, lty = 2) # Media de la población
abline(v = mean(medias_muestrales), col = "black", lwd = 2, lty = 2) # Media de las medias muestrales

legend("topright", legend = c("Curva Normal Teórica", "Media Poblacional", "Media de Medias Muestrales"),
       col = c("red", "blue", "black"), lty = c(1, 2, 2), lwd = 2)

text(x = media_poblacional, y = 0.03, labels = paste("Media Poblacional =", media_poblacional), col = "blue", pos = 4)
text(x = mean(medias_muestrales), y = 0.20, labels = paste("Media Muestral =", round(mean(medias_muestrales),3)), col = "black", pos = 4)



### CON DISTRIBUCION UNIFORME ----

# Configuración
set.seed(123) # Para reproducibilidad

# Paso 1: Crear una población con distribución uniforme
min_poblacional <- 0
max_poblacional <- 100
poblacion <- runif(1000000, min = min_poblacional, max = max_poblacional) # Distribución uniforme

# Graficar la distribución de la población
hist(poblacion, probability = TRUE, main = "Distribución uniforme de la Población",
     xlab = "Valores", col = "skyblue", breaks = 30)

# Curva uniforme
curve(dunif(x, min = min_poblacional, max = max_poblacional), 
      col = "red", lwd = 2, add = TRUE)

abline(v = mean(poblacion), col = "blue", lwd = 2, lty = 2) # Media de la población

legend("topright", legend = c("Curva Uniforme Teórica", "Media Poblacional"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)

text(x = mean(poblacion), y = 0.001, labels = paste("Media Poblacional =", round(mean(poblacion), 2)), col = "blue", pos = 4)

# Paso 2: Tomar múltiples muestras y calcular sus medias

# Parámetros
n_muestras <- 2000  # Número de muestras
tamano_muestra <- 2000  # Tamaño de cada muestra

medias_muestrales <- numeric(n_muestras) # Vector para almacenar las medias muestrales

for (i in 1:n_muestras) {
  muestra <- sample(poblacion, tamano_muestra, replace = FALSE)
  medias_muestrales[i] <- mean(muestra)
}

medias_dataframe_2 <- data.frame(Muestra = 1:n_muestras, Media = medias_muestrales)


# Paso 3: Graficar la distribución de las medias muestrales
hist(medias_muestrales, breaks = 30, probability = TRUE, 
     main = "Distribución de las Medias Muestrales", 
     xlab = "Medias Muestrales", col = "skyblue")

# Superponer una curva normal teórica
curve(dnorm(x, mean = mean(medias_muestrales), sd = sd(medias_muestrales)), 
      col = "red", lwd = 2, add = TRUE)

# Mostrar la media de la población y la media de las medias muestrales
abline(v = mean(poblacion), col = "blue", lwd = 2, lty = 2) # Media de la población
abline(v = mean(medias_muestrales), col = "black", lwd = 2, lty = 2) # Media de las medias muestrales

legend("topright", legend = c("Curva Normal Teórica", "Media Poblacional", "Media de Medias Muestrales"),
       col = c("red", "blue", "black"), lty = c(1, 2, 2), lwd = 2)

text(x = mean(poblacion), y = 0.02, labels = paste("Media Poblacional =", round(mean(poblacion), 2)), col = "blue", pos = 4)
text(x = mean(medias_muestrales), y = 0.05, labels = paste("Media de Medias Muestrales =", round(mean(medias_muestrales), 3)), col = "black", pos = 4)


## CON DISTRIBUCION GAMMA ----

# Configuración
set.seed(123) # Para reproducibilidad

# Parámetros de la distribución gamma
shape_param <- 2  # Parámetro de forma (alpha)
scale_param <- 2  # Parámetro de escala (theta)

# Crear una población con distribución gamma
poblacion <- rgamma(10000, shape = shape_param, scale = scale_param)

# Graficar la distribución de la población
hist(poblacion, probability = TRUE, main = "Distribución Gamma de la Población",
     xlab = "Valores", col = "skyblue", breaks = 30)

# Superponer la densidad teórica de la distribución gamma
curve(dgamma(x, shape = shape_param, scale = scale_param), 
      col = "red", lwd = 2, add = TRUE)

# Parámetros
n_muestras <- 2000  # Número de muestras
tamano_muestra <- 100  # Tamaño de cada muestra

# Paso 2: Tomar múltiples muestras y calcular sus medias
medias_muestrales <- numeric(n_muestras) # Vector para almacenar las medias muestrales

for (i in 1:n_muestras) {
  muestra <- sample(poblacion, tamano_muestra, replace = FALSE)
  medias_muestrales[i] <- mean(muestra)
}

medias_dataframe_3 <- data.frame(Muestra = 1:n_muestras, Media = medias_muestrales)


# Paso 3: Graficar la distribución de las medias muestrales
hist(medias_muestrales, breaks = 30, probability = TRUE, 
     main = "Distribución de las Medias Muestrales", 
     xlab = "Medias Muestrales", col = "skyblue")

# Superponer una curva normal teórica
curve(dnorm(x, mean = mean(medias_muestrales), sd = sd(medias_muestrales)), 
      col = "red", lwd = 2, add = TRUE)

# Mostrar la media de la población y la media de las medias muestrales
abline(v = mean(poblacion), col = "blue", lwd = 2, lty = 2) # Media de la población
abline(v = mean(medias_muestrales), col = "green", lwd = 2, lty = 2) # Media de las medias muestrales

legend("topright", legend = c("Curva Normal Teórica", "Media Poblacional", "Media de Medias Muestrales"),
       col = c("red", "blue", "green"), lty = c(1, 2, 2), lwd = 2)
