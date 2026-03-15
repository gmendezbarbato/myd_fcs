
# *************************************************************************
## Medición y diseño - FCS UdelaR
## Martín Opertti, Mayo 2023
## Muestreo Aleatorio Simple y Teorema del Límite Central
## *************************************************************************

## Teorema del Límite Central ==============================================

# Esta teorema es la forma en cómo sabemos multiplicar el error standard por 
# 1.96 para intervalos de cofianza de 95%

# Teórema: independientemente de la distribucion de la variable, la 
# distribucion de las madias de muestreos es normal

# Cargar paquetes
library(tidyverse)
library(haven)
library(labelled)
library(srvyr)

rm(list=ls())


# 1.) Variable con distribución normal:

# Simular datos poblacionales
poblacion <- tibble(altura = rnorm(2000000, mean = 170, sd = 10)) 

poblacion_2 <- tibble(altura = rnorm(1000000, mean = 145, sd = 20)) 
poblacion <- bind_rows(poblacion, poblacion_2)
rm(poblacion_2)

# Distribución de altura en la población
ggplot(poblacion, aes(x = altura)) +
  geom_density(fill = "#559575", alpha = .6) +
  geom_vline(xintercept = mean(poblacion$altura), linetype = "longdash") +
  theme_minimal(base_size = 16) +
  labs(title = paste0("Distribución poblacional de altura (media = ",
                      round(mean(poblacion$altura), digits = 1),
                      ")"),
       x = "Altura",
       y = "")

mean(poblacion$altura)

# La distribución poblacional no se asemeja a una distribución normal, veamos
# que pasa con si tomo 2000 muestras (con 2000 observaciones cada una)
# y observo la distribución de la media de esas 2000 muestras

n <- 2000 # cantidad de muestras
x_muestras1000 <- rep(NA, n) # Creo vector vacío

# Llenar vector vacío con la media de cada muestra
for(i in 1:n){
  x_muestras1000[i] = mean(sample(poblacion$altura, 1000))
  print(i)
}

# Creo un "data frame"
x_muestras1000 <- tibble(x_muestras1000) %>% 
  rename(altura = x_muestras1000)

# Distribución de la media de las muestras
ggplot(x_muestras1000, aes(x = altura)) +
  geom_density(fill = "#559575", alpha = .6) +
  geom_vline(xintercept = mean(poblacion$altura), linetype = "longdash") +
  theme_minimal(base_size = 16) +
  labs(title = "Distribución de la media de 2.000 muestras de 1.000 observaciones", 
       x = "Altura",
       y = "")

# Como era de esperar, la distribución de la media de las muestras se asemeja
# también a distribución normal

mean(x_muestras1000$altura)


## * Simulación votos ====================================================

rm(list=ls())

set.seed(716277447)

# Crear data poblacional
datos_votos <- sample(x = c("Frente Amplio", "Partido Nacional", "Partido Colorado", "Otros"),
                      size = 2433364, 
                      replace = TRUE, 
                      prob = c(.393, .288, .124, .195)
                      )

datos_votos <- tibble(voto = datos_votos)

prop.table(table(datos_votos$voto))


## Una muestra de 1000 casos ----
m_1000 <- sample_n(datos_votos, 1000)

prop.table(table(m_1000$voto))

# Ahora calcularemos el margen de error e intervalo de confianza para esta muestra
m_1000_sum <- m_1000 %>% 
  group_by(voto) %>% 
  summarise(n = n()) %>% 
  mutate(
    prop = n/sum(n), # Proporción de cada categoría
    moe = (qnorm(0.975) * sqrt(prop*(1-prop)/1000)), # margen de error al 95% confianza    ci_inf =  prop - moe, # Intervalo inferior
    ci_inf =  prop - moe, # Intervalo superior
    ci_sup =  prop + moe # Intervalo superior
  ) %>% 
  mutate(voto = factor(voto, 
                            levels = c("Frente Amplio",
                                       "Partido Nacional",
                                       "Partido Colorado",
                                       "Otros")))

ggplot(m_1000_sum, aes(x = voto, y = prop, color = voto)) +
  geom_point(size = 4) +
  geom_text(aes(label = prop*100), hjust = 1.5) +
  theme_minimal(base_size = 16) +
  labs(title = "Estimación muestra puntual (N = 1000)",
       x = "",
       y = "") +
  scale_color_manual(values = c("#0E6655", "#2E86C1", "#922B21", "#616A6B")) +
  theme(legend.position = "none") +
  ylim(0, .5) 

ggplot(m_1000_sum, aes(x = voto, y = prop, color = voto)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_inf, ymax = ci_sup), width = .2, lwd = .75) +
  geom_text(aes(label = prop*100), hjust = 1.5) +
  geom_text(aes(label = round(ci_sup*100, digits = 1)), vjust = -2, hjust = -1) +
  geom_text(aes(label = round(ci_inf*100, digits = 1)), vjust = 2, hjust = -1) +
  theme_minimal(base_size = 16) +
  labs(title = "Estimación muestra puntual (N = 1000) con intervalo de confianza",
       x = "",
       y = "") +
  scale_color_manual(values = c("#0E6655", "#2E86C1", "#922B21", "#616A6B")) +
  theme(legend.position = "none") +
  ylim(0, .5) 

datos_votos_sum <- datos_votos %>% 
  group_by(voto) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(m_1000_sum, aes(x = voto, y = prop, color = voto)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_inf, ymax = ci_sup), width = .2, lwd = .75) +
  geom_text(aes(label = prop*100), hjust = 1.5) +
  geom_text(aes(label = round(ci_sup*100, digits = 1)), vjust = -2, hjust = -1) +
  geom_text(aes(label = round(ci_inf*100, digits = 1)), vjust = 2, hjust = -1) +
  theme_minimal(base_size = 16) +
  labs(title = "Estimación muestra puntual (N = 1000) con intervalo de confianza",
       x = "",
       y = "") +
  scale_color_manual(values = c("#0E6655", "#2E86C1", "#922B21", "#616A6B")) +
  theme(legend.position = "none") +
  ylim(0, .5) +
  geom_point(data = datos_votos_sum %>% 
               mutate(voto = factor(voto, 
                                    levels = c("Frente Amplio",
                                               "Partido Nacional",
                                               "Partido Colorado",
                                               "Otros"))), aes(x = voto, y = prop), color = "black", shape = 4, size = 5, stroke = 2) 
  



## 100 Muestras de 1000 casos ----

set.seed(17)

# Crear vector de tamaño 100 pero vacío
n <- 100
x_muestras1000 <- list(NA)

# Llenar vector vacío con la media de cada muestra
for(i in 1:n){
  
  x_muestras1000[i] <- tibble(table(sample(datos_votos$voto, 1000)) / 1000)
  
  print(i)
}

x_muestras1000 <- bind_rows(x_muestras1000) 

# Manipulación y gráficos
x_muestras1000 <- x_muestras1000 %>% 
  pivot_longer(everything(),
               names_to = "voto",
               values_to = "valor")

# Comparación 100 muestras vs muestra original 
ggplot() +
  geom_point(data = m_1000_sum, aes(x = voto, y = prop, color = voto), size = 4) +
  # geom_errorbar(data =  m_1000_sum, aes(x = voto, ymin = ci_inf, ymax = ci_sup, color = voto),
  #               width = .2, lwd = .75) +
  geom_jitter(data = x_muestras1000, aes(x = voto, y = valor, color = voto), 
              alpha = .2, size = 2, width = .3) +
  theme_minimal(base_size = 16) +
  labs(title = "Estimación muestra puntual (N = 1000) con intervalo de confianza",
       x = "",
       y = "") +
  scale_color_manual(values = c("#0E6655", "#2E86C1", "#922B21", "#616A6B")) +
  theme(legend.position = "none")

# Agregamos parámetro 
datos_votos_sum <- datos_votos %>% 
  group_by(voto) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot() +
  geom_point(data = datos_votos_sum %>% 
               mutate(voto = factor(voto, 
                                    levels = c("Frente Amplio",
                                               "Partido Nacional",
                                               "Partido Colorado",
                                               "Otros"))), aes(x = voto, y = prop), shape = 4, size = 5, stroke = 2) +
  geom_point(data = m_1000_sum, aes(x = voto, y = prop, color = voto), size = 4) +
  # geom_errorbar(data =  m_1000_sum, aes(x = voto, ymin = ci_inf, ymax = ci_sup, color = voto),
  #               width = .2, lwd = .75) +
  geom_jitter(data = x_muestras1000, aes(x = voto, y = valor, color = voto),
              alpha = .2, size = 2, width = .3) +
  theme_minimal(base_size = 16) +
  labs(title = "Estimación muestra puntual (N = 1000) con intervalo de confianza",
       x = "",
       y = "") +
  scale_color_manual(values = c("#0E6655", "#2E86C1", "#922B21", "#616A6B")) +
  theme(legend.position = "none")


# Cuántas de las 100 muestras obtenidas incluyen al parámetro dentro de su intervalo?

x_muestras1000 <- x_muestras1000 %>%
  rename(prop = valor) %>% 
  mutate(
    moe = (qnorm(0.975) * sqrt(prop*(1-prop)/1000)), # margen de error al 95% confianza
    ci_inf =  prop - moe, # Intervalo inferior
    ci_sup =  prop + moe # Intervalo superior
  ) 

x_voto_c <- x_muestras1000 %>% 
  filter(voto == "Frente Amplio") %>% 
  arrange(prop) %>% 
  mutate(id = row_number(),
         parametro = 0.393,
         color = case_when(
           as.numeric(ci_sup)  < .393 ~ "Fuera",
           as.numeric(ci_inf)  > .393 ~ "Fuera",
           TRUE ~ "Dentro"
         )) %>% 
  mutate(color = as.factor(color))

ggplot() +
  geom_point(data = x_voto_c, 
             aes(x = id, y = prop, color = color), size = 2) +
  geom_errorbar(data =  x_voto_c,
                aes(x = id, ymin = ci_inf, ymax = ci_sup, color = color),
                width = .3, lwd = .5) +
  geom_hline(yintercept = 0.393) +
  theme_minimal(base_size = 16) +
  labs(title = "Intervalo de confianza de 100 muestras (N = 1000) y parámetro poblacional (Frente Amplio)",
       x = "",
       y = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 


## ¿Qué pasa si utilziamos muestras de 100 casos?

## 100 Muestras de 1000 casos ----

# Crear vector de tamaño 100 pero vacío
n <- 100
x_muestras100 <- list(NA)

# Llenar vector vacío con la media de cada muestra
for(i in 1:n){
  
  x_muestras100[i] <- tibble(table(sample(datos_votos$voto, 100)) / 100)
  
  print(i)
    
}

x_muestras100 <- bind_rows(x_muestras100) 

x_muestras100 <- x_muestras100 %>% 
  pivot_longer(everything(),
               names_to = "voto",
               values_to = "valor")

x_muestras100 <- x_muestras100 %>%
  rename(prop = valor) %>% 
  mutate(
    moe = (qnorm(0.975) * sqrt(prop*(1-prop)/100)), # margen de error al 95% confianza
    ci_inf =  prop - moe, # Intervalo inferior
    ci_sup =  prop + moe # Intervalo superior
  ) 

x_voto_c_2 <- x_muestras100 %>% 
  filter(voto == "Frente Amplio") %>% 
  arrange(prop) %>% 
  mutate(id = row_number(),
         parametro = 0.393,
         color = case_when(
           as.numeric(ci_sup)  < 0.393 ~ "Fuera",
           as.numeric(ci_inf)  > 0.393 ~ "Fuera",
           TRUE ~ "Dentro"
         )) %>% 
  mutate(color = as.factor(color))

ggplot() +
  geom_point(data = x_voto_c_2, 
             aes(x = id, y = prop, color = color), size = 2) +
  geom_errorbar(data =  x_voto_c_2,
                aes(x = id, ymin = ci_inf, ymax = ci_sup, color = color),
                width = .3, lwd = .5) +
  geom_hline(yintercept = 0.4) +
  theme_minimal(base_size = 20) +
  labs(title = "Intervalo de confianza de 100 muestras (N = 100) y parámetro poblacional (Frente Amplio)",
       x = "",
       y = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 


df_final <- rbind(mutate(x_voto_c, muestra = 1000),
                  mutate(x_voto_c_2, muestra = 100)) %>% 
  mutate(muestra = case_when(
    muestra == 100 ~ paste0(muestra, " casos. Margen de error ± 19%"),
    muestra == 1000 ~ paste0(muestra, " casos. Margen de error ± 6%")
  ))

ggplot() +
  geom_point(data = df_final, 
             aes(x = id, y = prop, color = color), size = 2) +
  geom_errorbar(data =  df_final,
                aes(x = id, ymin = ci_inf, ymax = ci_sup, color = color),
                width = .3, lwd = .5) +
  geom_hline(yintercept = 0.4) +
  theme_minimal(base_size = 20) +
  labs(title = "Intervalo de confianza de 100 muestras (N = 100) y parámetro poblacional (Frente Amplio)",
       x = "",
       y = "") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  facet_wrap(~muestra)




