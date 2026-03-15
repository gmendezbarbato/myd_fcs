library(forcats)
library(tidyverse)
library(rio)


# 0. Importo datos


data_lapop <- import("data/md_lapop_uruguay_2019.dta") 

glimpse(data_lapop)

# 1. Tabla cruzada o descriptiva entre las dos variables ----

# Tiremos una tabla cruzada y una tabla
# con medias para ver de forma preliminar datos sobre la relación entre las dos 
# variables que queremos estudiar

# 1.1 Tabla cruzada
data_lapop <- data_lapop %>% 
  mutate(l1_cat = case_when(
    l1 <= 4 ~ "Izquierda",
    l1 %in% c(5, 6) ~ "Centro",
    l1 >= 7 ~ "Derecha",
  )) %>% 
  mutate(redist3_cat = case_when(
    redist3 <= 3 ~ "Pro-redistribución",
    redist3 == 4 ~ "Neutro",
    redist3 >= 5 ~ "Anti-redistribución",
  ))

table(data_lapop$l1_cat)
table(data_lapop$redist3_cat)

# 1.2 Transformo a factor y ordeno levels

data_lapop <-  data_lapop %>% 
  mutate(l1_cat = as_factor(l1_cat),
         l1_cat = fct_relevel(l1_cat, "Izquierda", "Centro", "Derecha"))

data_lapop <-  data_lapop %>% 
  mutate(redist3_cat = as_factor(redist3_cat),
         redist3_cat = fct_relevel(redist3_cat,"Pro-redistribución","Neutro", "Anti-redistribución" )
  )
table(data_lapop$l1_cat)
table(data_lapop$redist3_cat)

# 1.3 Tabla con % a nivel de fila
data_lapop %>% 
  count(redist3_cat, l1_cat) %>% 
  mutate(n = n/sum(n)*100, .by = redist3_cat) %>% 
  spread(l1_cat, n)


# 2 Asociaciones entre variables: Chi cuaddrado
# test de chi cuadrado

test <- chisq.test(data_lapop$redist3_cat, data_lapop$l1_cat)
test
test$statistic
test$p.value
test$expected

# 2.1 calculo manual: creo tabla cruzada

t1 <- data_lapop %>% 
  count(redist3_cat, l1_cat) %>% 
  spread(l1_cat, n)
t1

# 2.2 calculo manual: calaculo marginales

f_total <- rowSums(t1[,2:4])

total <- sum(f_total)

c_total <- colSums(t1[,2:4])

# 2.3 Calculo frecuencia esperada

m <- matrix(nrow = 3, ncol=3)

m[1,1] <- (f_total[1])*(c_total[1])/total # fila 1 col 1
m[2,1] <- (f_total[2])*(c_total[1])/total # fila 2 col 1
m[3,1] <- (f_total[3])*(c_total[1])/total # fila 3 col 1

m[1,2] <- (f_total[1])*(c_total[2])/total # fila 1 col 2
m[2,2] <- (f_total[2])*(c_total[2])/total # fila 2 col 2
m[3,2] <- (f_total[3])*(c_total[2])/total # fila 3 col 2

m[1,3] <- (f_total[1])*(c_total[3])/total # fila 1 col 3
m[2,3] <- (f_total[2])*(c_total[3])/total # fila 2 col 3
m[3,3] <- (f_total[3])*(c_total[3])/total # fila 3 col 3

m


#2.4 Tranformo tabla en matriz y caculo chi2

t1m <- t1 %>% 
  select(-redist3_cat) %>% 
  as.matrix() %>% 
  unname() #tranformo en matriz y quito nombres


# Calculo en base a la formula
((t1m[1,1] - m[1,1])^2/m[1,1]) + ((t1m[1,2] - m[1,2])^2/m[1,2]) + ((t1m[1,3] - m[1,3])^2/m[1,3]) +
  ((t1m[2, 1] - m[2,1])^2/m[2,1]) + ((t1m[2,2] - m[2,2])^2/m[2,2]) + ((t1m[2,3] - m[2,3])^2/m[2,3]) +
  ((t1m[3, 1] - m[3,1])^2/m[3,1]) + ((t1m[3,2] - m[3,2])^2/m[3,2]) + ((t1m[3,3] - m[3,3])^2/m[3,3])
