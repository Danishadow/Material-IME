#Diego Alvarado 20.283.543-0
#Daniel Jara 20.113.716-0

#Se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}


if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(pwr)){
  install.packages("pwr", dependencies = TRUE )
  require (pwr)
}
 

# Enunciado
# Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con
# medidas de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de
# fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamaño 25, un ingeniero quiere averiguar
# si una línea de producción está generando barras con dureza media de 170 [kgf mm^-2]

#variables del enunciado
desEstandar <- 10
n_muestra <- 25
mu_muestra <- 170

#****************** PREGUNTA 1 ************************
#Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media menor a 166 [kgfmm-2] o mayor a 174
#[kgf mm-2], ¿cuál es la probabilidad de que cometa un error de tipo 1?

#La probabilidad de cometer un error tipo I corresponde a alfa (nivel de significativa), por lo que se calculará.

#Se establecen las medias nulas (para las pruebas de hipótesis)
media_nula1 <- 166
media_nula2 <- 174
#Se procede a generar un modelo normal
SE <- desEstandar / sqrt(n_muestra)
x <- seq (80 * SE, 90 * SE, 0.01)
y <- dnorm (x, mean = mu_muestra, sd = SE)
g <- ggplot (data = data.frame(x, y), aes(x))
g <- g + stat_function(fun = dnorm,
                       args = list(mean = mu_muestra, sd = SE),
                       colour = "red", size = 1)
g <- g + ylab ("")
g <- g + scale_y_continuous (breaks = NULL)
g <- g + scale_x_continuous (name = "Ejercicio 1",
                             breaks = seq (75 * SE, 90 * SE, 2))
g <- g + theme_pubr()
#print(g)

#Se identifican los valores menores a 166
g <- g + geom_area(data = subset(g$data, x < media_nula1),
                   aes (y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

#Se identifican los valores mayores a 174
g <- g + geom_area(data = subset(g$data, x > media_nula2),
                   aes (y = y),
                   colour = "red",
                   fill = "green",
                   alpha = 0.5)

#Se muestra el gráfico
print(g)

#Cantidad de observaciones bajo la media_nula1 (166)
bajo_mediaN <- subset(g$data, x < media_nula1)

#Cantidad de observaciones sobre la media_nula2 (174)
sobre_mediaN <- subset(g$data, x > media_nula2)

#Se realiza una regla de 3 para calcular la probabilidad de estar en los sectores indicados
prob_bajo <- length(bajo_mediaN$x)/length(x)
prob_sobre <- length(sobre_mediaN$x)/length(x)
#Se calcula la probabilidad total (ambos por separado corresponden a alfa/2 al ser una prueba bilateral)
prob_total <- prob_bajo + prob_sobre

#Respuesta: la probabilidad de cometer un error tipo I cuando la muestra presente una media menor a 166 [kgf mm^-2] o mayor
#           a 174 [kgf mm^-2] es de 0.5997 aproximadamente.





#****************** PREGUNTA 2 ************************
#Si la verdadera dureza media de la línea de producción fuera 173 [kgf mm^-2], ¿cuál sería la probabilidad de
#que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?


#La probabilidad de cometer un error tipo II corresponde a beta, por lo que se calculará.

#Se calcula el poder
media_efecto <- 173
poder <- pnorm(media_nula2,
               mean = media_efecto,
               sd = SE,
               lower.tail = FALSE) + pnorm(media_nula1,
                                           mean = media_efecto,
                                           sd = SE,
                                           lower.tail = TRUE)

#Se calcula beta (probabilidad de cometer error tipo II)
beta <- 1 - poder
#Respuesta: la probabilidad de cometer un error tipo II si la verdadera dureza media fuera 173 [kgf mm^-2] es de
#           0.6912 aproximadamente





#****************** PREGUNTA 3 ************************
# Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las
# condiciones anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 162 a 178
# [kgf mm^-2].

#Se crea nuevamente el gráfico de la pregunta 1 pero con mayor rango
SE <- desEstandar / sqrt(n_muestra)
x <- seq (75 * SE, 95 * SE, 0.01)
y <- dnorm (x, mean = mu_muestra, sd = SE)
g3 <- ggplot (data = data.frame(x, y), aes(x))
g3 <- g3 + stat_function(fun = dnorm,
                       args = list(mean = mu_muestra, sd = SE),
                       colour = "red", size = 1)
g3 <- g3 + ylab ("")
g3 <- g3 + scale_y_continuous (breaks = NULL)
g3 <- g3 + scale_x_continuous (name = "Ejercicio 3",
                             breaks = seq (75 * SE, 95 * SE, 2))
g3 <- g3 + theme_pubr()

#Se identifican los valores menores a 166
g3 <- g3 + geom_area(data = subset(g3$data, x < media_nula1),
                   aes (y = y),
                   colour = "red",
                   fill = "red",
                   alpha = 0.5)

#Se identifican los valores mayores a 174
g3 <- g3 + geom_area(data = subset(g3$data, x > media_nula2),
                   aes (y = y),
                   colour = "red",
                   fill = "green",
                   alpha = 0.5)

#Se procede a agregar los nuevos gráficos
media_3 <- 162
media_4 <- 178
#para 162
g3 <- g3 + stat_function(fun = dnorm,
                       args = list (mean = media_3, sd = SE),
                       colour = "blue", size = 1)
x1 <- seq (75 * SE, 95 * SE, 0.01)
y1 <- dnorm (x, mean = media_3 , sd = SE)

g3 <- g3 + geom_area (data = subset (data.frame (x1, y1),
                                   x < media_3),
                    aes ( x = x1 , y = y1 ),
                    colour = " blue ",
                    fill = " blue ",
                    alpha = 0.5)
g3 <- g3 + geom_area (data = subset (data.frame (x1, y1) ,
                                   x > media_4 ),
                    aes(x = x1 , y = y1) ,
                    colour = " blue ",
                    fill = " blue ",
                    alpha = 0.5)


#para 178
g3 <- g3 + stat_function(fun = dnorm,
                       args = list (mean = media_4, sd = SE),
                       colour = "blue", size = 1)
x2 <- seq (75 * SE, 95 * SE, 0.01)
y2 <- dnorm (x, mean = media_4 , sd = SE)
g3 <- g3 + geom_area (data = subset (data.frame (x2, y2),
                                   x < media_3),
                    aes ( x = x2 , y = y2 ),
                    colour = " blue ",
                    fill = " blue ",
                    alpha = 0.5)
g3 <- g3 + geom_area (data = subset (data.frame (x2, y2) ,
                                   x > media_4),
                    aes(x = x2 , y = y2) ,
                    colour = " blue ",
                    fill = " blue ",
                    alpha = 0.5)


#Se muestra el gráfico
print(g3)


#Gráfico "alternativo" sobre poder estadístico
efecto <- seq(-8, 8, 0.01)
poder_efecto <- power.t.test ( n = n_muestra ,                             
                               delta = efecto ,                            
                               sd = desEstandar ,                             
                               sig.level = prob_total,   
                               power = NULL,
                               type = "one.sample",                            
                               alternative = "two.sided")

datos <- data.frame(efecto,poder_efecto$power)
g3_alt <- ggplot(datos, aes(efecto, poder_efecto$power))
g3_alt <- g3_alt + geom_line ()
g3_alt <- g3_alt + ylab ("Poder estadístico ")
g3_alt <- g3_alt + xlab ("Tamaño del efecto ")
g3_alt <- g3_alt + theme_pubr()
g3_alt <- g3_alt + ggtitle("Curva de poder para prueba bilateral")
g3_alt <- g3_alt + geom_vline (xintercept = 0, linetype = "dashed")
#se imprime el gráfico
print(g3_alt)





#****************** PREGUNTA 4 ************************
# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,80 y un nivel de significación
# de 0,05?

#Se calculará el tamaño de la muestra utilizando la función power.t.test, ingresando los parámetros entregados.

n_4 <- power.t.test ( n = NULL ,                             
                            delta = 173-170 ,                            
                            sd = desEstandar,                             
                            sig.level = 0.05 ,                             
                            power = 0.8 ,                             
                            type = "one.sample",                            
                            alternative = "two.sided")

#Respuesta: Para conseguir un poder estadístico de 0.80 y un nivel de significación de 0.05 se deberían revisar 90 barras
#           (se aproximó de 89.1) 





#****************** PREGUNTA 5 ************************
#¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

#Se calculará el tamaño de la muestra utilizando la función power.t.test, ingresando los parámetros entregados.

n_5 <- power.t.test ( n = NULL ,                             
                      delta = 173-170 ,                            
                      sd = desEstandar ,                             
                      sig.level = 0.01 ,                             
                      power = 0.8 ,                             
                      type = "one.sample",                            
                      alternative = "two.sided")

#Respuesta: Para conseguir un poder estadístico de 0.80 y un nivel de significación de 0.01 se deberían revisar 133 barras



