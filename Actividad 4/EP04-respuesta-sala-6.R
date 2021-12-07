# Diego Alvarado 20.283.543-0
# Daniel Jara 20.113.716-0


#Se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(TeachingDemos)){
  install.packages("TeachingDemos", dependencies = TRUE )
  require (TeachingDemos)
}



#**************************** Pregunta 1 ***************************************
# El artículo Automatic Segmentation of Medical Images Using Image Registration: Diagnostic and
# Simulation Applications” (Journal of Medical Engeeniering and Technology 2005) propuso una nueva
# técnica para la identificación automática de los bordes de estructuras significativas en una imagen médica
# utilizando desplazamiento lineal promedio (ALD, por sus siglas en inglés). El artículo dio las siguientes
# observaciones de ALD con una muestra de 49 riñones (en pixeles y usando punto en vez de coma
#                                                     decimal):
# 1.38 1.28 1.09 1.07 0.96 1.28 0.91 1.49 1.11 0.66 1.14 1.13 0.91 0.94 1.30
# 0.87 0.73 0.92 1.00 1.05 1.12 1.10 0.95 1.29 0.86 0.96 0.94 1.45 1.12 1.06
# 0.71 0.88 0.96 1.14 1.03 0.89 0.81 1.04 1.15 0.75 1.12 1.01 1.11 0.64 1.25
# 0.68 1.44 1.28 1.21
# Los autores comentaron que el ALD medio sería menor a un 1.0 pixel. ¿Los datos soportan esta
# afirmación?


#RESPUESTA:

#Se cargan los datos
texto <- "1.38 1.28 1.09 1.07 0.96 1.28 0.91 1.49 1.11 0.66 1.14 1.13 0.91 0.94 1.30 0.87 0.73 0.92 1.00 1.05 1.12 1.10 0.95 1.29 0.86 0.96 0.94 1.45 1.12 1.06 0.71 0.88 0.96 1.14 1.03 0.89 0.81 1.04 1.15 0.75 1.12 1.01 1.11 0.64 1.25 0.68 1.44 1.28 1.21"
file <- textConnection(texto)
datos <- scan(file)

#Se empleará la prueba Z, y cumple con las condiciones. A continuación se demuestran.

# 1. Tamaño de la muestra: es mayor a 30, en este caso son 49
# 2. Observaciones independientes: son independiente porque son imágenes médicas escogidas de manera automática (aleatoria).
# 3. Sigue aproximadamente una distribución normal: para esto se realizará el gráfico Q-Q
datosQQ <- data.frame(datos)
g <- ggqqplot(datosQQ, x="datos", color="SteelBlue")
print(g)
#Como con el gráfico presenta valores atípicos, se procede a realizar la prueba de Shapiro-Wilk
pruebaN <- shapiro.test(datos)
print(pruebaN)
#Con ambos datos podemos concluir dos cosas: Los datos siguen aproximadamente una distribución normal 
#(el valor de p es demasiado alto) y que tendremos que utilizar un nivel de significación alto (0.01) debido a
#la situación presentada en el gráfico.

#Se calcula el promedio de la muestra y la desviación estándar
promDatos <- mean(datos)
desvEst <- sd(datos)
valorNulo <- 1
#Nivel de significancia
alfa1 = 0.01
#Formulación de hipótesis
#H0: El ALD medio es igual a 1.0 pixel
#Ha: El ALD medio es menor a 1.0 pixel 

#Se realiza la prueba Z
p1 <- z.test(promDatos, mu = valorNulo, alternative = "less", stdev = desvEst, conf.level = 1-alfa1)
print(p1)

#Se obtiene un valor de p igual a 0.5842, esto es mayor que nuestro nivel de significación igual a 0.01

#Resultado: se falla en rechazar la hipótesis nula (H0), esto quiere decir que los datos no soportan la
#afirmación realiza por los autores (ALD medio es menor a 1.0 pixel).





#**************************** Pregunta 2 ***************************************
# Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar cantidades de calcio
# adecuadas para la producción de leche. Un estudio intenta determinar si madres adolescentes podían
# recuperar niveles más normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-
# 1326). El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
# (en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
# postparto) y posterior a ella (12-30 semana postparto):

#  (añadir tabla)

#¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por más de 90 g?


#Se cargan los datos
texto2 <- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"
file2 <- textConnection(texto2)
lactancia <- scan(file2)

texto2_1 <- "2126 2885 2895 1942 1750 2184 2164 2626 2006 2627"
file2_1 <- textConnection(texto2_1)
posdestete <- scan(file2_1)

#Se calcula la diferencia entre las muestras
diferencia <- posdestete - lactancia

#Se empleará la prueba T para 2 muestras pareadas, y cumple con las condiciones. A continuación se demuestran.
#1. Observaciones independientes: para poder realizar un estudio de esta magnitud se requiere de sujetos (madres
#   adolescentes) escogidas de manera aleatoria, lo que implica independencia de observaciones.
#2. Distribución cercana a la normal: para esto se realiza la prueba de shapiro
pruebaN2 <- shapiro.test(diferencia)
print(pruebaN2)
#Obteniendo un valor de p igual a 0.1389, por lo que se concluye que se posee una distribución cercana a la normal

#Nivel de significación
alfa2 <- 0.05
valorNulo2 <- 90

#Formulación de hipótesis
#H0: Contenido total mineral en los huesos durante el posdestete excede al de lactancia por 90 g
#Ha: Contenido total mineral en los huesos durante el posdestete excede al de lactancia por más de 90 g

#Se realiza la prueba t
p2 <- t.test (diferencia, alternative = "greater", mu = valorNulo2, conf.level = 1 - alfa2)
print(p2)

#Se obtiene un valor p igual 0.322, esto es mayor que nuestro nivel de significación igual a  0.05

#Resultado: se falla en rechazar la hipótesis nula (H0), esto quiere decir que no existen pruebas suficientes
#para afirmar que el contenido total mineral en los huesos durante el posdestete excede al de lactancia por más
#de 90 g





#**************************** Pregunta 3 ***************************************
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 7ma
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en linaza (linseed) y el basado en caseína (casein).



#Se cargan los datos
datos3 <- chickwts
datos3Casein <- datos3 %>% filter(feed == "casein")
datos3CaseinW <- datos3Casein %>% select(weight)

datos3Linseed <- datos3 %>% filter(feed == "linseed")
datos3LinseedW <- datos3Linseed %>% select(weight)


#Se empleará la prueba T para 2 muestras independientes, y cumple con las condiciones. A continuación se demuestran.
#1. Observaciones independientes: esto se cumple debido a que en el enunciado se menciona que se realiza un
#   experimento para medir la efectividad de varios suplementos alimenticios en la tasa de crecimiento de las aves,
#   separando pollitos recién nacidos en 6 grupos aleatorios y a cada grupo se le dio un suplemento distinto.
#2. Distribución cercana a la normal: para esto se realiza la prueba de shapiro
pruebaN3_1 <- shapiro.test(datos3CaseinW$weight)
print(pruebaN3_1)

pruebaN3_2 <- shapiro.test(datos3LinseedW$weight)
print(pruebaN3_2)
#Obteniendo valores p iguales a 0.2592 (para caseína) y 0.9035 (para linaza), por lo que se concluye que se posee
#una distribución cercana a la normal

#Formulación de hipótesis
#H0: No existe diferencia en la efectividad entre el suplemento basado en linaza y el basado en caseína
#Ha: Existe diferencia en la efectividad entre el suplemento basado en linaza y el basado en caseína

#Se establece un nivel de significación
alfa3 <- 0.05
p3 <- t.test (x = datos3CaseinW, y = datos3LinseedW, paired = FALSE, alternative = "two.sided", 
              mu = 0, conf.level = 1 - alfa3)
print(p3)

#Se obtiene un valor p igual 0.0002606, esto es menor que nuestro nivel de significación igual a  0.05

#Resultado: se rechaza la hipótesis nula (H0) en favor de la hipótesis alternativa (Ha), esto quiere decir que
#existe evidencia suficiente para afirmar hay una diferencia en la efectividad entre el suplemento basado en
#linaza y el basado en caseína.




  