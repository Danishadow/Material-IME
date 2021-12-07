

#se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

#se cargan los datos en formato csv ingles
#IMPORTANTE: SE ABRIRA UNA VENTANA PARA QUE SELECCIONE EL ARCHIVO
datos <- read.csv(file.choose(), encoding = "UTF-8")

#Se trabajaran con las variables: sexo, edad (ejeX), ytot (ejeY)
filtroSexo <- datos %>% filter(sexo == "Mujer", region == "Región Metropolitana de Santiago")

#------------------------ pregunta sala 6 --------------------------------------

#se realiza el grafico disperso (ingreso vs edad)
#se utilizo el grafico disperso debido a que se trabaja con 2 variables numericas, ademas de que permite 
#visualizar si existe una relacion entre ambas variables

#como medidas estadisticas descartamos las medidas de tendencia central puesto que:
  # 1. existen valores atipicos
  # 2. no entregan informacion relevante para el estudio (pregunta planteada)
#HINT: se busca la relacion entre 2 variables a medida que una de estas evoluciona

graficoDisperso <- ggscatter(filtroSexo,
                             x = "edad",
                             y = "ytot",
                             color = "red",
                             title = "Ingreso vs Edad",
                             xlab = "Edad [años]",
                             ylab = "Ingreso [clp]")
print(graficoDisperso)
  # Respuesta: No, no van en aumento, debido a que:
  # 1. Se percibe un alza en los ingresos a partir de la edad de 20 años
  # 2. Existen valores atipicos que afectan al estudio
  # 3. Se percibe una baja en los ingresos a partir de la edad de jubilacion (60 años aprox)
  # 4. Entre los 20 y 60 años oscilan los valores de ingreso

