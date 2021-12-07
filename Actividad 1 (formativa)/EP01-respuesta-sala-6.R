
# Respuestas
# ¿que variables se han cargado?
#   R: Regiones, cantidad de contagiados con sintomas, fecha
# 
# ¿que tipo tiene cada una de estas variables?
#   R:
#   Region: categorica nominal
#   Cantidad de contagiados con sintomas: numerica discreta
#   Fecha: categorica ordinal
# 
# ¿que escala parecen tener estas variables?
#   R:
#   Region: escala nominal
#   Cantidad de contagiados con sintomas: escala de razon
#   Fecha: escala ordinal o de rangos 


#------------------------------ Codigo ---------------------------------------
#DISCLAIMER: en caso de que el run no funcione, se recomienda ejecutar linea a linea, debido a que el codigo funciona

#se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE )
  require (tidyr)
}


#se cargan los datos en formato csv ingles
#IMPORTANTE: SE ABRIRA UNA VENTANA PARA QUE SELECCIONE EL ARCHIVO
datos <- read.csv(file.choose(), encoding = "UTF-8")

#se filtra por la region Metropolitana (es la unica que debemos usar)
metropolitana <- datos %>% filter(Region == "Metropolitana")

#se alarga la matriz
metropolitanaLarge <- metropolitana %>% pivot_longer(cols = starts_with("X"),
                                                     names_to = "Fecha",
                                                     values_to = "Contagiados",
                                                     values_drop_na = TRUE)
  
#Se filtra entre los meses solicitados (abril - agosto)
metropolitanaFilter <- metropolitanaLarge %>% filter(Fecha >= "X2020.04.01" & Fecha <= "X2020.08.31")

#se filtra por mes
metropolitanaAbril <- metropolitanaLarge %>% filter(Fecha >= "X2020.04.01" & Fecha < "X2020.05.01")
metropolitanaMayo <- metropolitanaLarge %>% filter(Fecha >= "X2020.05.01" & Fecha < "X2020.06.01")
metropolitanaJunio <- metropolitanaLarge %>% filter(Fecha >= "X2020.06.01" & Fecha < "X2020.07.01")
metropolitanaJulio <- metropolitanaLarge %>% filter(Fecha >= "X2020.07.01" & Fecha < "X2020.08.01")
metropolitanaAgosto <- metropolitanaLarge %>% filter(Fecha >= "X2020.08.01" & Fecha <= "X2020.08.31")



#se obtiene la informacion requerrida (total contagiados por mes y el maximo del periodo abril-agosto)
informacion <- metropolitanaFilter %>% summarise(TotalAbril = sum(metropolitanaAbril$Contagiados),
                                                 TotalMayo = sum(metropolitanaMayo$Contagiados),
                                                 TotalJunio = sum(metropolitanaJunio$Contagiados),
                                                 TotalJulio = sum(metropolitanaJulio$Contagiados),
                                                 TotalAgosto = sum(metropolitanaAgosto$Contagiados),
                                                 FechaDelMaximo = Fecha[which.max(Contagiados)],
                                                 Maximo = max(Contagiados))
                                                
#las respuestas a las preguntas de la sala 6 se encuentran en informacion

