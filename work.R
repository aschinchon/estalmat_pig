library(tidyverse)

########################################################################
# Simulamos la evolucion de un jugador que tirar como mucho 5 veces
########################################################################

max_tiradas <- 5
total_puntos <- 0
evolu_puntos <- numeric()

while(total_puntos < 100){
  parcial <- 0
  tiradas <- 1
  while(tiradas <= max_tiradas){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial <- 0
      break
    }else{
      parcial <- parcial + resultado    
      tiradas <- tiradas + 1
      if(total_puntos + parcial >= 100) break
    }
  }
  evolu_puntos <- append(evolu_puntos, parcial)
  total_puntos <- total_puntos + parcial
}


ggplot() +
  geom_line(aes(x = 1:length(evolu_puntos), 
                y = cumsum(evolu_puntos))) +
  scale_x_continuous(breaks=1:length(evolu_puntos))

# La probabilidad de sacar al menos un 1 en 5 tiradas es
# P = 1-(5/6)^5


########################################################################
# ?Es mejor tirar 5 veces o tirar 3? ellos
########################################################################

########################################################################
# Dos jugadores, uno tira 5 y otro tira 3
########################################################################

max_tiradas1 <- 5
max_tiradas2 <- 3

total_puntos1 <- 0
total_puntos2 <- 0

evolu_puntos1 <- numeric()
evolu_puntos2 <- numeric()

while(total_puntos1 < 100 & total_puntos2 < 100){
  
  parcial1 <- 0
  tiradas1 <- 1
  while(tiradas1 <= max_tiradas1){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial1 <- 0
      break
    }else{
      parcial1 <- parcial1 + resultado    
      tiradas1 <- tiradas1 + 1
      if(total_puntos1 + parcial1 >= 100) break
    }
  }
  evolu_puntos1 <- append(evolu_puntos1, parcial1)
  total_puntos1 <- total_puntos1 + parcial1
  
  parcial2 <- 0
  tiradas2 <- 1
  while(tiradas2 <= max_tiradas2){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial2 <- 0
      break
    }else{
      parcial2 <- parcial2 + resultado    
      tiradas2 <- tiradas2 + 1
      if(total_puntos2 + parcial2 >= 100) break
    }
  }
  evolu_puntos2 <- append(evolu_puntos2, parcial2)
  total_puntos2 <- total_puntos2 + parcial2
  
  
}


ganador <- ifelse(total_puntos1 > total_puntos2, 1, 2)
rondas <- ifelse(total_puntos1 > total_puntos2, length(evolu_puntos1), length(evolu_puntos2))

print(paste0("Ha ganado el jugador ", ganador, " en ",rondas , " rondas."))


ggplot() +
  geom_line(aes(x = 1:length(evolu_puntos1), y = cumsum(evolu_puntos1)), col = "red") +
  geom_line(aes(x = 1:length(evolu_puntos2), y = cumsum(evolu_puntos2)), col = "blue") +
  scale_x_continuous(breaks=1:max(length(evolu_puntos1), length(evolu_puntos2)))

# ?Quien crees que gana?

########################################################################
# ?Es esto sufuciente para demostrarlo?
# Hacer muchas simulaciones y un histograma
########################################################################

max_tiradas1 <- 5
max_tiradas2 <- 3

partidas <- 1000
resultados <- tibble(partida = numeric(), ganador = numeric())


for (i in 1:partidas)
{
  total_puntos1 <- 0
  total_puntos2 <- 0
  while(total_puntos1 < 100 & total_puntos2 < 100){
    
    parcial1 <- 0
    tiradas1 <- 1
    while(tiradas1 <= max_tiradas1){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial1 <- 0
        break
      }else{
        parcial1 <- parcial1 + resultado    
        tiradas1 <- tiradas1 + 1
        if(total_puntos1 + parcial1 >= 100) break
      }
    }
    total_puntos1 <- total_puntos1 + parcial1
    
    parcial2 <- 0
    tiradas2 <- 1
    while(tiradas2 <= max_tiradas2){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial2 <- 0
        break
      }else{
        parcial2 <- parcial2 + resultado    
        tiradas2 <- tiradas2 + 1
        if(total_puntos2 + parcial2 >= 100) break
      }
    }
    total_puntos2 <- total_puntos2 + parcial2
  }
  
  
  resultados <- add_row(resultados,
                        partida = i,
                        ganador = ifelse(total_puntos1 > total_puntos2, 1, 2))
  
}

ggplot(resultados) + geom_bar(aes(ganador))


# ?Hay alguna estrategia mejor que 5?

########################################################################
# Ahora el jugador 2 cambia de estrategia intenta superar una cantidad
# fija de puntos en su tirada, digamos 20 puntos
########################################################################

max_tiradas1 <- 5
min_puntos2 <- 20

partidas <- 1000
resultados <- tibble(partida = numeric(), ganador = numeric())


for (i in 1:partidas)
{
  total_puntos1 <- 0
  total_puntos2 <- 0
  while(total_puntos1 < 100 & total_puntos2 < 100){
    
    parcial1 <- 0
    tiradas1 <- 1
    while(tiradas1 <= max_tiradas1){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial1 <- 0
        break
      }else{
        parcial1 <- parcial1 + resultado    
        tiradas1 <- tiradas1 + 1
        if(total_puntos1 + parcial1 >= 100) break
      }
    }
    total_puntos1 <- total_puntos1 + parcial1
    
    parcial2 <- 0
    while(parcial2 <= min_puntos2){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial2 <- 0
        break
      }else{
        parcial2 <- parcial2 + resultado    
        if(total_puntos2 + parcial2 >= 100) break
      }
    }
    total_puntos2 <- total_puntos2 + parcial2
  }
  
  
  resultados <- add_row(resultados,
                        partida = i,
                        ganador = ifelse(total_puntos1 > total_puntos2, 1, 2))
  
}

ggplot(resultados) + geom_bar(aes(ganador))


########################################################################
# Add un tercer jugador que intente superar a los dos anteriores
########################################################################

max_tiradas1 <- 5
min_puntos2 <- 20

total_puntos1 <- 0
total_puntos2 <- 0
total_puntos3 <- 0

evolu_puntos1 <- numeric()
evolu_puntos2 <- numeric()
evolu_puntos3 <- numeric()

while(total_puntos1 < 100 & total_puntos2 < 100 & total_puntos3 < 100){
  
  parcial1 <- 0
  tiradas1 <- 1
  while(tiradas1 <= max_tiradas1){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial1 <- 0
      break
    }else{
      parcial1 <- parcial1 + resultado    
      tiradas1 <- tiradas1 + 1
      if(total_puntos1 + parcial1 >= 100) break
    }
  }
  evolu_puntos1 <- append(evolu_puntos1, parcial1)
  total_puntos1 <- total_puntos1 + parcial1
  
  parcial2 <- 0
  while(parcial2 <= min_puntos2){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial2 <- 0
      break
    }else{
      parcial2 <- parcial2 + resultado    
      if(total_puntos2 + parcial2 >= 100) break
    }
  }
  evolu_puntos2 <- append(evolu_puntos2, parcial2)
  total_puntos2 <- total_puntos2 + parcial2
  
  parcial3 <- 0
  while(parcial3 <= max(parcial1, parcial2)){
    resultado <- sample(1:6, 1)
    if (resultado == 1) {
      parcial3 <- 0
      break
    }else{
      parcial3 <- parcial3 + resultado    
      if(total_puntos3 + parcial3 >= 100) break
    }
  }
  evolu_puntos3 <- append(evolu_puntos3, parcial3)
  total_puntos3 <- total_puntos3 + parcial3
}

ganador <- case_when(
  total_puntos1 >= 100 ~ 1,
  total_puntos2 >= 100 ~ 2,
  total_puntos3 >= 100 ~ 3
)

# ganador <- case_when(
#   max(total_puntos1, total_puntos2, total_puntos3) == total_puntos1 ~ 1,
#   max(total_puntos1, total_puntos2, total_puntos3) == total_puntos2 ~ 2,
#   max(total_puntos1, total_puntos2, total_puntos3) == total_puntos3 ~ 3
# )

rondas <- case_when(
  ganador == 1 ~ length(evolu_puntos1),
  ganador == 2 ~ length(evolu_puntos2),
  ganador == 3 ~ length(evolu_puntos3)
)


print(paste0("Ha ganado el jugador ", ganador, " en ",rondas , " rondas."))


ggplot() +
  geom_line(aes(x = 1:length(evolu_puntos1), y = cumsum(evolu_puntos1)), col = "red") +
  geom_line(aes(x = 1:length(evolu_puntos2), y = cumsum(evolu_puntos2)), col = "blue") +
  geom_line(aes(x = 1:length(evolu_puntos3), y = cumsum(evolu_puntos3)), col = "green") +
  scale_x_continuous(breaks=1:max(length(evolu_puntos1), 
                                  length(evolu_puntos2), 
                                  length(evolu_puntos3)))


########################################################################
# Muchas simulaciones. Hacer una hipotesis de como seria el resultado
########################################################################

max_tiradas1 <- 5
min_puntos2 <- 20

partidas <- 1000
resultados <- tibble(partida = numeric(), ganador = numeric())

for (i in 1:partidas){
  
  total_puntos1 <- 0
  total_puntos2 <- 0
  total_puntos3 <- 0
  
  while(total_puntos1 < 100 & total_puntos2 < 100 & total_puntos3 < 100){
    
    parcial1 <- 0
    tiradas1 <- 1
    while(tiradas1 <= max_tiradas1){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial1 <- 0
        break
      }else{
        parcial1 <- parcial1 + resultado    
        tiradas1 <- tiradas1 + 1
        if(total_puntos1 + parcial1 >= 100) break
      }
    }
    total_puntos1 <- total_puntos1 + parcial1
    
    parcial2 <- 0
    while(parcial2 <= min_puntos2){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial2 <- 0
        break
      }else{
        parcial2 <- parcial2 + resultado    
        if(total_puntos2 + parcial2 >= 100) break
      }
    }
    total_puntos2 <- total_puntos2 + parcial2
    
    parcial3 <- 0
    while(parcial3 <= max(parcial1, parcial2)){
      resultado <- sample(1:6, 1)
      if (resultado == 1) {
        parcial3 <- 0
        break
      }else{
        parcial3 <- parcial3 + resultado    
        if(total_puntos3 + parcial3 >= 100) break
      }
    }
    total_puntos3 <- total_puntos3 + parcial3
  }
  
  
  resultados <- add_row(resultados,
                        partida = i,
                        ganador = ganador <- case_when(
                          total_puntos1 >= 100 ~ 1,
                          total_puntos2 >= 100 ~ 2,
                          total_puntos3 >= 100 ~ 3
                        )
  )
  
}

ggplot(resultados) + geom_bar(aes(ganador))

