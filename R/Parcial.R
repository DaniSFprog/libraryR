library(nycflights13)
library(tidyverse)

## ejercicio 5.2.4 item 1

## 1. tubo un retraso de llegada de dos o mas horas

flights<- flights
vuelos_retraso<- filter(flights,arr_delay >= 120)

##2. volo a houston

houston <- filter(flights,dest == 'HOU')

## 3. aerolineas united,american o delta

aerolineas <- filter(flights,carrier == 'DL'|carrier=='UA'|carrier=='AA')

## 4. viajes en verano

verano <- filter(flights,month %in%  c(7,8,9))

## 5. llegaron mas de 2 horas tarde, pero no salieron tarde

cinco <- filter(flights, dep_delay <= 0 & arr_delay> 120)

## 6. Se retrasaron al menos una hora, pero recuperaron más de 30 minutos en vuelo

seis <- filter(flights, arr_delay >= 60 & arr_delay - dep_delay > 30)

## 7. Partieron entre la medianoche y las 6 a.m. (incluyente)

siete <- filter(flights,dep_time %in% c(1:600) | arr_time == 2400 )

## item 2. Otro útil ayudante de filtrado de dplyr es between(). ¿Qué hace? ¿Puedes usarlo para simplificar el código necesario para responder a los desafíos anteriores?

##permite abreviar la escritura de código que tiene esta estructura `x >= derecha & x <= izquierda` por `between(x, derecha, izquierda)

verano <- filter(flights,between (month,7,9))

## ejercicio 5.2.4

##item 1. uso de arrange para ordenar datos faltantes

faltantes <- arrange(flights,desc(is.na(air_time)))

## item 2 Ordena `vuelos` para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano.

## vuelos mas retrasados

vuelos_retra <- arrange(flights, desc(dep_delay))

## vuelos mas  temprano

temprano <- arrange(flights, dep_delay)

## item  3 vuelos mas rapidos

rapidos <- arrange(flights,desc(distance/air_time))


## item 4 ¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó menos cerca?

## mas lejos

lejos <- arrange(flights,desc(distance))

## mas cerca

cerca <- arrange(flights,distance)

##5.4.1

## item 2 ¿Qué sucede si incluye el nombre de una variable varias veces en una select()llamada?

cuatrodf2 <- select(flights,distance,distance,distance,distance,distance)

## no importa cuantas veces sea llamada , esta sera considerada solamente una vez

##item 3 funcion any_OF

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
cuatrodf3 <- select(flights, any_of(vars))
##el any_of sirve para colocar valores faltantes si el any lo acompañamos de un menos nos pondra las demas columnas
##en este caso el select nods filtra las variables que ha llamado el any_of del vector "vars'

#item 4 Es una función muy util ya que nos puede ayudar a buscar de manera muy general un conjunto de caracteres dentro del nombre de una variable.
cuatrodf4 <- select(flights, contains("TIME"))

##5.5.2

## item 1 Actualmente dep_timey sched_dep_time son convenientes a la vista, pero difíciles de calcular porque en realidad no son números continuos. Conviértalos a una representación más conveniente de la cantidad de minutos desde la medianoche.

cincodf1 <- mutate(flights, sched_dep_time_min
                   = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440) %>%
  mutate(dep_time_min =
           (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440)

##item 2 Comparar air_time con arr_time - dep_time. Que esperas ver? ¿Que ves? ¿Qué necesitas hacer para arreglarlo?

cincodf2 <-flights%>%
  mutate(air_time_comp= arr_time-dep_time)%>%

  ## en la tabla se muestra que hay un desfase de tiempo ya que hay que tomar en cuenta otros datos

  ##5.6.7

  ##item 1
