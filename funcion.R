#' Function punto 4 github
#'
#' @param input : es un numero entero de 1-6
#' @return una de las soluciones propuestas en el punto 2
#' @examples
#' retrieve_answer(#) must be between 1 and 6
#' EXPLICAR UN POCO MEJOR
#'
retrieve_answer <- function(input){
  if (input == 1){
    print("BREVE EXPLICACION PUNTO 1")
    print("## Ejercicio 5.2.4 item 1")
    print("## 1. tubo un retraso de llegada de dos o mas horas")
    flights<- flights
    filter(flights,arr_delay >= 120)
    print("## 2. volo a houston")
    filter(flights,dest == 'HOU') %>%
      select(dest,year,month,tailnum)
    print("## 3. aerolineas united,american o delta")
    filter(flights,carrier == 'DL'|carrier=='UA'|carrier=='AA')%>%
      select(carrier,year,month,tailnum)
    print("## 4. viajes en verano")
    filter(flights,month %in%  c(7,8,9))
    print("## 5. llegaron mas de 2 horas tarde, pero no salieron tarde")
    filter(flights, dep_delay <= 0 & arr_delay> 120)
    print("## 6. Se retrasaron al menos una hora, pero recuperaron más de 30 minutos en vuelo")
    filter(flights, arr_delay >= 60 & arr_delay - dep_delay > 30)
    print("## 7. Partieron entre la medianoche y las 6 a.m. (incluyente)")
    filter(flights,dep_time %in% c(1:600) | arr_time == 2400 )
    print("## item 2. Otro útil ayudante de filtrado de dplyr es between(). ¿Qué hace? ¿Puedes usarlo para simplificar el código necesario para responder a los desafíos anteriores?")
    print("##permite abreviar la escritura de código que tiene esta estructura `x >= derecha & x <= izquierda` por `between(x, derecha, izquierda)")
    filter(flights,between (month,7,9))
    return(TRUE)
  }
  if (input == 2){
    print("BREVE EXPLICACION PUNTO 2")
    print("## Ejercicio 5.3.1")
    print("##item 1. uso de arrange para ordenar datos faltantes")
    arrange(flights,desc(is.na(air_time)))
    print("## item 2 Ordena `vuelos` para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano.")
    print("## vuelos mas retrasados")
    arrange(flights, desc(dep_delay))
    print("## vuelos mas  temprano")
    arrange(flights, dep_delay)
    print("## item  3 vuelos mas rapidos")
    arrange(flights,desc(distance/air_time))
    print("## item 4 ¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó menos cerca?")
    print("## mas lejos")
    arrange(flights,desc(distance))
    print("## mas cerca")
    arrange(flights,distance)
    return(TRUE)
  }
  if (input == 3){
    print("BREVE EXPLICACION PUNTO 3")
    print("## Ejercicio 5.4.1")
    print("## item 2 ¿Qué sucede si incluye el nombre de una variable varias veces en una select()llamada?")
    select(flights,distance,distance,distance,distance,distance)
    print("## no importa cuantas veces sea llamada , esta sera considerada solamente una vez")
    print("##item 3 funcion any_OF")
    vars <- c("year", "month", "day", "dep_delay", "arr_delay")%>%
      flights <- flights%>%
      select(flights, any_of(vars))
    print("##el any_of sirve para colocar valores faltantes si el any lo acompañamos de un menos nos pondra las demas columnas")
    print("##en este caso el select nods filtra las variables que ha llamado el any_of del vector 'vars' ")
    print("##item 4 Es una función muy util ya que nos puede ayudar a buscar de manera muy general un conjunto de caracteres dentro del nombre de una variable.")
    select(flights, contains("TIME"))
    return(TRUE)
  }
  if (input == 4){
    print("BREVE EXPLICACION PUNTO 4")
    print("## item 1 Actualmente dep_timey sched_dep_time son convenientes a la vista, pero difíciles de calcular porque en realidad no son números continuos. Conviértalos a una representación más conveniente de la cantidad de minutos desde la medianoche.")
    mutate(flights, sched_dep_time_min
           = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440) %>%
      mutate(dep_time_min =
               (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440)
    print("##item 2 Comparar air_time con arr_time - dep_time. Que esperas ver? ¿Que ves? ¿Qué necesitas hacer para arreglarlo?")
    mutate(flights,air_time_comp= arr_time-dep_time)%>%
      select(tailnum,air_time,air_time_comp,dep_time,arr_time)
    print("## en la tabla se muestra que hay un desfase de tiempo ya que hay que tomar en cuenta otros datos")
    return(TRUE)
  }
  if (input == 5){
    print("BREVE EXPLICACION PUNTO 5")
    print("## Ejercicio 5.6.7")
    print("## item 1")
    print("## Un vuelo llega 15 minutos antes el 50% del tiempo y 15 minutos tarde el 50% del tiempo.")
    print("##- Un vuelo siempre llega 10 minutos tarde.")
    print("##- Un vuelo llega 30 minutos antes el 50% del tiempo y 30 minutos tarde el 50% del 'tiempo.'")
    print("##- 99% del tiempo un vuelo es puntual. El 1% de las veces llega 2 horas tarde.")
    print("##¿Qué es más importante: el retraso en la llegada o el retraso en la salida?")
    print("## primero debemos filtrar los datos de vuelo no existentes es decir que aparescan na")
    not_cancelled <- flights %>%
      filter(!is.na(air_time))
    print("##la llegada es mas importante asi que es lo que toca ordenar primero")
    not_cancelled %>%
      group_by(tailnum) %>%
      summarise(
        count = n(),
        p_15_early_arr = mean(arr_delay < -15),
        p_15_dep_arr = mean(dep_delay < -15)
      ) %>%
      filter(p_15_early_arr > 0.5 | p_15_dep_arr > 0.5) %>%
      filter(count > 30) %>%
      arrange(desc(p_15_early_arr), desc(p_15_dep_arr))
    print("##auí se muestra  los aviones  que realizaron al menos 30 vuelos en un año y llegaron o salieron antes de los 15 minutos durante más del 50% del tiempo.")


    print("##La llegada es más 'importante'")
    not_cancelled %>%
      group_by(tailnum, origin, dest) %>%
      summarise(
        count = n(),
        arr_delay_10_c = sum(arr_delay > 10),
        arr_delay_10_p = mean(arr_delay > 10),
        dep_delay_10_c = sum(dep_delay > 10),
        dep_delay_10_p = mean(dep_delay > 10)
      ) %>%
      filter(count > 20) %>%
      arrange(desc(arr_delay_10_p))
    print("## Los vuelos de  Atlanta llegan con un retraso de más de 10 minutos la mayoría de las veces.")
    print("##PROBABILIDAD EXACTA DE 10 MINUTOS DE TARDÍA")
    not_cancelled %>%
      group_by(tailnum) %>%
      summarise(
        count = n(),
        exact_10 = mean(arr_delay == 10)
      ) %>%
      filter(count > 10) %>%
      arrange(desc(exact_10))

    print("##SELECCIONE PARA UN ENFOQUE DE 30 MINUTOS")
    print("## MEJORES VUELOS MENOS DE 30 MINUTOS CRITERIOS TEMPRANO O TARDÍO ORDENADOS")
    print("## LA LLEGADA ES MÁS IMPORTANTE DURANTE LA CLASIFICACIÓN")

    not_cancelled %>%
      group_by(tailnum) %>%
      mutate(
        count = n(),
        arr_30_early = mean(arr_delay < -30),
        dep_30_early = mean(dep_delay < -30),
        arr_30_late  = mean(arr_delay > 30),
        dep_30_late = mean(dep_delay > 30)
      ) %>%
      filter(count > 20) %>%
      arrange(desc(arr_30_early), desc(dep_30_early), arr_30_late, dep_30_late) %>%
      select(dest)
    print("##la llegada es mas importante asi que  se ordena primero la llegada")
    not_cancelled %>%
      group_by(tailnum) %>%
      mutate(
        count = n(),
        median_arr_delay = median(arr_delay),
        median_dep_delay = median(dep_delay)
      ) %>%
      filter(count > 30) %>%
      arrange(median_arr_delay, median_dep_delay)
    print("## De la tabla vemos que el vuelo N479AA# el vuelo  llega antes de los 20 minutos el 50% del tiempo, sale antes de los 3 minutos el 50% del tiempo.")
    return(TRUE)
  }
  if (input == 6){
    print("BREVE EXPLICACION PUNTO 6")
    print("## Ejercicio 5.7.1")

    print("##item 2 ¿Qué avión ( tailnum) tiene el peor récord de puntualidad?")
    not_cancelled %>%
      group_by(tailnum) %>%
      summarise(
        count = n(),
        max_arr_delay = max(arr_delay),
        is_on_time_freq = mean(arr_delay <= 0, na.rm = TRUE)
      ) %>%
      filter(count > 30) %>%
      arrange(desc(is_on_time_freq))
    return(TRUE)
  }
  else{
    print("Error 404 el numero ingresado debe ser entre 1 y 6")
    return(FALSE)
  }
}
