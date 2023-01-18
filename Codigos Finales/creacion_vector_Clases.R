
# No necesariamentre deber ecibir el vector sin null o na, porque lo limpia antes
creacion_vector_Clases <- function(vector_columna){
  vector_con_datos <- list_sin_na(vector_columna)
  vector_con_datos <- list_sin_null(vector_con_datos)
  vector_con_datos <- unlist(vector_con_datos) #Aqui transformamos la lista a vector
  
  # Num de elementos
  n <- length(vector_con_datos)
  
  # Num de intervalos
  n_intervalos <- ceiling(sqrt(n))
  # Rango, nos interesa en numero entero
  valor_min <- floor(min(vector_con_datos))
  valor_max <- ceiling(max(vector_con_datos))
  rango <- valor_max-valor_min
  # tamano intervalos
  tamano <- ceiling(rango/n_intervalos)
  
  #Creacion vector clases
          # Intervalos
          max_inf <- valor_min+((n_intervalos-1)*tamano)
          max_sup <- valor_min+(n_intervalos*tamano)
          limites_inf <- seq(valor_min,max_inf,tamano)
          limites_sup <- seq(valor_min+tamano,max_sup,tamano)
          # Leyenda de Intervalos
          clases <- paste("[",limites_inf,",", limites_sup,")", sep = " ")
          frecuencias <- rep(0,n_intervalos)
          
  for (i in 1:n_intervalos){
    pivote_contador=0
    #Frecuencias
      for (j in 1:n){
          if(vector_con_datos[j]>=limites_inf[i] & vector_con_datos[j]<limites_sup[i]){
            pivote_contador=pivote_contador+1
          } else {pivote_contador=pivote_contador}}
    frecuencias[i]=pivote_contador
      }
matriz_clases <- data.frame(limites_inf, limites_sup, clases,frecuencias)
vector_clases <- frecuencias
  
return(matriz_clases)
}