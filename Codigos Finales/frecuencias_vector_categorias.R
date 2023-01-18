#Esta funcion consiste en unir todas las categorias registradas, en una tabla de frecuencias


frecuencias_vector_categorias <- function(lista_categorias) { #Recibe el vector columna con los datos de las categorias
  contador <- 1
  nuevo_vector <- c()  
  for(j in 1:length(lista_categorias)) { 
    categorias_registro <- lista_categorias[[j]]
    for(i in 1:length(categorias_registro)){
      nuevo_vector[contador]=lista_categorias[[j]][i]
      contador=contador+1
    }
  }
  #tablas_frecuencias_categorias <- table(nuevo_vector)
  #return(tablas_frecuencias_categorias)
  return(nuevo_vector)
}