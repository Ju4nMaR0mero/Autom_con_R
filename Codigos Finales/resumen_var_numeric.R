# Funcion que muestra el resumen de una variable numerica

resumen_var_numeric <- function(vector_columna, nombre_var){
  # Estructura
  mens_k1 <- paste("La varaible ", nombre_var, "tiene las siguientes primeras 6 observaciones")
  print(mens_k1)
  head(vector_columna)
  
  #Caracteristicas
  mens_k2 <- paste("La varaible ", nombre_var, " tambien presenta las siguientes caracteristicas: (min, max y promedio)")
  print(mens_k2)
  summary(vector_columna)
}