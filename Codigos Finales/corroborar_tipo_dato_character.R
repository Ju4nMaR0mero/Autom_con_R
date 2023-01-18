corroborar_tipo_dato_character <- function(columna_vector){
  try(columna_vector<-as.Date(columna_vector))
  tipo_dato_verificar <- class(columna_vector)
  
  #Primero se verifica si es tipo Date, de lo contrario continua con tipo Numerico
  if(tipo_dato_verificar=="Date"){
    
    print("El tipo de Dato es Fecha")
    print(tipo_dato_verificar)}
  else{
    tipo_dato_verificar <- corroborar_tipo_dato_numeric(columna_vector)
  }
  return(tipo_dato_verificar)
}