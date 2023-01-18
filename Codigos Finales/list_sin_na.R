#Esta es una funcion que recibe de parametro una lista y la transforma en otra lista
#Igual pero sin registros de NA

list_sin_na <- function(lista_con_na) {
  lista_sin_na <- Filter(Negate(is.na),lista_con_na)
  return(lista_sin_na)
}