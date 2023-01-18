#Esta funcion cuenta los registros na de una lista
count_na_list <- function(lista_con_na) {
  vect_frec_na <- c()
  for(j in 1:length(lista_con_na)) { 
    vect_frec_na[j]=is.na(lista_con_na[[j]])
  }
  frec_na <- sum(vect_frec_na, na.rm = TRUE)
  return(frec_na)
}