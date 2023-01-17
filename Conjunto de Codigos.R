#############################
#Conjunto de codigos y funciones para precargar que usa la automatización, se
#planea subirse a un repositorio

count_na_list <- function(lista_con_na) {
  vect_frec_na <- c()
  for(j in 1:length(lista_con_na)) { 
    vect_frec_na[j]=is.na(lista_con_na[[j]])
  }
  frec_na <- sum(vect_frec_na, na.rm = TRUE)
  return(frec_na)
}
#Esta funcion cuenta los registros null de una lista
count_null_list <- function(lista_con_null) {
  vect_frec_null <- c()
  for(j in 1:length(lista_con_null)) { 
    vect_frec_null[j]=is.null(lista_con_null[[j]])
  }
  frec_null <- sum(vect_frec_null, na.rm = TRUE)
  return(frec_null)
}


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
#Esta es una funcion que recibe de parametro una lista y la transforma en otra lista
#Igual pero sin registros de NA

list_sin_na <- function(lista_con_na) {
  lista_sin_na <- Filter(Negate(is.na),lista_con_na)
  return(lista_sin_na)
}
#Esta es una funcion que recibe de parametro una lista y la transforma en otra lista
#Igual pero sin registros de Null

list_sin_null <- function(lista_con_null) {
  lista_sin_null <- Filter(Negate(is.null),lista_con_null)
  return(lista_sin_null)
}

graf_pastel <- function(lista_categorias, name_variable) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- data.frame(table(datos))
  gama_colores <- hcl.colors(length(frecuencia_datos$Freq), "Plasma")
  pie3D(frecuencia_datos$Freq, labels = frecuencia_datos$datos, explode=0.2, main = paste("Diagrama de Pastel - ",name_variable),
        col = gama_colores)
}

graf_barras_f_abs <- function(lista_categorias,var_name) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- table(datos)
  gama_colores <- hcl.colors(length(frecuencia_datos), "Plasma")
  factor_et <- max(frecuencia_datos)*0.1
  grafica <- barplot(frecuencia_datos, ylim=c(0,max(frecuencia_datos)+(factor_et*2)), main = paste("Frec. Abs - ",var_name),col=gama_colores)
  text(grafica, frecuencia_datos + factor_et, labels = frecuencia_datos)
}

graf_barras_f_relat <- function(lista_categorias,var_name) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- table(datos)
  gama_colores <- hcl.colors(length(frecuencia_datos), "Plasma")
  factor_et <- max(frecuencia_datos)*0.1
  grafica <- barplot(prop.table(frecuencia_datos), main = paste("Frec. Relativa - ",var_name),col=gama_colores)
  #text(grafica, frecuencia_datos + factor_et, labels = frecuencia_datos)
}

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

Crea_frec_acum <- function(tabla_frecuencias){
  num_clases <- length(tabla_frecuencias[,1])
  frecuencias_acum <- rep(0,num_clases)
  for (i in 1:num_clases) {
    if(i==1){ frecuencias_acum[i]=tabla_frecuencias[1,4]}
    else { frecuencias_acum[i]=frecuencias_acum[i-1]+tabla_frecuencias[i,4]}
  }
  frecuencias_acum_tabla <- data.frame(tabla_frecuencias,frecuencias_acum)
  return(frecuencias_acum_tabla)
}

# Recibe como parametros el pivote (1:Intervalo, 2:Valor_fijo)
Crea_graf_ojiva <- function(tabla_frecuencias_acum, pivote, name_var){
  # Adecuacion tabla para que incluya la frecuencia cero y todos los limites sup e inf
  if(pivote==1){
    nuevos_limites <- c(tabla_frecuencias_acum[1,1],as.vector(tabla_frecuencias_acum$limites_sup))
    nuevas_frec_acum <- c(0, as.vector(tabla_frecuencias_acum$frecuencias_acum))
    aux <- (nuevos_limites[2]-nuevos_limites[1])/2
    x <- nuevos_limites-aux
    y <- nuevas_frec_acum
  }else{
    x <- tabla_frecuencias_acum$clases
    y <- tabla_frecuencias_acum$frecuencias_acum
    aux <- 0
  }
  
  plot(x,y,type="p",pch=20,lty=1,xlab="Clases",ylab="frec",main=paste("Ojiva - ",name_var),xaxt="n",yaxt="n")
  axis(side=1,x+aux,labels=TRUE)
  axis(side=2,y,labels=TRUE,las=2) 
  lines(x,y)}

# Recibe tal cual la columna de la tabla con los datos numericos ($)
Crea_graf_histograma <- function(vector_columna, titulo_graf){
  x <- vector_columna
  
  hist(x, main=paste("Histograma-",titulo_graf),xlab=titulo_graf,ylab="Frecuencia",col=palette())
}
# Crea_graf_histograma(tabla_airtable_prueba[,6],names(tabla_airtable_prueba[6]))

#Recibe 
Crea_graf_linea <- function(tabla_frecuencias, titulo_graf){
  y <- tabla_frecuencias$frecuencias
  x <- seq(1,length(y),1)
  
  plot(x, y, type = "b", main=paste("Graf. Lineas-",titulo_graf),xlab=titulo_graf,ylab="Frecuencia",col="lightblue", lwd = 5)
}


Procesar_Automatizacion <- function(tabla_a_automatizar){
  # El nombre de la variable que se recibe como parametro es:
  #  tabla_a_automatizar
  #Se verifica el numero de columnas
  
  num_variables <- length(tabla_a_automatizar) #Numero de Columnas/Variables
  nombres_Variables <- names(tabla_a_automatizar) #Extraemos el nombre de nuestras variables
  
  #Se crean vectores que guardaran info de cada una de las variables
  id_Variable <- c()
  name_Var <- c()
  tipo_Variable <- c()
  num_null_Var <- c()
  num_NA_Var <- c()
  
  for(k in 1:num_variables) { 
    columna_Var <- tabla_a_automatizar[[k]]
    
    id_Variable[k] <- k
    name_Var[k] <- nombres_Variables[k]#: Ya esta declarada al extraer los nombres
    tipo_Variable[k] <- class(columna_Var)
    
    # Vamos a verificar los NULL y NA para limpiarlos y saber la presencia
    num_null_Var[k] <- count_null_list(columna_Var)
    if(num_null_Var[k]>0){ columna_Var_nueva <- list_sin_null(columna_Var)}
    num_NA_Var[k] <- count_na_list(columna_Var)
    if(num_NA_Var[k]>0 & num_null_Var[k]>0){ columna_Var_nueva <- list_sin_na(columna_Var_nueva)}
    if(num_NA_Var[k]>0 & num_null_Var[k]==0){ columna_Var_nueva <- list_sin_na(columna_Var)}
    if(num_NA_Var[k]==0 & num_null_Var[k]==0){ columna_Var_nueva <- columna_Var}
    
    #if(tipo_variable[k]!="list"){ tipo_Variable[k]=corroborar_tipo_dato_character(columna_Var_nueva)}
    
    mensajeK1 <- paste(id_Variable[k],". La variable '", name_Var[k],"' es de tipo:",tipo_Variable[k], ".")
    mensajeK2 <- paste("Contiene ", length(columna_Var), " registros, de los cuales son ", num_null_Var[k], " Null y ", num_NA_Var[k], " NA")
    
    print(mensajeK1)
    print(mensajeK2)
    
    # Verificación de tipo Var
    if(tipo_Variable[k]=="character" | tipo_Variable[k]=="list"){
      
      if(length(unique(columna_Var_nueva))<length(columna_Var_nueva)*0.7){
        print("Se muestra su grafica de pastel y  grafica de Barras de acuerdo a la dist:")
        graf_pastel(columna_Var_nueva, name_Var[k])
        graf_barras_f_abs(columna_Var_nueva, name_Var[k])
        graf_barras_f_relat(columna_Var_nueva, name_Var[k])
        print(unique(columna_Var_nueva))
      } else if(length(unique(columna_Var_nueva))==length(columna_Var_nueva)) { print("La variable tiene todos los registros diferentes (Funciona como ID), no podemos obtener estadistícas")
      } else {print("La variable presenta muchos valores unicos, no se puede obtener estadisticas")
        print(paste("Son ", length(columna_Var_nueva), " observaciones, de las cuales ", length(unique(columna_Var_nueva)), " son diferentes"))
        print(unique(columna_Var_nueva))}
    } else { 
      print("Se muestra a continuacion las graficas de Linea, el histograma y la Ojiva de acuerdo a su dist.:")
      
      resumen_var_numeric(columna_Var_nueva, name_Var[k])
      clases_y_frecuencias <- creacion_vector_Clases(columna_Var)
      frecuencias_acumuladas <- Crea_frec_acum(clases_y_frecuencias)
      
      Crea_graf_ojiva(frecuencias_acumuladas, 1, name_Var[k])
      Crea_graf_histograma(columna_Var_nueva, name_Var[k])
      Crea_graf_linea(clases_y_frecuencias, name_Var[k])
      
    }
  }
}

