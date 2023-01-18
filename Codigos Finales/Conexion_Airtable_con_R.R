#install.packages("dplyr")
#install.packages("devtools")
#devtools::install_github("bergant/airtabler")

library(airtabler)
library(dplyr)

Sys.setenv("AIRTABLE_API_KEY"="keypGBUoR0Axl7hx0")
# Sys.setenv("AIRTABLE_API_KEY"="clave API de Usuario")
airtable <- airtabler::airtable("app8a51li6LWHdlyq", "equipo")
# airtabler::airtable("clave API de la base de Datos", "nombre de la Tabla")
tabla_airtable_prueba <- airtable$equipo$select_all()
# airtable$"nombre de la tabla"$select_all(), se coloca select_all para no omititr ningun dato


tabla_airtable <- air_select("app8a51li6LWHdlyq", "equipo")
columna1 <- tabla_airtable$nombre
columna2 <- tabla_airtable$`como prefieren`
columna3 <- tabla_airtable$`status dmon`

layla <- data.frame(columna1, columna3)
