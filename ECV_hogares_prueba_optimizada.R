

# Cargar librerías necesarias
library(data.table)  # Para manipulación eficiente de datos
library(survey)      # Para análisis de encuestas complejas
library(readxl)     # Para leer archivos Excel
library(writexl)    # Para exportar resultados a Excel

# Definir directorio de trabajo
setwd("C:/Users/lorie/Desktop/CGT/R/ECV_hogares/detallados_hogar_copia")

# Cargar deflactores
ipc <- read_xlsx("C:/Users/lorie/Desktop/CGT/R/ECV_hogares/IPC.xlsx")
ipc <- as.data.table(ipc)

# Función para leer y combinar archivos de un tipo (D o H)
cargar_datos <- function(tipo, columnas) {
  archivos <- paste0("esudb", substr(2008:2023, 3, 4), tipo, ".csv")
  dt_list <- lapply(archivos, function(archivo) {
    fread(archivo, select = columnas, na.strings = c("", "NA"))
  })
  return(rbindlist(dt_list, fill = TRUE))
}

# Definir columnas a cargar
columnas_d <- c("DB010", "DB030", "DB090")   # Factor de elevación
columnas_h <- c("HB010", "HB020", "HB030", "vhRentaa", "vhRentaAIa")

# Cargar datos
df_d <- cargar_datos("d", columnas_d)
df_h <- cargar_datos("h", columnas_h)

# Unir ambos conjuntos de datos (join mediante data.table)
setkey(df_d, DB010, DB030)
setkey(df_h, HB010, HB030)
df_completo <- df_h[df_d, nomatch = 0]

# Crear variable de año de renta
df_completo[, AÑO_RENTA := HB010 - 1]

# Unir deflactores
df_completo <- merge(df_completo, ipc, by = "AÑO_RENTA", all.x = TRUE)

# Calcular ingresos deflactados
df_completo[, renta_disp_real := vhRentaa / deflactor_IPC]
df_completo[, renta_disp_real_alq := vhRentaAIa / deflactor_IPC]

# Definir diseño de encuesta
encuesta_design <- svydesign(
  id = ~HB030,     # Unidades primarias de muestreo
  strata = ~HB010, # Estratos (aquí se usa HB010; para deciles por año se hace el subset)
  weights = ~DB090,  # Factor de elevación
  data = df_completo,
  nest = TRUE
)

# Calcular deciles ponderados por año
df_completo[, decil := cut(
  renta_disp_real, 
  breaks = quantile(renta_disp_real, probs = seq(0, 1, by = 0.1), weights = DB090, na.rm = TRUE),
  labels = 1:10,
  include.lowest = TRUE
), by = AÑO_RENTA]

# Calcular media ponderada por decil y año
tabla_medias <- svyby(
  ~renta_disp_real, 
  ~AÑO_RENTA + decil, 
  encuesta_design, 
  svymean, 
  na.rm = TRUE
)

# Convertir a data.table
tabla_medias <- as.data.table(tabla_medias)

# Convertir la tabla de medias a formato ancho:
tabla_ancha <- dcast(tabla_medias, AÑO_RENTA ~ decil, value.var = "renta_disp_real")

# Exportar la tabla en formato ancho a Excel:
write_xlsx(tabla_ancha, "media_renta_por_decil.xlsx")

# Comprobar peso de cada decil respecto al total de cada año:
pesos_por_decil <- df_completo[, .(suma_pesos = sum(DB090)), by = .(AÑO_RENTA, decil)]
pesos_por_decil[, prop_pesos := suma_pesos / sum(suma_pesos), by = AÑO_RENTA]

# Visualizamos el resultado
print(pesos_por_decil)






# Calcular quintiles ponderados por año
df_completo[, quintil := cut(
  renta_disp_real, 
  breaks = quantile(renta_disp_real, probs = seq(0, 1, by = 0.2), weights = DB090, na.rm = TRUE),
  labels = 1:5,
  include.lowest = TRUE
), by = AÑO_RENTA]


# Calcular media ponderada por quintil y año
tabla_quintiles <- svyby(
  ~renta_disp_real, 
  ~AÑO_RENTA + quintil, 
  encuesta_design, 
  svymean, 
  na.rm = TRUE
)

# Convertir a data.table
tabla_quintiles <- as.data.table(tabla_quintiles)

# Convertir la tabla de medias a formato ancho:
tabla_ancha_quintiles <- dcast(tabla_quintiles, AÑO_RENTA ~ quintil, value.var = "renta_disp_real")

# Exportar la tabla en formato ancho a Excel:
write_xlsx(tabla_ancha_quintiles, "media_renta_por_quintil.xlsx")

# Comprobar peso de cada quintil respecto al total de cada año:
pesos_por_quintil <- df_completo[, .(suma_pesos = sum(DB090)), by = .(AÑO_RENTA, quintil)]
pesos_por_quintil[, prop_pesos := suma_pesos / sum(suma_pesos), by = AÑO_RENTA]

# Visualizamos el resultado
print(pesos_por_quintil)






# Calcular tramos de centiles ponderados
df_completo[, quantil := cut(
  renta_disp_real, 
  breaks = quantile(renta_disp_real, probs = c(0, 0.25, 0.50, 0.75, 0.90, 1), weights = DB090, na.rm = TRUE),
  labels = c("0-25", "25-50", "50-75", "75-90", "90-100"),
  include.lowest = TRUE
), by = AÑO_RENTA]

# Calcular media ponderada por decil y año
tabla_medias_quantil <- svyby(
  ~renta_disp_real, 
  ~AÑO_RENTA + quantil, 
  encuesta_design, 
  svymean, 
  na.rm = TRUE
)

# Convertir a data.table
tabla_medias_quantil <- as.data.table(tabla_medias_quantil)

# Convertir la tabla de medias a formato ancho:
tabla_ancha_quantil <- dcast(tabla_medias_quantil, AÑO_RENTA ~ quantil, value.var = "renta_disp_real")

# Exportar la tabla en formato ancho a Excel:
write_xlsx(tabla_ancha_quantil, "media_renta_por_quantil.xlsx")



##### Top 20% / Bottom 20% #####
renta_tramos_quintil <- df_completo[, .(renta_total = sum(renta_disp_real * DB090, na.rm = TRUE)), by = .(AÑO_RENTA, quintil)]
renta_tramos_quintil <- dcast(renta_tramos_quintil, AÑO_RENTA ~ quintil, value.var = "renta_total")
renta_tramos_quintil[, indicador_desigualdad := `5` / (`1`)]
write_xlsx(renta_tramos_quintil, "indicador_top20_bottom20.xlsx")


# Indicador Top 10% / Bottom 50%
renta_tramos_quantil <- df_completo[, .(renta_total = sum(renta_disp_real * DB090, na.rm = TRUE)), by = .(AÑO_RENTA, quantil)]
renta_tramos_quantil <- dcast(renta_tramos_quantil, AÑO_RENTA ~ quantil, value.var = "renta_total")
renta_tramos_quantil[, indicador_desigualdad := `90-100` / (`0-25` + `25-50`)]
write_xlsx(renta_tramos_quantil, "indicador_top10_bottom50.xlsx")


