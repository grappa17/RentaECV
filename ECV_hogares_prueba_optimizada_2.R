
###                       ###
###                       ###
##### CARGA DE DATOS    #####
###                       ###
###                       ###


# Cargar librerías necesarias
library(data.table)  # Para manipulación eficiente de datos
library(survey)      # Para análisis de encuestas complejas
library(readxl)     # Para leer archivos Excel
library(writexl)    # Para exportar resultados a Excel
library(tidyverse)

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

  ###                       ###
  ###                       ###
##### PROCESAMIENTO DE DECILES#####
  ###                       ###
  ###                       ###

# Crear listas vacías para almacenar los resultados
años <- unique(df_completo$AÑO_RENTA)
diseños_encuesta <- list()
resultados_deciles <- list()
medias_deciles <- list()

# Bucle principal para procesar cada año
for (year in años) {
  # 1. Crear subset y diseño de encuesta para cada año
  datos_año <- subset(df_completo, AÑO_RENTA == year)
  diseños_encuesta[[as.character(year)]] <- svydesign(
    id = ~HB030,     
    weights = ~DB090,  
    data = datos_año,
    nest = TRUE
  )
  
  # 2. Calcular deciles para el año actual
  deciles <- svyquantile(
    ~renta_disp_real,
    diseños_encuesta[[as.character(year)]],
    seq(0.1, 0.9, by = 0.1),
    keep.var = TRUE
  )
  resultados_deciles[[as.character(year)]] <- deciles
  
  # 3. Calcular medias por decil
  medias_año <- list()
  # Extraer valores numéricos de los deciles
  valores_deciles <- as.numeric(deciles[[1]])
  
  for (i in 1:10) {
    # Definir límites para cada decil
    limite_inferior <- if(i == 1) -Inf else valores_deciles[i-1]
    limite_superior <- if(i == 10) Inf else valores_deciles[i]
    
    # Subset para el decil actual
    subset_decil <- subset(
      diseños_encuesta[[as.character(year)]],
      renta_disp_real >= limite_inferior & renta_disp_real < limite_superior
    )
    
    # Calcular media para el decil
    media_decil <- svymean(~renta_disp_real, subset_decil)
    medias_año[[i]] <- media_decil
  }
  medias_deciles[[as.character(year)]] <- medias_año
}

# Función auxiliar para extraer resultados en formato más amigable
extraer_resultados <- function(año) {
  # Obtener los valores de los deciles para este año
  deciles_año <- resultados_deciles[[as.character(año)]]
  valores_deciles <- as.numeric(deciles_año[[1]])
  
  # Crear vector de límites (uno más que el número de deciles)
  limites <- c(-Inf, valores_deciles, Inf)
  
  # Crear data frame
  data.frame(
    Año = rep(año, 10),  # Repetir el año 10 veces
    Decil = 1:10,
    Limite_inferior = limites[1:10],
    Limite_superior = limites[2:11],
    Media = sapply(medias_deciles[[as.character(año)]], function(x) as.numeric(x))
  )
}

# Crear un data frame con todos los resultados
resultados_finales <- do.call(rbind, lapply(años, extraer_resultados))



###                               ###
###                               ###
##### PROCESAMIENTO DE QUANTILES  #####
###                               ###
###                               ###

# Crear listas vacías para almacenar los resultados
años <- unique(df_completo$AÑO_RENTA)
diseños_encuesta <- list()
resultados_quantiles <- list()
medias_intervalos <- list()

# Definir los puntos de corte que necesitamos (0.25, 0.50, 0.75, 0.90)
puntos_corte <- c(0.25, 0.50, 0.75, 0.90)
# Definir nombres de los intervalos para mejor interpretación
nombres_intervalos <- c("0-25", "25-50", "50-75", "75-90", "90-100")

# Bucle principal para procesar cada año
for (year in años) {
  # 1. Crear subset y diseño de encuesta para cada año
  datos_año <- subset(df_completo, AÑO_RENTA == year)
  diseños_encuesta[[as.character(year)]] <- svydesign(
    id = ~HB030,     
    weights = ~DB090,  
    data = datos_año,
  )
  
  # 2. Calcular quantiles para el año actual
  quantiles <- svyquantile(
    ~renta_disp_real,
    diseños_encuesta[[as.character(year)]],
    puntos_corte,
    keep.var = TRUE
  )
  resultados_quantiles[[as.character(year)]] <- quantiles
  
  # 3. Calcular medias por intervalo
  medias_año <- list()
  valores_quantiles <- as.numeric(quantiles[[1]])
  
  # Calcular media para cada intervalo
  for (i in 1:5) {  # Ahora tenemos 5 intervalos
    # Definir límites según el intervalo
    limite_inferior <- if(i == 1) -Inf else valores_quantiles[i-1]
    limite_superior <- if(i == 5) Inf else valores_quantiles[i]
    
    # Subset para el intervalo actual
    subset_intervalo <- subset(
      diseños_encuesta[[as.character(year)]],
      renta_disp_real >= limite_inferior & renta_disp_real < limite_superior
    )
    
    # Calcular media para el intervalo
    media_intervalo <- svymean(~renta_disp_real, subset_intervalo)
    medias_año[[i]] <- media_intervalo
  }
  medias_intervalos[[as.character(year)]] <- medias_año
}

# Crear listas para almacenar las sumas de renta y el ratio
renta_total_50_pobre <- list()
renta_total_10_rico <- list()
ratio_10r_vs_50p <- list()

for (year in años) {
  # Obtener valores de los quantiles
  valores_quantiles <- as.numeric(resultados_quantiles[[as.character(year)]][[1]])
  
  # Definir límites de los grupos
  limite_50_pobre <- valores_quantiles[2]  # Límite del 50% más pobre
  limite_90_rico <- valores_quantiles[4]   # Límite del 90% más rico
  
  # Subset para el 50% más pobre
  subset_50_pobre <- subset(
    diseños_encuesta[[as.character(year)]],
    renta_disp_real < limite_50_pobre
  )
  
  # Subset para el 10% más rico
  subset_10_rico <- subset(
    diseños_encuesta[[as.character(year)]],
    renta_disp_real >= limite_90_rico
  )
  
  # Calcular renta total ponderada para cada grupo
  renta_total_50_pobre[[as.character(year)]] <- svytotal(~renta_disp_real, subset_50_pobre)
  renta_total_10_rico[[as.character(year)]] <- svytotal(~renta_disp_real, subset_10_rico)
  
  # Extraer valores numéricos
  renta_50_pobre_valor <- as.numeric(renta_total_50_pobre[[as.character(year)]])
  renta_10_rico_valor <- as.numeric(renta_total_10_rico[[as.character(year)]])
  
  # Calcular ratio
  ratio_10r_vs_50p[[as.character(year)]] <- renta_10_rico_valor / renta_50_pobre_valor
}

# Función auxiliar para extraer resultados en formato más amigable
extraer_resultados <- function(año) {
  # Obtener los valores de los quantiles para este año
  quantiles_año <- resultados_quantiles[[as.character(año)]]
  valores_quantiles <- as.numeric(quantiles_año[[1]])
  
  # Crear vector de límites
  limites <- c(-Inf, valores_quantiles, Inf)
  
  # Crear data frame
  data.frame(
    Año = rep(año, 5),  # Ahora 5 intervalos en lugar de 10
    Intervalo = nombres_intervalos,
    Limite_inferior = limites[1:5],
    Limite_superior = limites[2:6],
    Media = sapply(medias_intervalos[[as.character(año)]], function(x) as.numeric(x))
  )
}

# Crear un data frame con todos los resultados
resultados_finales_quantil <- do.call(rbind, lapply(años, extraer_resultados))

# Creo tabla ancha
tabla_ancha_quantil <- resultados_finales_quantil %>%
  select(Año, Intervalo, Media) %>%  # Mantiene solo las columnas necesarias
  pivot_wider(names_from = Intervalo, values_from = Media)

# Exporto a excel
write_xlsx(tabla_ancha_quantil, "media_renta_por_quantil.xlsx")


# Convertir resultados en un dataframe
df_ratio <- data.frame(
  Año = años,
  Renta_total_50_pobre = sapply(renta_total_50_pobre, as.numeric),
  Renta_total_10_rico = sapply(renta_total_10_rico, as.numeric),
  Ratio_10r_vs_50p = sapply(ratio_10r_vs_50p, as.numeric)
)

# Exportar a Excel
write_xlsx(df_ratio, "ratio_renta_10r_vs_50p.xlsx")