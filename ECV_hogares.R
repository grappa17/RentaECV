


### INGRESOS MEDIOS POR DECILES DE RENTA - ECV ###

##### PREPARACIÓN DEL DATAFRAME BASE #####

# He descargado manualmente los ficheros de microdatos, transversales. Trabajo inicialmente con los ficheros detallados de hogares (h), pero para obtener
# el factor de elevación tengo que extraerlo de los ficheros de datos basicos del hogar (d).
# Lo primero es unirlos en un único dataframe. Para ello creo que debería crear una función y bucle en la que eligiera de cada año solo
# -> las columnas que necesito, con el mismo nombre, y luego las uniera en un único df. 

# Cargar librerías necesarias
library(data.table) # Para leer archivos grandes eficientemente
library(dplyr)      # Para la manipulación de dataframes
library(readxl)
library(tidyr)
library(purrr)
library(ggplot2)
library(writexl)
library(Hmisc)

# Defino directorio
setwd("C:/Users/lorie/Desktop/CGT/R/ECV_hogares/detallados_hogar")

# FICHERO D
# Definir todas las columnas, incluyendo las variantes flag. EXCLUIR LAS NECESARIAS
columnas <- c('DB010', 'DB030', 'DB090', 'DB090_F')
              
              
# Definir los archivos a leer
anios <- 2008:2023 # Crea un vector de años, desde 2008 hasta 2023
archivos <- paste0("esudb", substr(anios, 3, 4), "d.csv") # Este código combina elementos para alcanzar los nombres de todos los archivos que tengo

# Leer y seleccionar columnas de cada archivo, luego combinar todo
df_d <- lapply(archivos, function(archivo) {
  fread(archivo, select = columnas, na.strings = c("", "NA"))
}) %>%
  bind_rows()


# FICHERO H
# Definir todas las columnas, incluyendo las variantes flag. EXCLUIR LAS NECESARIAS
columnas <- c('HB010', 'HB020', 'HB030', 'HB050', 'HB050_F', 'HB060', 'HB060_F', 'HB070', 'HB070_F', 'HB080', 'HB080_F', 'HB100', 'HB100_F', 'HB120', 'HB120_F', # INFORMACIÓN BÁSICA
              'HY020', 'HY020_F', 'HY022', 'HY022_F', 'HY023', 'HY023_F', # RENTA
              'HY030N', 'HY030N_F', 'HY040N', 'HY040N_F', 'HY050N', 'HY050N_F', 'HY060N', 'HY060N_F', 'HY070N', 'HY070N_F', 'HY080N', 'HY080N_F', 'HY081N', 
              'HY081N_F', 'HY090N', 'HY090N_F', 'HY100N', 'HY100N_F', 'HY110N', 'HY110N_F', 'HY120N', 'HY120N_F', 'HY130N', 'HY130N_F', 'HY131N', 'HY131N_F', 
              'HY145N', 'HY145N_F', 'HY170N', 'HY170N_F', # RENTA NETA
              'HY010', 'HY010_F', 'HY040G', 'HY040G_F', 'HY050G', 'HY050G_F', 'HY060G', 'HY060G_F', 'HY070G', 'HY070G_F', 'HY080G', 
              'HY080G_F', 'HY081G', 'HY081G_F', 'HY090G', 'HY090G_F', 'HY100G', 'HY100G_F', 'HY110G', 'HY110G_F', 'HY120G', 'HY120G_F', 
              'HY130G', 'HY130G_F', 'HY131G', 'HY131G_F', 'HY140G', 'HY140G_F', # RENTA BRUTA
              'HS011', 'HS011_F', 'HS021', 'HS021_F', 'HS022', 'HS022_F', 'HS031', 'HS031_F', 'HS040', 'HS040_F', 'HS050', 'HS050_F', 'HS060', 'HS060_F', 
              'HS090', 'HS090_F', 'HS110', 'HS110_F', 'HS120', 'HS120_F', 'HS130', 'HS130_F', 'HS150', 'HS150_F', #EXCLUSIÓN SOCIAL
              'HD080', 'HD080_F', #EXCLUSIÓN SOCIAL MODULO APARTE
              'HH010', 'HH010_F', 'HH020', 'HH020_F','HH021', 'HH021_F', 'HH030', 'HH030_F', 'HH050', 'HH050_F', 'HH060', 'HH060_F', 'HH070', 'HH070_F', # VIVIENDA
              'HI010', 'HI010_F', 'HI020', 'HI020_F', 'HI030', 'HI030_F', 'HI040', 'HI040_F', 
              'CUOTAHIP', 'CUOTAHIP_F', 'cuotahip', 'cuotahip_F', 'HX040', 'HX060', 'HX240', 'vhRentaa', 'vhRentaaAI', 'vhRentaAIa', 'vhPobreza', 'vhMATDEP') # VARIABLES COMPLEMENTARIAS

# Definir los archivos a leer
anios <- 2008:2023 # Crea un vector de años, desde 2008 hasta 2023
archivos <- paste0("esudb", substr(anios, 3, 4), "h.csv") # Este código combina elementos para alcanzar los nombres de todos los archivos que tengo

# Leer y seleccionar columnas de cada archivo, luego combinar todo
df_h <- lapply(archivos, function(archivo) {
  fread(archivo, select = columnas, na.strings = c("", "NA"))
}) %>%
  bind_rows()


# Unir ambos DFs
df_completo <- left_join(df_h, df_d, by = c("HB030" = "DB030", "HB010" = "DB010"))

##### DEFLACTAR RENTA Y CALCULAR DECILES Y MEDIAS #####

# Lo primero, creo una nueva columna con el año al que corresponden los datos de renta (un año menos que la encuesta)
df_completo$AÑO_RENTA <- ifelse(
  !is.na(df_completo$HB010), # Condición: si HB010 no es NA
  df_completo$HB010 - 1,     # Resultado si la condición es TRUE
  df_completo$DB010 - 1      # Resultado si la condición es FALSE
)

# Introduzco el deflactor del IPC # OJO! AÑO BASE 2022!!!!!!!
deflactores <- read_xlsx("C:/Users/lorie/Desktop/CGT/R/ECV_hogares/IPC.xlsx")
df_completo <- df_completo %>%
  left_join(deflactores, by = "AÑO_RENTA")

# Calculo los ingresos deflactados para cada observación
df_completo <- df_completo %>%
  mutate(renta_disp_real = HY020 / deflactor_IPC) %>%
  mutate(renta_bruta_real = HY010 / deflactor_IPC)

# Calculo una columna con los ingresos que tiene en consideración el INE
df_completo <- df_completo %>%
  mutate(renta_INE = vhRentaAIa) # Ojo, cojo los interese pagados por la hipoteca para renta neta, no sé si sería mejor bruta

# Creo deciles
df_deciles <- df_completo %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_INE, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),        # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),# Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    decil = cut(
      proporción_acumulada,
      breaks = seq(0, 1, by = 0.1),  # Dividir en 10 partes iguales (deciles)
      labels = 1:10,
      include.lowest = TRUE
    )
  ) %>%
  ungroup() # Desagrupamos para evitar problemas posteriores

tabla_medias <- df_deciles %>%
  group_by(AÑO_RENTA, decil) %>%
  summarise(
    media_renta = weighted.mean(renta_INE, w = DB090, na.rm = TRUE),
    .groups = "drop"
  )
tabla_medias <- tabla_medias %>%
  arrange(AÑO_RENTA, decil)

# Exportar excel
tabla_ancha <- tabla_medias %>%
  pivot_wider(names_from = decil, values_from = media_renta, 
              names_prefix = "Decil_")  # Esto añade el prefijo "Decil_"
write_xlsx(tabla_ancha, "media_renta_por_decil.xlsx")


# CREO QUINTILES
# Creo quintiles
df_quintiles <- df_completo %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_INE, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),        # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),# Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    quintil = cut(
      proporción_acumulada,
      breaks = seq(0, 1, by = 0.2),  # Dividir en 5 partes iguales (quintiles)
      labels = 1:5,
      include.lowest = TRUE
    )
  ) %>%
  ungroup() # Desagrupamos para evitar problemas posteriores

# Medias por quintil
tabla_medias_quintiles <- df_quintiles %>%
  group_by(AÑO_RENTA, quintil) %>%
  summarise(
    media_renta = weighted.mean(renta_INE, w = DB090, na.rm = TRUE),
    .groups = "drop"
  )
tabla_medias_quintiles <- tabla_medias_quintiles %>%
  arrange(AÑO_RENTA, quintil)

# Exportar
tabla_ancha_quintiles <- tabla_medias_quintiles %>%
  pivot_wider(names_from = quintil, values_from = media_renta, 
              names_prefix = "Quintil_")  # Añadimos el prefijo "Quintil_"
write_xlsx(tabla_ancha_quintiles, "media_renta_por_quintil.xlsx")

##### POR TRAMOS PERSONALIZADOS PARA CUADRAR CON LA EFF #####

# Crear tramos de centiles personalizados
df_tramos_centiles <- df_completo %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_disp_real, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),              # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),      # Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    tramo_centil = cut(
      proporción_acumulada,
      breaks = c(0, 0.25, 0.50, 0.75, 0.90, 1),  # Tramos personalizados
      labels = c("0-25", "25-50", "50-75", "75-90", "90-100"),
      include.lowest = TRUE
    )
  ) %>%
  ungroup() # Desagrupamos para evitar problemas posteriores

# Calcular medias por tramo de centiles
tabla_medias_centiles <- df_tramos_centiles %>%
  group_by(AÑO_RENTA, tramo_centil) %>%
  summarise(
    media_renta = weighted.mean(renta_disp_real, w = DB090, na.rm = TRUE),
    .groups = "drop"
  )

# Ordenar la tabla
tabla_medias_centiles <- tabla_medias_centiles %>%
  arrange(AÑO_RENTA, tramo_centil)

# Exportar resultados
tabla_ancha_centiles <- tabla_medias_centiles %>%
  pivot_wider(names_from = tramo_centil, values_from = media_renta, 
              names_prefix = "Tramo_")  # Esto añade el prefijo "Tramo_"
write_xlsx(tabla_ancha_centiles, "media_renta_por_tramos_centiles.xlsx")

##### Top 10% / Bottom 50% #####
# Calcular la renta total por tramo (ponderada)
renta_por_tramo <- df_tramos_centiles %>%
  group_by(AÑO_RENTA, tramo_centil) %>%
  summarise(
    renta_total = sum(renta_disp_real * DB090, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = tramo_centil, values_from = renta_total, 
              names_prefix = "Tramo_")
colnames(renta_por_tramo)
# Calcular el indicador Top 10% / Bottom 50%
renta_por_tramo <- renta_por_tramo %>%
  mutate(indicador_desigualdad = `Tramo_90-100` / (`Tramo_0-25` + `Tramo_25-50`))

# Exportar los resultados
write_xlsx(renta_por_tramo, "indicador_desigualdad.xlsx")


##### Top 20% / Bottom 20% #####
# Agrupar por año y quintil para calcular la renta media ponderada en cada grupo
renta_por_quintil <- df_quintiles %>%
  group_by(AÑO_RENTA, quintil) %>%
  summarise(
    # Calcula la media ponderada: suma(renta * peso) / suma(peso)
    media_renta = sum(renta_INE * DB090, na.rm = TRUE) / sum(DB090, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = quintil, values_from = media_renta, names_prefix = "Quintil_")

# Calcular el indicador: media del top 20% (Quintil_5) dividido por la media del bottom 20% (Quintil_1)
renta_por_quintil <- renta_por_quintil %>%
  mutate(indicador_top20_bottom20 = Quintil_5 / Quintil_1)

# Exportar el resultado a Excel
write_xlsx(renta_por_quintil, "indicador_top20_bottom20.xlsx")

##### RATIO P75/P25 #####
# Calcular percentiles ponderados por año
percentiles <- df_completo %>%
  group_by(AÑO_RENTA) %>%
  summarise(
    P25 = Hmisc::wtd.quantile(renta_disp_real, weights = DB090, probs = 0.25, na.rm = TRUE),
    P75 = Hmisc::wtd.quantile(renta_disp_real, weights = DB090, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular el ratio P75/P25
percentiles <- percentiles %>%
  mutate(ratio_P75_P25 = P75 / P25)

# Mostrar los resultados
print(percentiles)

##### QUINTILES CON ALQUILER ######
# Crear renta sin alquiler, manejando NA en HH060
df_completo <- df_completo %>%
  mutate(
    renta_sin_alquiler = ifelse(
      is.na(HH060),
      renta_disp_real,
      renta_disp_real - (HH060 * 12 / deflactor_IPC)
    )
  )

df_quintiles_alquiler <- df_completo %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_sin_alquiler, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),        # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),# Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    quintil = cut(
      proporción_acumulada,
      breaks = seq(0, 1, by = 0.2),  # Dividir en 5 partes iguales (quintiles)
      labels = 1:5,
      include.lowest = TRUE
    )
  ) %>%
  ungroup() # Desagrupamos para evitar problemas posteriores

# Medias por quintil
tabla_medias_quintiles_alquiler <- df_quintiles_alquiler %>%
  group_by(AÑO_RENTA, quintil) %>%
  summarise(
    media_renta = mean(renta_sin_alquiler, na.rm = TRUE),
    .groups = "drop"
  )
tabla_medias_quintiles_alquiler <- tabla_medias_quintiles_alquiler %>%
  arrange(AÑO_RENTA, quintil)

# Exportar
tabla_ancha_quintiles_alquiler <- tabla_medias_quintiles_alquiler %>%
  pivot_wider(names_from = quintil, values_from = media_renta, 
              names_prefix = "Quintil_")  # Añadimos el prefijo "Quintil_"
write_xlsx(tabla_ancha_quintiles_alquiler, "media_renta_por_quintil_alquiler.xlsx")


##### TRABAJO CON DATOS DE ALQUIERES #####
# Habria que seleccionar solo inquilinos, calcular cuantos son en total en cada decil y cada año.
# Luego calcular para el grupo de inquilinos, que está pasando con sus rentas. 
df_inquilinos <- df_completo %>%
  filter(
    (HH021 %in% c(3, 4) & !is.na(HH021)) |  # Inquilinos según HH021
      (HH020 %in% c(2, 3) & !is.na(HH020))    # Inquilinos según HH020
  )

# Calcular la media mensual del gasto en alquiler ponderada por año
media_mensual_alquiler <- df_inquilinos %>%
  filter(!is.na(HH060)) %>% # Filtrar hogares con datos de alquiler
  group_by(AÑO_RENTA) %>%   # Agrupar por año
  summarise(
    media_mensual_alquiler = sum(HH060 * DB090, na.rm = TRUE) / sum(DB090, na.rm = TRUE), # Media ponderada
    .groups = "drop" # Evitar agrupamientos adicionales
  )

# Ver la tabla de medias
print(media_mensual_alquiler)

# Exportar a Excel si es necesario
write_xlsx(media_mensual_alquiler, "media_mensual_alquiler_por_año.xlsx")


# DECILES CON INQUILINOS

# Calcular deciles y agregar columna de inquilinos
df_deciles_inquilinos <- df_completo %>%
  mutate(inquilino = ifelse(
    (!is.na(HH021) & HH021 %in% c(3, 4)) | (!is.na(HH020) & HH020 %in% c(2, 3)),
    1,  # Marcamos como inquilino
    0   # No es inquilino
  )) %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_disp_real, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),              # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),      # Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    decil = cut(
      proporción_acumulada,
      breaks = seq(0, 1, by = 0.1),  # Dividir en 10 partes iguales (deciles)
      labels = 1:10,
      include.lowest = TRUE
    )
  ) %>%
  ungroup()

# Calcular número y porcentaje de inquilinos por decil y año
tabla_inquilinos_deciles <- df_deciles_inquilinos %>%
  group_by(AÑO_RENTA, decil) %>%                    # Agrupar por año y decil
  summarise(
    total_hogares_decil = sum(DB090, na.rm = TRUE), # Total de hogares en el decil
    num_inquilinos = sum(DB090[inquilino == 1], na.rm = TRUE), # Total de hogares inquilinos
    pct_inquilinos = num_inquilinos / total_hogares_decil * 100, # Porcentaje de inquilinos
    .groups = "drop" # Evitar agrupamientos adicionales
  )

# Ver los resultados
print(tabla_inquilinos_deciles)

# Exportar como Excel
write_xlsx(tabla_inquilinos_deciles, "inquilinos_por_decil_y_año.xlsx")

# QUINTILES CON INQUILINOS
# Calcular quintiles y agregar columna de inquilinos
df_quintiles_inquilinos <- df_completo %>%
  mutate(inquilino = ifelse(
    (!is.na(HH021) & HH021 %in% c(3, 4)) | (!is.na(HH020) & HH020 %in% c(2, 3)),
    1,  # Marcamos como inquilino
    0   # No es inquilino
  )) %>%
  group_by(AÑO_RENTA) %>%                    # Agrupar por año
  arrange(renta_disp_real, .by_group = TRUE) %>% # Ordenar por renta dentro de cada grupo
  mutate(
    total_hogares = sum(DB090),              # Total de hogares reales por año
    hogares_acumulados = cumsum(DB090),      # Suma acumulativa por año
    proporción_acumulada = hogares_acumulados / total_hogares, # Proporción acumulada
    quintil = cut(
      proporción_acumulada,
      breaks = seq(0, 1, by = 0.2),  # Dividir en 5 partes iguales (quintiles)
      labels = 1:5,
      include.lowest = TRUE
    )
  ) %>%
  ungroup()

# Calcular número y porcentaje de inquilinos por quintil y año
tabla_inquilinos_quintiles <- df_quintiles_inquilinos %>%
  group_by(AÑO_RENTA, quintil) %>%                    # Agrupar por año y quintil
  summarise(
    total_hogares_quintil = sum(DB090, na.rm = TRUE), # Total de hogares en el quintil
    num_inquilinos = sum(DB090[inquilino == 1], na.rm = TRUE), # Total de hogares inquilinos
    pct_inquilinos = num_inquilinos / total_hogares_quintil * 100, # Porcentaje de inquilinos
    .groups = "drop" # Evitar agrupamientos adicionales
  )

# Ver los resultados
print(tabla_inquilinos_quintiles)

# Exportar como Excel
write_xlsx(tabla_inquilinos_quintiles, "inquilinos_por_quintil_y_año.xlsx")


##### GRAFICAR #####





