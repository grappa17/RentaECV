# Notas

No estoy acostumbrado a trabajar con Github así que simplemente subo las cosas para replicar mi Scipt aquí, porque no lo tengo vinculado directamente a Github

El objetivo del script realmente es analizar como ha evolucionado la renta real en el Estado español, creando distintos grupos (deciles, quintiles...), y comprobando su evolución en el tiempo. Derivado de eso, extraer también algunas medidas de desigualdad. La cuestión es que, como me están saliendo resultados inesperados y que no me terminan de cuadrar (una desigualdad más baja de lo esperado, rentas del bottom 50% relativamente elevadas, y del top 10% relativamente bajas), estoy comprobando primero si puedo replicar los datos de la ECV, en concreto la evolución de la desigualdad de ingresos S80/S20 (disponible en: https://www.ine.es/jaxiT3/Datos.htm?t=59970#_tabs-grafico). Para hacerlo, verás qque utilizo la variable "renta_INE", que no es más que la propia variable que proporciona la encuesta "vhRentaAIa", y que no la deflacto, pues el INE no lo hace. Sin embargo, no logro replicar el resultado. 

PD: El script contiene más cosas, he estado trasteando cosas con los alquileres y así, pero eso no es relevante. 
