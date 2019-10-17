# IDENTIFICACIÓN DE PALABRAS CLAVE SOBRE EQUIDAD DE GÉNERO ====

# Departamento Nacional de Planeación - Dirección de Desarrollo Digital
# Unidad de Científicos de Datos
# Contacto: ucd@dnp.gov.co

# 1. CONFIGURACIÓN DEL ENTORNO ====

# Inicialmente limpiamos el entorno y cargamos las librerías necesarias.

remove(list=ls())

library(rstudioapi) 
library(dplyr)
library(curl)
library(pdftools)
library(tm)
library(quanteda)

setwd(dirname(getActiveDocumentContext()$path))

# 2. FUNCIONES ====

## 2.1 Preprocesamiento de texto ----

# La función preprocesar_texto permite realizar la limpieza de cadenas de texto:
# - Remueve caracteres no imprimibles
# - Pasa el texto a minúsculas
# - Elimina stopwords
# - Elimina números y espacios consecutivos
# - Elimina los espacios al inicio y al final de la cadena

preprocesar_texto <- function(texto){
  texto_preprocesado <- texto %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    removeWords(., tm::stopwords(kind = "es"))%>%
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(texto_preprocesado)
}

# 3. DESCARGA, LECTURA Y LIMPIEZA DE DOCUMENTOS ====

## 3.1 Definición de los enlaces de descarga ----

enlaces_genero <- c("https://colaboracion.dnp.gov.co/CDT/Conpes/Social/161.pdf",
                    "http://www.equidadmujer.gov.co/ejes/Documents/Evaluacion-Equidad_de_Genero-Conpes_161.pdf",
                    "http://www.equidadmujer.gov.co/oag/Documents/Evaluacion-Mujeres-Victimas-2017-CONPES_3784.pdf")

enlaces_otros_sectores <- paste0("https://colaboracion.dnp.gov.co/CDT/Conpes/Econ%C3%B3micos/",
                                 c(3916, 3900 ,3963, # transporte
                                   3812, 3803, 3783, # cultura, deporte y recreación
                                   3914, 3831, 3809, # educación
                                   3897, 3859, 3848, # vivienda
                                   3798, 3780, 3715, # agua potable y saneamiento básico
                                   3811, 3763, 3675, # agricultura (agropecuario)
                                   3867, 3850, 3932, # inclusión social y reconciliación
                                   3716, 3700, 3697, # ambiente y desarrollo sostenible
                                   3887, 3755, 3622, # salud y protección social
                                   3873, 3510, 3347, # minas y energía
                                   3898, 3854, 3769, # telecomunicaciones
                                   3866, 3771, 3709), # comercio, industria y turismo,
                                 ".pdf")

enlaces <- c(enlaces_genero, enlaces_otros_sectores)
             
etiquetas_documentos <- c(rep("Género", length(enlaces_genero)), rep("Otros", length(enlaces_otros_sectores)))

## 3.2 Descarga, lectura y limpieza ----

dir.create("Insumos")

for(i in 1:length(enlaces)){
  curl_download(enlaces[i], destfile = paste0("Insumos/", i, ".pdf"))  
}

documentos_limpios <- vector(mode = "list", length = length(enlaces))

for(i in 1:length(enlaces)){
  texto <- pdf_text(paste0("Insumos/", i, ".pdf"))
  texto_limpio <- preprocesar_texto(texto) 
  documentos_limpios[[i]] <- texto_limpio
}
  
etiquetas_paginas <- rep(etiquetas_documentos, sapply(documentos_limpios, length))

# 4. VECTORIZACIÓN DE TEXTOS CON EL MODELO BAG OF WORDS (BOW) ----

paginas_limpias <- unlist(documentos_limpios)
bag_of_words <- dfm(paginas_limpias)
bag_of_words <- dfm_trim(bag_of_words, min_termfreq = 5)
bag_of_words <- convert(bag_of_words, to = "matrix")

# 5. IDENTIFICACIÓN DE PALABRAS CLAVE ----

bow_genero <- bag_of_words[etiquetas_paginas == "Género", ]
bow_otros_sectores <- bag_of_words[etiquetas_paginas == "Otros", ]

# 5.1 Palabras exclusivas ----
palabras_exclusivas_genero <- colSums(bow_genero[, colSums(bow_otros_sectores) == 0])

View(sort(palabras_exclusivas_genero, decreasing = T))

# 5.2 Palabras representativas ----
bow_genero <- bow_genero[, colSums(bow_otros_sectores) != 0]
bow_otros_sectores <- bow_otros_sectores[, colSums(bow_otros_sectores) != 0]
  
componentes_principales <- prcomp(bow_otros_sectores)

proyeccion <- ((bow_genero %*% componentes_principales$rotation) %*% t(componentes_principales$rotation))
diferencias <- colSums(bow_genero - proyeccion)
  
View(sort(diferencias, decreasing = TRUE))

