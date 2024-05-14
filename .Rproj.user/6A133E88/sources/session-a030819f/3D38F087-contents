# 1. Exploración inicial ----
options(scipen=999)
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, tidyverse)
datos <- read_excel("input/base.xlsx", sheet = "areaverde-hab")
view(datos)
dim(datos)

# 2. Selección de variables ----
proc_data <- datos %>% select(Comuna,
                                            `Superficie de Área Verde (m2)`, 
                                            `Superficie Plazas (m2)`, 
                                            `BPU_29 Superficie de Área Verde por Habitantes (m2/Hab)`,
                              Región,
                              Cod_Ciudad) 

## 2. 1 Comprobar ----
names(proc_data)

sjlabelled::get_label(proc_data) #Observar las etiquetas de las variables
view(proc_data) #Ver data procesada 
proc_data <- proc_data %>% dplyr::filter(Cod_Ciudad==13001) #Filtrar por región (metropolitana)

# PROCESAMIENTO ----- 
frq(proc_data$`BPU_29 Superficie de Área Verde por Habitantes (m2/Hab)`)
proc_data <- proc_data %>% rename("comuna"=Comuna, 
                                  "region"=Región,
                                  "sup_averde"=`Superficie de Área Verde (m2)`,
                                  "sup_plaza"=`Superficie Plazas (m2)`,
                                  "sup_hab"=`BPU_29 Superficie de Área Verde por Habitantes (m2/Hab)`)


proc_data2 <- proc_data %>% select(comuna,
                                   sup_averde,
                                   sup_plaza)

# Filtrar el dataframe para incluir solo las filas donde la columna "comuna" coincide con algunas comunas específicas
proc_data2 <- filter(proc_data2, comuna %in% c("PROVIDENCIA",
                                               "SAN MIGUEL",
                                               "LAS CONDES",
                                               "VITACURA",
                                               "ÑUÑOA",
                                               "RENCA",
                                               "LA PINTANA",
                                               "MACUL"))
proc_data3 <- proc_data %>% select(comuna,
                                   sup_hab)


frq(proc_data2$sup_averde)
frq(proc_data2$sup_plaza)
frq(proc_data3$sup_hab)

sjmisc::descr(proc_data2,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) # Selecciona estadísticos
sjmisc::descr(proc_data3,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))


summarytools::dfSummary(proc_data3, plain.ascii = FALSE)
proc_data2 %>% ggplot(aes(x = comuna)) + 
  geom_bar()

