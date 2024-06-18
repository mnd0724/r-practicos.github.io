
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,
               haven,
               GGally,
               ggpubr,
               textreg,
               car,
               fastDummies,
               sjlabelled,
               ggeffects,
               texreg) 

# Para la mayoría de los gráficos
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica#Cargar base de datos

data <- read_dta("input/ELSOC_W01_v4.01_Stata14.dta")
View(data)
dim(data) #2927 428
names(data)

proc_data <- data %>% select(t06_01,
                             t06_02,
                             t06_03,
                             t06_04,
                             t06_06,
                             t08,
                             t10,
                             t11_04,
                             m29,
                             comuna,
                             region,
                             m0_edad,
                             m0_sexo)
view(proc_data)
sjmisc::descr(proc_data)
proc_data <- proc_data %>% rename("sats_seguridad" =t06_01,
                                  "sats_conectividad"=t06_02, # Satisfacción conectividad
                                  "sats_averde"=t06_03, # Satisfacción área verde
                                  "sats_limpieza"=t06_04,# Satisfacción Limpieza
                                  "sats_colegios"=t06_06, #satisfacción cercanía a colegios de buena calidad 
                                  "percp_barrio"=t08,
                                  "percp_seguridad"=t10,
                                  "frq_vecinosbasura"=t11_04,
                                  "ingreso"=m29,
                                  "edad"=m0_edad,
                                  "sexo"=m0_sexo)

#VER LOS NOMBRES DE LAS VARIABLES
names(proc_data)

#Filtrar por comunas de la región metropolitana
proc_data <- proc_data %>% dplyr::filter(region == "Metropolitana") # Filtrar por región (
proc_data <- filter(proc_data, comuna %in% c("Providencia",
                                             "Colina",
                                             "Maipu",
                                             "Las Condes",
                                             "Santiago",
                                             "Puente Alto",
                                             "La Florida",
                                             "San Bernardo"))

#La selección de estas comunas se hace respecto a la cantidad
#de personas encuestadas, y que sean lo más cercano posible en 
#en número de personas que participaron.

sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE)) #Tabla como imagen
sum(is.na(proc_data))
proc_data <- proc_data %>%
  filter_all(all_vars(!is.na(.) & . != -999 & . != -888)) #Filtrar NA'S

summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))  #SIN NA´S

#GRÁFICOS
proc_data %>% ggplot(aes(x = sats_seguridad)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción Seguridad en el Barrio",
       x = "Satisfacción Seguridad",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = sats_conectividad)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción Conectividad",
       x = "Satisfacción Conectividad",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = sats_averde)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción área verde",
       x = "Satisfacción área verde",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = sats_limpieza)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción Limpieza y Belleza en el Barrio",
       x = "Satisfacción Limpieza",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = sats_colegios)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción cercanía a colegios de buena calidad",
       x = "Satisfacción cercanía a colegios de buena calidad",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = percp_barrio)) + 
  geom_bar(fill = "coral")+
  labs(title = "Percepción Evaluación del Barrio",
       x = "Percepción Evaluación del Barrio",
       y = "Frecuencia")

proc_data %>% ggplot(aes(x = frq_vecinosbasura)) + 
  geom_bar(fill = "coral")+
  labs(title = "Satisfacción Limpi",
       x = "Satisfacción Limpieza",
       y = "Frecuencia")


ggpairs(proc_data)

#2. ENTREGA 3 -----

proc_data %>% dplyr::select(sats_seguridad, sats_conectividad, sats_averde, sats_limpieza) %>% 
  sjPlot::plot_stackfrq()+
  theme(legend.position = "bottom")

view(dfSummary(proc_data, headings=FALSE, graph.col = FALSE))
cor(proc_data$sats_averde, proc_data$ingreso)
cor(proc_data$sats_conectividad, proc_data$ingreso)
cor(proc_data$sats_limpieza, proc_data$ingreso)
na.omit(proc_data)

# Crear un vector con los nombres de las comunas
nombres_comunas <- c("Providencia", "Colina", "Las Condes", "Puente Alto", "La Florida", "Santiago", "Maipu", "San Bernardo")

# Ordenar alfabéticamente los nombres de las comunas
nombres_comunas <- sort(nombres_comunas)

# Crear un vector con los valores numéricos en orden alfabético
valores_numericos <- 1:length(nombres_comunas)

# Crear una nueva columna en el dataframe para asignar los valores numéricos
proc_data$comunas_numericas <- NA

# Asignar valores numéricos a las comunas
for (i in 1:length(nombres_comunas)) {
  proc_data$comunas_numericas[proc_data$comuna == nombres_comunas[i]] <- valores_numericos[i]
}

graph4 <- ggplot(proc_data, aes(x = comuna, y = sats_averde, fill = comuna)) +
  geom_bar(stat = "identity") +  # Utilizar stat = "identity" para usar los valores exactos de y
  labs(x = "Comuna", y = "Satisfacción con áreas verdes", title = "Satisfacción con áreas verdes por comuna") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x para mejor legibilidad

#calcular el valor máximo en Ingreso
max_valor <- max(proc_data$ingreso)
#Imprimir el valor máximo
print(max_valor)

# Calcular el valor mínimo en la variable
min_valor <- min(proc_data$ingreso)
# Imprimir el valor mínimo
print(min_valor)

graph5 <- ggplot(proc_data, aes(x = comuna, y = ingreso)) +
  geom_point() +  # Agregar puntos al gráfico
  labs(x = "Comuna", y = "Ingresos", title = "Gráfico de Dispersión de Ingresos por Comuna") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar las etiquetas del eje x para mejor legibilidad
  ylim(0, 8000000)

# Crear el gráfico de barras con escala logarítmica en el eje y y etiquetas personalizadas
graph6 <- ggplot(proc_data, aes(x = comuna, y = ingreso, fill = comuna)) +
  geom_bar(stat = "identity") +  # Utilizar stat = "identity" para usar los valores exactos de y
  labs(x = "Comuna", y = "Ingresos", title = "Ingresos por Comuna") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar las etiquetas del eje x para mejor legibilidad
  scale_y_continuous(labels = scales::comma)  # Etiquetas personalizadas para los valores reales en el eje y

proc_data %>% dplyr::select(sats_seguridad, sats_conectividad, sats_averde, sats_limpieza) %>% 
  sjPlot::plot_stackfrq()+
  theme(legend.position = "bottom")

proc_data %>% dplyr::select(percp_barrio, percp_seguridad) %>% 
  sjPlot::plot_stackfrq()+
  theme(legend.position = "bottom")

proc_data %>% dplyr::select(frq_vecinosbasura) %>% 
  sjPlot::plot_stackfrq()+
  theme(legend.position = "bottom")

psych::alpha(dplyr::select(proc_data, sats_seguridad, sats_conectividad, sats_averde, sats_limpieza, sats_colegios))

#Sabemos que los valores de alfa van desde 0 y 1, cuando los valores
#son más cercanos a 1, existe una mayor confiabilidad 
#cuando el valor es mayor a 0.7 se considera como un valor aceptable
#Observamos que la variable
#Observamos que con estos 5 casos el raw.alpha es de 0.75
#lo que quiere decir que hay una confiabilidad aceptable, si quitaramos alguno
#de estos items la confianza disminuiría, así que se pueden construir
#una escala con ellas.

proc_data <- proc_data %>%
  rowwise() %>% 
  mutate(calidad_vida = sum(sats_limpieza, sats_seguridad, sats_conectividad, sats_averde, sats_colegios))
summary(proc_data$calidad_vida)

graph3 <- ggplot(proc_data, aes(x = calidad_vida)) +
  geom_histogram(binwidth=0.6, colour="black", fill="pink") +
  theme_bw() +
  xlab("Calidad de vida") +
  ylab("Cantidad")

proc_data2 <- proc_data %>% select(calidad_vida, ingresos=ingreso, comuna=comuna)

#Grafico x1 = ACT
graph1 <- ggplot(proc_data2, aes(x = ingresos, y = calidad_vida)) +
  geom_point(size = 1) +  # Puntos
  geom_smooth(method = "lm", se = FALSE) +  # Recta de regresión
  labs(x = "Ingresos", y = "Calidad de Vida")  # Etiquetas de ejes

# Gráfico 2
graph2 <- ggplot(proc_data2, aes(x = comuna, y = calidad_vida)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Comuna", y = "Calidad de Vida")
ggarrange(graph1, graph2, nrow = 1) # Unir graficos


#ENTREGA 4 
proc_data <- proc_data %>%
  filter_all(all_vars(!is.na(.) & . != -999 & . != -888)) #Filtrar NA'S
view_df(proc_data,max.len = 50)

# Definir los límites del salario mínimo
salario_minimo <- 460000

frq(proc_data$grupo_ingreso)
proc_data$grupo_ingreso <- cut(
  proc_data$ingreso,
  breaks = c(-Inf, salario_minimo, 3 * salario_minimo, Inf),
  labels = c("Bajo", "Medio", "Alto"),
  right = TRUE
)

frq(proc_data$grupo_ingreso)

proc_data$ingreso <- as_factor(proc_data$ingreso)
proc_data$sexo <- as_factor(proc_data$sexo)
proc_data <- na.omit(proc_data)
reg1 <- lm(calidad_vida ~ 1, data=proc_data)
stargazer(reg1, type="text")

#ANALISISI



#REGRESIÓN LINEAL SIMPLE
reg2 <- lm(calidad_vida ~ edad, data=proc_data)
reg3 <- lm(calidad_vida ~ grupo_ingreso, data=proc_data)
reg4 <- lm(calidad_vida ~ sexo, data=proc_data)

knitreg(list(reg2, reg3, reg4), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Edad",
                              "Ingreso Medio <br> <i>(Ingreso Bajo)</i>", 
                              "Ingreso Alto", 
                              "Mujer <br> <i>(Ref. Hombre)</i>"),
        caption = "Calidad de vida",
        caption.above = TRUE)







#REGRESION LINEAL MULTIPLE
reg5 <- lm(calidad_vida ~ edad + grupo_ingreso, data=proc_data)
reg6 <- lm(calidad_vida ~ edad + sexo, data=proc_data)
reg7 <- lm(calidad_vida ~ grupo_ingreso + sexo, data=proc_data)
reg8 <- lm(calidad_vida ~ edad + grupo_ingreso + sexo, data=proc_data)

knitreg(list(reg5, reg6, reg7, reg8), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2",
                               "Modelo 3",
                               "Modelo 4"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Edad",
                              "Ingreso Medio <br> <i>(Ref. Ingreso Bajo)</i>", 
                              "Ingreso Alto", 
                              "Mujer <br> <i>(Ref. Hombre)</i>"),
        caption = "Calidad de vida",
        caption.above = TRUE)


graph7 <- plot_model(reg8, 
           title = "", #quitar titulo
           show.values = TRUE, #mostrar valor de efectos
           dot.size = 3, #tamaño circulos
           line.size = 1, #tamaño CI
           value.size = 4, #tamaño valor efectoss
           spacing = 1, #espacio entre efectos
           vline.color = "red", # linea roja en punto neutro (0)
           axis.labels = rev(c("Edad",
                               "Ingresos", 
                               "Mujer")), #con rev porque automatico los tira en otro orden
           show.legend = FALSE) + # variables dependientes
  theme_bw()
ggsave(graph1, file="output/graphs/graph1.png")
ggsave(graph2, file="output/graphs/graph2.png")
ggsave(graph3, file="output/graphs/graph3.png")
ggsave(graph4, file="output/graphs/graph4.png")
ggsave(graph5, file="output/graphs/graph5.png")
ggsave(graph6, file="output/graphs/graph6.png")
ggsave(graph7, file="output/graphs/graph7.png")
