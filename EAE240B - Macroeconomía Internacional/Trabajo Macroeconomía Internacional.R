
# Trabajo Macroeconomía Internacional
# Vicente Jaramillo y José Luis Valdés

library(kableExtra)
library(knitr)
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(lubridate)
library(readr)

#Pregunta 2.3

vix_2_3 <- read_excel("C:/Users/vctja/Downloads/P2/vix_2_3.xls", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))

##
# Nos aseguramos de que 'vix_close' sea numérico y luego calcula su logaritmo
vix_2_3$vix_close <- as.numeric(df$vix_close)
vix_2_3$log_vix <- log(df$vix_close)

# Calculamos la variación del tipo de cambio (usando logaritmos) para cada país
df <- vix_2_3 %>%
  mutate(Chile_e_diff = log(Chile_e / lag(Chile_e)),
         Colombia_e_diff = log(Colombia_e / lag(Colombia_e)),
         Hungary_e_diff = log(Hungary_e / lag(Hungary_e)),
         Mexico_e_diff = log(Mexico_e / lag(Mexico_e)),
         South_Africa_e_diff = log(South_Africa_e / lag(South_Africa_e)))

# Eliminamos las filas con NA que pueden haberse creado debido al cálculo de logaritmos de la variación
df <- na.omit(df)


# Definimos los modelos de regresión para cada país
model_Chile <- lm(Chile_e_diff ~ log_vix, data = df)
model_Colombia <- lm(Colombia_e_diff ~ log_vix, data = df)
model_Hungary <- lm(Hungary_e_diff ~ log_vix, data = df)
model_Mexico <- lm(Mexico_e_diff ~ log_vix, data = df)
model_South_Africa <- lm(South_Africa_e_diff ~ log_vix, data = df)

# modelos
tab_model(model_Chile)
tab_model(model_Colombia) 
tab_model(model_Hungary) 
tab_model(model_Mexico)
tab_model(model_South_Africa)
##


#Pregunta 3

##3.2
# Leemos los datos
embi_i_tc <- read_excel("C:/Users/vctja/Downloads/p3/embi + i + tc.xlsx", 
                        col_types = c("date", "date", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
names(embi_i_tc)
num_vars <- embi_i_tc %>%
  select(Chile_embi, China_embi, Colombia_embi, Hungary_embi, Mexico_embi, South_africa_embi,
         Chile_i, Colombia_i, Hungary_i, Mexico_i, South_Africa_i, United_states_i,
         Hungary_e, Chile_e, Colombia_e, Mexico_e, `South Africa_e`)

# Creamos una tabla con el promedio y la desviación estándar de cada variable
summary_table <- num_vars %>%
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE)

# Imprimimos la tabla con knitr::kable
tabla <- knitr::kable(summary_table, digits = 2)

##

# Seleccionamos las variables numéricas
num_vars <- embi_i_tc %>%
  select(Chile_embi, China_embi, Colombia_embi, Hungary_embi, Mexico_embi, South_africa_embi,
         Chile_i, Colombia_i, Hungary_i, Mexico_i, South_Africa_i, United_states_i,
         Hungary_e, Chile_e, Colombia_e, Mexico_e, `South Africa_e`)

# Creamos una tabla con el promedio y la desviación estándar de cada variable
summary_table <- num_vars %>%
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE)

# Creamos la tabla con knitr::kable y kable_styling
knitr::kable(summary_table, digits = 2, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## 3.3


# Cargamos las bibliotecas necesarias
library(dplyr)
library(readxl)
library(sjPlot)

# Leemos los datos
embi_i_tc <- read_excel("C:/Users/vctja/Downloads/p3/embi + i + tc.xlsx", 
                        col_types = c("date", "date", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))

# Calculamos la depreciación para cada país y corremos la regresión
# Para Chile
embi_i_tc <- embi_i_tc %>%
  mutate(Chile_depreciation = log(lead(Chile_e)) - log(Chile_e))
model_chile <- lm(lead(Chile_depreciation, 3) ~ Chile_i - United_states_i + Chile_embi, data = embi_i_tc)

# Para Colombia
embi_i_tc <- embi_i_tc %>%
  mutate(Colombia_depreciation = log(lead(Colombia_e)) - log(Colombia_e))
model_colombia <- lm(lead(Colombia_depreciation,3) ~ Colombia_i - United_states_i + Colombia_embi, data = embi_i_tc)

# Para Hungría
embi_i_tc <- embi_i_tc %>%
  mutate(Hungary_depreciation = log(lead(Hungary_e)) - log(Hungary_e))
model_hungary <- lm(lead(Hungary_depreciation,3) ~ Hungary_i - United_states_i + Hungary_embi, data = embi_i_tc)

# Para México
embi_i_tc <- embi_i_tc %>%
  mutate(Mexico_depreciation = log(lead(Mexico_e)) - log(Mexico_e))
model_mexico <- lm(lead(Mexico_depreciation,3) ~ Mexico_i - United_states_i + Mexico_embi, data = embi_i_tc)

# Para Sudáfrica
embi_i_tc <- embi_i_tc %>%
  mutate(South_Africa_depreciation = log(lead(`South Africa_e`)) - log(`South Africa_e`))
model_south_africa <- lm(lead(South_Africa_depreciation,3) ~ South_Africa_i - United_states_i + South_africa_embi, data = embi_i_tc)

# Imprimimos los resúmenes de los modelos
print(summary(model_chile))
print(summary(model_colombia))
print(summary(model_hungary))
print(summary(model_mexico))
print(summary(model_south_africa))

# Creamos una tabla de los modelos para cada país
tab_model(model_chile)
tab_model(model_colombia)
tab_model(model_hungary)
tab_model(model_mexico)
tab_model(model_south_africa)

#Pregunta 4


# importamos data

library(readxl)
pregunta_4 <- read_excel("C:/Users/vctja/Downloads/P4/hoja_1_1.xlsx")


#arreglamos las variables

names(pregunta_4)[names(pregunta_4) == "South Africa"] <- "South_Africa"
pregunta_4$date <- as.Date(paste(pregunta_4$date, "01", sep = "-"), format = "%Y-%m-%d")


#cambio absoluto
# 
# pregunta_4_1 <- pregunta_4 %>%
#   arrange(date) %>%
#   mutate(Hungary = Hungary - lag(Hungary, 1),
#          Chile = Chile - lag(Chile, 1),
#          Colombia = Colombia - lag(Colombia, 1),
#          Mexico = Mexico - lag(Mexico, 1),
#          South_Africa = South_Africa - lag(South_Africa, 1)) # calcula la diferencia

#cambio relativo

pregunta_4_2 <- pregunta_4 %>%
  arrange(date) %>%
  mutate(Hungary = log(Hungary) - log(lag(Hungary)),
         Chile = log(Chile) - log(lag(Chile)),
         Colombia = log(Colombia) - log(lag(Colombia)),
         Mexico = log(Mexico) - log(lag(Mexico)),
         South_Africa = log(South_Africa) - log(lag(South_Africa)))

# Luego calculamos el rezago de esta diferencia
pregunta_4_2 <- pregunta_4_2 %>%
  mutate(Hungary_lag = lag(Hungary, 1),
         Chile_lag = lag(Chile, 1),
         Colombia_lag = lag(Colombia, 1),
         Mexico_lag = lag(Mexico, 1),
         South_Africa_lag = lag(South_Africa, 1))

# Ahora podemos hacer las regresiones con el término rezagado
modelo_Hungary <- lm(Hungary ~ Hungary_lag + sorpmon, data = pregunta_4_2)
modelo_Chile <- lm(Chile ~ Chile_lag + sorpmon, data = pregunta_4_2)
modelo_Colombia <- lm(Colombia ~ Colombia_lag + sorpmon, data = pregunta_4_2)
modelo_Mexico <- lm(Mexico ~ Mexico_lag + sorpmon, data = pregunta_4_2)
modelo_South_Africa <- lm(South_Africa ~ South_Africa_lag + sorpmon, data = pregunta_4_2)

library(sjPlot)

tab_model(modelo_Hungary, modelo_Chile, modelo_Colombia)
tab_model(modelo_Mexico, modelo_South_Africa)


## resultados sin rezago

# Ajustar los modelos sin término de rezago
modelo_Hungary_nolag <- lm(Hungary ~ sorpmon, data = pregunta_4_2)
modelo_Chile_nolag <- lm(Chile ~ sorpmon, data = pregunta_4_2)
modelo_Colombia_nolag <- lm(Colombia ~ sorpmon, data = pregunta_4_2)
modelo_Mexico_nolag <- lm(Mexico ~ sorpmon, data = pregunta_4_2)
modelo_South_Africa_nolag <- lm(South_Africa ~ sorpmon, data = pregunta_4_2)

tab_model(modelo_Hungary_nolag, modelo_Chile_nolag, modelo_Colombia_nolag)
tab_model(modelo_Mexico_nolag, modelo_South_Africa_nolag)


