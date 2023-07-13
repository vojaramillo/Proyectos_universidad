###############################################################################
## Tarea 01 - Machine Learning para negocios                                 ##
##                                      Integrantes:                         ##
##                                           -Vicente Jaramillo              ##
##                                           -José Vilchez                   ##
###############################################################################

## En primer lugar importamos las librerías a usar junto con el dataframe a
## utilizar:

if (!require('corrr')) install.packages('corrr'); library(corrr)
if (!require('polycor')) install.packages('polycor'); library(polycor)
if(!require('ggpubr')) install.packages('ggpubr'); library(ggpubr)
if(!require('car')) install.packages('car'); library(car)
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse') #Incluye dyplr y ggplot
if (!require('tree')) install.packages('tree'); library('tree')       #Librería de clases para hacer el árbol de decisión
if (!require('moments')) install.packages('moments'); library('moments')  
if (!require('ggpubr')) install.packages('ggpubr'); library('ggpubr') #Para juntar gráficos
if (!require('randomForest')) install.packages('randomForest'); library('randomForest') #Para hacer Bagging
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('corrplot')) install.packages('corrplot'); library('corrplot') #Para gráfico de correlación
if (!require('vcd')) install.packages('vcd'); library('vcd') #Para graficar matriz de confusión en mosaico
if(!require('MLmetrics')) install.packages('MLmetrics'); library("MLmetrics")
if(!require('gamlss')) install.packages('gamlss'); library(gamlss)
library(readxl)
library(tidymodels)
library(ggplot2)
library(skimr)
library(dplyr)
library(readr)
library(reshape2)
library(gridExtra)
library(gt)
library(corrr)
library(readxl)
library(vcd)
library(ISLR)
library(caret)
library(scales)
library(DescTools)
library(ResourceSelection)
library(recipes)
library(ipred)
library(moments)

## Se importa lo que es el xlsx con la información en un df
transit_cost_full = read_excel("transit_cost.xlsx")

transit_cost = transit_cost_full %>%
  dplyr::select(country, city, line, start_year, end_year, rr, length,
                tunnel_per, tunnel, stations, cost, currency,
                year, ppp_rate, real_cost, cost_km_millions) %>%
  dplyr::mutate(rr = dplyr::recode(rr, `0` = 'No', `1` = 'Yes')) %>%
  dplyr::mutate(across(where(is.character), as_factor)) %>% 
  dplyr::mutate_if(is.numeric, list( ~ ifelse(is.na(.), 0, .))) %>% 
  dplyr::mutate(tunnel_per = tunnel/length * 100)

dplyr::glimpse(transit_cost)

##### Pregunta 1: ####
#### 1.a.) ####

###variables categoricas

## real_cost vs. country
ggplot(data = transit_cost, mapping = aes(country, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() + 
  labs(x = 'País',
       y = 'Costo real en millones de dólares',
       title = 'Relación País y costo real')

### real_cost v/s city
transit_cost %>% 
  ggplot(aes(real_cost, city, color = "default")) + 
  geom_point(alpha = 0.4) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Default status by income and balance")+
  theme_classic()+
  theme(axis.text.y = element_text(size = 6))             # y-axis text size

## real_cost vs. rr:
transit_cost %>% 
  dplyr::count(rr) %>%
  dplyr::mutate(prop = n/sum(n))

ggplot(data = transit_cost, mapping = aes(x = rr, y = real_cost, fill = rr)) +
  geom_boxplot() +
  theme_light()+
  labs(x = 'Es un ferrocarril', y = 'Costo real en millones de dólares',
       title = 'Relación costo real y si es ferrocarril')

## real_cost vs. currency
plot8.1 = ggplot(data = transit_cost, mapping = aes(currency, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_light()+
  labs(x = 'Tipo de cambio monetario',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre tipo de cambio monetario y costo real')
plot8.1

### Variables continuas ###

## real_cost vs. start_year
plot1 = ggplot(data = transit_cost, mapping = aes(start_year, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() +
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Año iniciado',
       y = 'Costo real en millones de dólares',
       title = 'Relación año inicio y costo real')
plot1

transit_cost %>% 
  dplyr::select(start_year, real_cost) %>%
  cor()
## correlación del 0.12

## real_cost vs. end_year
plot2 = ggplot(data = transit_cost, mapping = aes(end_year, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() + 
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Año termino',
       y = 'Costo real en millones de dólares',
       title = 'Relación año termino y costo real')
plot2

transit_cost %>% 
  dplyr::select(end_year, real_cost) %>%
  cor()
## correlación del 0.21

## real_cost vs. length
plot3 = ggplot(data = transit_cost, mapping = aes(length, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() + 
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Largo de la linea (en km.)',
       y = 'Costo real en millones de dólares',
       title = 'Relación largo de la linea y costo real')
plot3

transit_cost %>% 
  dplyr::select(length, real_cost) %>%
  cor()
## correlación del 0.73

## real_cost vs. tunnel_per
plot4 = ggplot(data = transit_cost, mapping = aes(tunnel_per, real_cost)) +
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() + 
  labs(x = 'Porcentaje de la linea completada',
       y = 'Costo real en millones de dólares',
       title = 'Relación porcentaje de la linea completada y costo real')
plot4

transit_cost %>% 
  dplyr::select(tunnel_per, real_cost) %>%
  cor()

## real_cost vs. tunnel
plot5 = ggplot(data = transit_cost, mapping = aes(tunnel, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  theme_classic() + 
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Kilometros de linea completada',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre Kilometros de linea completada y costo real')
plot5

transit_cost %>% 
  dplyr::select(tunnel, real_cost) %>%
  cor()
## correlación del 0.69

## real_cost vs. stations
plot6 = ggplot(data = transit_cost, mapping = aes(stations, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic() + 
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Número de estaciones donde los pasajeros pueden embarcar/salir',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre número de estaciones donde los pasajeros pueden embarcar/salir y costo real')
plot6

transit_cost %>% 
  dplyr::select(stations, real_cost) %>%
  cor()
## correlación de 0.52

## real_cost vs. cost
plot7 = ggplot(data = transit_cost, mapping = aes(cost, real_cost)) +
  geom_point() +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic()+
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Costo en millones de moneda local',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre costo en millones de moneda local y costo real')
plot7

transit_cost %>% 
  dplyr::select(cost, real_cost) %>%
  cor()
## correlación del 0.014

## real_cost vs. year
plot9 = ggplot(data = transit_cost, mapping = aes(year, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic()+
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Año medio de construcción',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre año medio de construcción y costo real')
plot9

transit_cost %>% 
  dplyr::select(year, real_cost) %>%
  cor()
## Correlación del 0.12

## real_cost vs. ppp_rate --> Hay 2 filas nulas

plot10 = ggplot(data = transit_cost, mapping = aes(ppp_rate, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic()+
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Paridad del poder adquisitivo',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre paridad del poder adquisitivo y costo real')
plot10

transit_cost %>% 
  dplyr::select(ppp_rate, real_cost) %>%
  cor()

## real_cost vs. cost_km_millions --> Hay 2 filas nulas
plot11 = ggplot(data = transit_cost, mapping = aes(cost_km_millions, real_cost)) +
  geom_point(color = '#006EA0', alpha = 0.25) +
  theme_classic()+
  stat_smooth(method = 'lm', se = FALSE, col = 'tomato', formula = y ~ x) +
  labs(x = 'Costo/km en millones de USD',
       y = 'Costo real en millones de dólares',
       title = 'Relación entre Costo/km en millones de USD y costo real')
plot11

transit_cost %>% 
  dplyr::select(cost_km_millions, real_cost) %>%
  cor()


## Gráficos:
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
grid.arrange(plot5, plot6, plot7, plot9, nrow = 2, ncol = 2)
grid.arrange(plot10, plot11, nrow = 1, ncol = 2)

## Correlación policorica??
polycor::polychor(transit_cost$real_cost, transit_cost$country)
polycor::polychor(transit_cost$real_cost, transit_cost$city)
polycor::polychor(transit_cost$real_cost, transit_cost$rr)
polycor::polychor(transit_cost$real_cost, transit_cost$line)
polycor::polychor(transit_cost$real_cost, transit_cost$currency)


#### 1.b.) ####

transit_cost2 <- transit_cost[, c(4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16)]

x <- transit_cost2 %>% 
  correlate() %>% 
  focus(real_cost)
x
x %>% 
  mutate(term = factor(term, levels = term[order(real_cost)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = real_cost)) +
  theme_light()+
  geom_bar(stat = "identity", fill = "#66d9ff") +
  ylab("Correlación con real_cost") +
  xlab("Variables") +
  ggtitle("Correlación de variables con real_cost")+
  geom_hline(yintercept = 0.3, col='tomato')

#### 1.c.) ####
set.seed(3707)

cost_split <- rsample::initial_split(transit_cost, prop = 0.75, strata = stations)

## Los set de datos:
# Entrenamiento
data_train  = rsample::training(cost_split)

# Prueba
data_test  = rsample::testing(cost_split)

## Distribution of real_cost en:
# Data Train
data_train %>% 
  dplyr::summarize(min  = min(real_cost),
                   max = max(real_cost),
                   mean  = mean(real_cost),
                   sd  = sd(real_cost))

# Data Test
data_test %>%
  dplyr::summarize(min  = min(real_cost),
                   max = max(real_cost),
                   mean  = mean(real_cost),
                   sd  = sd(real_cost))

## Se define el modelo:
lm_model  =  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

## Se realiza la receta:
lm_recipe = recipe(real_cost ~ stations + tunnel + length, data = data_train) %>%  
  step_normalize(all_numeric(), -all_outcomes())

summary(lm_recipe)

## Se define el Workflow
lm_workflow =  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_recipe)


## Se realiza el fit del workflow
train_fit =  lm_workflow %>% 
  fit(data = data_train)

## Ajuste del modelo de regresión lineal múltiple y sus coeficientes estimados:
tidy(train_fit)

#### Reporte del MAPE, R2
predictions = predict(train_fit, new_data = data_test)

test_results  =  data_test %>% 
  bind_cols(predictions) %>% 
  bind_cols(predict(train_fit, new_data = data_test, type = 'conf_int'))
head(test_results)

## RMSE y RSQ del testeo
test_results %>% 
  rmse(estimate=.pred, truth= real_cost) ## RMSE de 2686.

test_results %>% mape(truth = real_cost, estimate=.pred) ## MAPE INF

mape2 = abs(test_results$real_cost - test_results$.pred)/test_results$real_cost
for(x in 1:length(mape2)){
  if(is.infinite(mape2[x])){
    mape2[x] = 0
  }
}
mean(mape2) ## MAPE omitiendo los infinitos es de 0.7772809


test_results %>% 
  rsq(estimate = .pred, truth = real_cost) ## RSQ de 0.828.

## Gráfico de dispersión entre el costo real del proyecto y su valor predicho
test_results = test_results %>% 
  ggplot(mapping = aes(x = .pred, y = real_cost)) +
  theme_light() +
  geom_point(color = '#006EA0', alpha = 0.40) +
  geom_abline(intercept = 0, slope = 1, color = 'tomato') +
  labs(title = 'Resultados de la Regresión Lineal - Test Set',
       x = 'Costos Reales predecidos',
       y = 'Costos Reales del proyecto')
test_results


## Ajustado nuevamente con 10-fold cross super validation 5000:
## Empezamos con la seed
set.seed(3707)

## Hacemos los folds
data_folds = vfold_cv(data_train, v=10, strata = stations)

doParallel::registerDoParallel()
ctrl_pred = control_resamples(save_pred = TRUE)
fit_folds = fit_resamples(lm_workflow, resamples = data_folds, control = ctrl_pred)

head(fit_folds)

fit_folds %>% 
  collect_predictions() 

fit_folds %>% 
  collect_metrics()

fit_folds %>% collect_predictions() %>% 
  mape(estimate = .pred, truth = real_cost)

##### Pregunta 2: ####
#### 2.a) ####
set.seed(3770)

cost_boots = rsample::bootstraps(transit_cost, times = 1000)

#### 2.b.) ####
mean_cost = c()


for(x in 1:1000){
  val = (cost_boots$splits[[x]] %>% analysis())$real_cost
  mean_cost[x] =  val %>% mean()
}

#### 2.c #####
transit_cost$real_cost %>% 
  mean()
transit_cost$real_cost %>% 
  sd()

theta_boot = mean(mean_cost)
theta_boot

se_boot = sd(mean_cost)
se_boot

#### 2.d #####


mean_cost_df = data.frame(mean_cost)

ggplot(data = mean_cost_df, mapping = aes(x = mean_cost)) +
  geom_histogram(aes(y =..density..), fill = '#006EA1', color = '#006EA5', alpha = 0.5, bins = 30) +
  geom_density(color = 'tomato') +
  geom_vline(xintercept = theta_boot) +
  theme_classic() + 
  labs(x = 'Costo Real',
       y = 'Densidad',
       title = 'Densidad estimada',
       subtitle = 'De las estimaciones bootstrap')

# Calculamos el limite inferior:
inferior_l = mean_cost_df$mean_cost %>%  quantile(.05/2)
inferior_l
# Calculamos el límite superior:
superior_l = mean_cost_df$mean_cost %>%  quantile(1-(.05/2))
superior_l
# Graficamos nuevamente el histograma pero con los límites
ggplot(data = mean_cost_df, mapping = aes(x = mean_cost)) +
  geom_histogram(aes(y =..density..), fill = '#006EA1', color = '#006EA5', alpha = 0.5, bins = 30) +
  geom_density(color = 'tomato') +
  geom_vline(xintercept = inferior_l, color = 'blue') +
  geom_vline(xintercept = superior_l, color = 'blue') +
  theme_classic() + 
  labs(x = 'Costo Real',
       y = 'Densidad',
       title = 'Densidad estimada',
       subtitle = 'De las estimaciones bootstrap')

#### 2.e #####
t.test_val = (transit_cost$real_cost %>% t.test())$conf.int ## al realizar t.test se esta asumiendo distribución normal
t.test_val
# Graficamos nuevamente el histograma con los limites y les añadimos los intervalos del t.test
ggplot(data = mean_cost_df, mapping = aes(x = mean_cost)) +
  geom_histogram(aes(y =..density..), fill = '#006EA1', color = '#006EA5', alpha = 0.5, bins = 30) +
  geom_density(color = 'tomato') +
  geom_vline(xintercept = t.test_val[1], color = 'red') +
  geom_vline(xintercept = t.test_val[2], color = 'red') +
  geom_vline(xintercept = inferior_l, color = 'blue') +
  geom_vline(xintercept = superior_l, color = 'blue') +
  theme_classic() + 
  labs(x = 'Costo Real',
       y = 'Densidad',
       title = 'Densidad estimada',
       subtitle = 'De las estimaciones bootstrap')









