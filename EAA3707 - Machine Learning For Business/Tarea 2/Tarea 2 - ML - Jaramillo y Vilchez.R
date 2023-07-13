
## Tarea 2
## Integrantes: Vicente Jaramillo y José Vilchez


##### Pregunta 1 ######

## librerías
library(tidymodels)

## carga de datos
data = read.csv('predict.csv')

## desarrollo pregunta:

# 1. Modificación del nivel de referencia de la variable respuesta
data = data %>% 
  mutate(Purchased = factor(Purchased,levels=c(1,0)))

# 2. Ordenar los dataframes
## Knn
knn_data = data[order(data$pred_knn),]
knn_data
## svm
svm_data = data[order(data$pred_svm),]
svm_data

## 3. Vector thresholds

thresholds_knn = sort(unique(c(0, knn_data$pred_knn, 1)))
thresholds_svm = sort(unique(c(0, knn_data$pred_svm, 1)))

## 4.
## knn 
df_knn = data.frame(matrix(nrow = length(knn_data$pred_knn)))

pos_truth_knn = c()
false_pos_knn = c()
for (i in 1:length(thresholds_knn)) {
  y_hat = c()
  for (j in 1:length(knn_data$pred_knn)) {
    if(knn_data$pred_knn[j] >= thresholds_knn[i]){
      y_hat[j] = 1   
    }else{
      y_hat[j] = 0
    }
  }
  df_knn = cbind(as.factor(y_hat), df_knn)
  pos_truth_knn[i] = sensitivity_vec(knn_data$Purchased,factor(y_hat, levels=c(1,0)))
  false_pos_knn[i] = 1 - specificity_vec(knn_data$Purchased, factor(y_hat, levels=c(1,0)))
}

df_auc_knn = data.frame(false_pos_knn,pos_truth_knn)

## svm
df_svm = data.frame(matrix(nrow = length(svm_data$pred_svm)))

pos_truth_svm = c()
false_pos_svm = c()
for (i in 1:length(thresholds_svm)) {
  y_hat = c()
  for (j in 1:length(svm_data$pred_svm)) {
    if(svm_data$pred_svm[j] >= thresholds_svm[i]){
      y_hat[j] = 1   
    }else{
      y_hat[j] = 0
    }
  }
  df_svm = cbind(as.factor(y_hat), df_svm)
  pos_truth_svm[i] = sensitivity_vec(svm_data$Purchased,factor(y_hat, levels=c(1,0)))
  false_pos_svm[i] = 1 - specificity_vec(svm_data$Purchased, factor(y_hat, levels=c(1,0)))
}

df_auc_svm = data.frame(false_pos_svm, pos_truth_svm)
# 5.
## knn
df_auc_knn = rbind(df_auc_knn, c(0,0))

df_auc_knn %>% 
  ggplot(aes(x = false_pos_knn, y = pos_truth_knn)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

# svm
df_auc_svm %>% 
  ggplot(aes(x = false_pos_svm, y = pos_truth_svm), ) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()


# 6.
knn_data %>% 
  roc_curve(truth = Purchased, pred_knn) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

svm_data %>% 
  roc_curve(truth = Purchased, pred_svm) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

knn_data %>% 
  roc_auc(truth = Purchased, pred_knn)

svm_data %>% 
  roc_auc(truth = Purchased, pred_svm)

##### Pregunta 2 ######

## librerías

library('keras')
library(tidymodels)
library(beepr)

## carga de datos

# install_keras()
mnist = dataset_mnist()
train_images = mnist$train$x
train_labels = mnist$train$y

set.seed(123)

index = sample(1:60000,2000, replace = FALSE)
sample_images = train_images[index,,]
sample_y = train_labels[index]

# Vemos que las primeras 5 imagenes
plot(as.raster(sample_images[1,,],max=255)) #6
plot(as.raster(sample_images[2,,],max=255)) #8
plot(as.raster(sample_images[3,,],max=255)) #7
plot(as.raster(sample_images[4,,],max=255)) #7
plot(as.raster(sample_images[5,,],max=255)) #5

# Creamos el dataframe con todas las imagenes en cada fila y sus píxeles en cada columa
df = as.data.frame(sample_images)
# Añadimos una nueva columna con los valores respuesta y cambiamos el nombre de la col por amor al arte
df = cbind(as.factor(sample_y), df)
colnames(df)[1] = 'var_response'

## desarollo pregunta:

# Realizamos la partición del 90%
set.seed(3707)

data_split <- rsample::initial_split(df, prop = 0.9)

## Los set de datos:
# Entrenamiento
data_train  = rsample::training(data_split)
# Prueba
data_test  = rsample::testing(data_split)

# Se hacen los folds de k = 5
data_folds =  vfold_cv(data_train, v = 5)
data_folds

##### Ajuste del SVM lineal con margen suave #####
# Hacemos el 'feature engineering'
svm_eng = recipe(var_response ~ ., data = data_train)

# Se realiza el SVM lineal
svm_spec =  svm_linear(cost = tune())%>%
  set_mode("classification") %>%
  set_engine("kernlab")

# Hacemos el workflow
wflow = workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_eng)

# Procedemos con el tuning para obtener el valor C

parallel::detectCores()

cl =  parallel::makeCluster(30)

doParallel::registerDoParallel(cl)

svm_res  =  tune::tune_grid(
  svm_spec,
  preprocessor = svm_eng,
  resamples =  data_folds,
  grid = 10,
  control = tune::control_resamples(save_pred = TRUE))

parallel::stopCluster(cl)

collect_metrics(svm_res); beep(1)

# Se selecciona el mejor parametro según la métrica roc_auc
best.C  =  svm_res %>%
  select_best(metric = "roc_auc")
best.C

# Se añade el modelo usando el mejor parametro
svm_mod_final  =  svm_spec %>%
  finalize_model(best.C)

final_svm_wf  =  wflow %>% 
  finalize_workflow(best.C)

doParallel::registerDoParallel()

last_fit_svm =  final_svm_wf %>% 
  last_fit(split = data_split)

last_fit_svm %>% collect_metrics()


svm_predictions = last_fit_svm %>%
  collect_predictions()


####  confusion matrix
svm_predictions %>%
  conf_mat(truth = var_response, estimate = .pred_class)  

# Accuracy:
svm_predictions %>%
  accuracy(var_response, .pred_class)

##### Ajuste del SVM no lineal utilizando el kernel RBF #####
# planteamos el SVM no lineal
svm_nl =  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

# Planteamos el workflow
svm_wflow = 
  workflow() %>% 
  add_model(svm_nl) %>% 
  add_recipe(svm_eng)

# Obtenemos el C y sigma para el kernel
parallel::detectCores()

cl = parallel::makeCluster(32)
doParallel::registerDoParallel(cl)

svm_res = tune::tune_grid(
  svm_nl,
  preprocessor = svm_eng,
  resamples =  data_folds,
  grid = 10,
  control = tune::control_resamples(save_pred = TRUE))

parallel::stopCluster(cl)

collect_metrics(svm_res); beep(1)

# Se selecciona el mejor parametro según la métrica roc_auc
best_rbf  =  svm_res %>%
  select_best(metric = "roc_auc")
best_rbf

# Realizamos el el ajuste del modelo usando los parametros seleccionados segun roc_auc
svm_mod_final  =  svm_nl %>%
  finalize_model(best_rbf)

final_svm_wf  =  svm_wflow %>% 
  finalize_workflow(best_rbf)

doParallel::registerDoParallel()

last_fit_svm =  final_svm_wf %>% 
  last_fit(split = data_split)


svm_predictions = last_fit_svm %>%
  collect_predictions(); beep(1)


####  confusion matrix
svm_predictions%>%
  conf_mat(truth = var_response, estimate = .pred_class)  

# Accuracy:
svm_predictions %>%
  accuracy(var_response, .pred_class)

##### Pregunta 3 ######

## librerías

library(tidyverse)
library(tidymodels)
library(ggcorrplot)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ranger)
library(recipes)
library(baguette)

## carga de datos

df <- read_csv(("stroke.csv"), show_col_types = FALSE)

# a

str(df)
df <- df %>% 
  dplyr::mutate(gender = factor(gender, levels = c("Male", "Female", "Other"))) %>%
  dplyr::mutate(hypertension = factor(hypertension, levels = c("1", "0"),
                                      labels = c("Yes", "No"))) %>%
  dplyr::mutate(heart_disease = factor(heart_disease, levels = c("1", "0"),
                                       labels = c("Yes", "No"))) %>%
  dplyr::mutate(ever_married = factor(ever_married, levels = c("No", "Yes"))) %>%
  dplyr::mutate(work_type = factor(work_type, levels = c("children", "Govt_jov","Never_worked","Private","Self-employed"))) %>%
  dplyr::mutate(Residence_type = factor(Residence_type, levels = c("Rural", "Urban"))) %>%
  dplyr::mutate(smoking_status = factor(smoking_status, levels = c("formerly smoked", "never smoked","smokes","Unknown"))) %>%
  dplyr::mutate(stroke = factor(stroke, levels = c("1", "0")))
glimpse(df)

# b

table(df$smoking_status)
round(table(df$smoking_status)/nrow(df) * 100,2)

# df2 <- df %>% 
#   group_by(stroke, smoking_status) %>% 
#   tally() %>% 
#   complete(smoking_status, fill = list(n = 0)) %>% 
#   mutate(percentage = n / sum(n) * 100)
# 
# ggplot(df2, aes(smoking_status, percentage, fill = stroke)) + 
#   geom_bar(stat = 'identity', position = 'dodge') +
#   theme_bw()

df3 <- df %>% 
  group_by(smoking_status, stroke) %>% 
  tally() %>% 
  complete(stroke, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

library(wesanderson)
names(wes_palettes)

ggplot(df3, aes(stroke, percentage, fill = smoking_status)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = wes_palette("Zissou1", n = 4))+
  theme_bw()

#eliminamos el estatos Unknown
df_final <- subset(df, smoking_status!="Unknown")
table(df_final$smoking_status)
round(table(df_final$smoking_status)/nrow(df_final) * 100,2)

# c

##  1. Splitting Data y Resample Data    ##

set.seed(314)  

data_split     =   initial_split(df, prop = 0.80, strata = stroke)
data_train     = data_split %>% training()
data_test      = data_split %>% testing()
data_folds =  vfold_cv(data_train , v = 10)

# d

##  2. Feature Enginering   ##

df_recipe = recipe(stroke ~ ., data = data_train ) 

df_recipe %>% 
  prep() %>% 
  bake(new_data = data_train)

##  3. Model Specification  ##

tree_model_large  =  decision_tree(cost_complexity =  0,
                                   tree_depth = 20,
                                   min_n = 15) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

##   4. Create a Workflow   ##

tree_workflow_large  =  workflow() %>% 
  add_model(tree_model_large) %>% 
  add_recipe(df_recipe)

##      5. Fit a Model      ##

str(data_train)

tree_wf_fit_large  =  tree_workflow_large %>% 
  fit(data = data_train)

prediccion_large = predict(tree_wf_fit_large, new_data = data_test) 
prediccion_large_train = predict(tree_wf_fit_large, new_data = data_train) 

data_test  =  data_test%>% 
  mutate(pred_large = prediccion_large$.pred_class)
data_train1  =  data_train%>% mutate(pred_large_train = prediccion_large_train$.pred_class)              

# e

##    Decision Tree Plot

tree_fit_large  =  tree_wf_fit_large %>% 
  extract_fit_engine()

par(xpd = TRUE)
plot(tree_fit_large, compress = TRUE)
text(tree_fit_large, use.n = TRUE, cex= .45)

# rpart.plot::prp(tree_fit_large, type = 0, fallen.leaves = TRUE,
#                 tweak = 0.3, roundint = FALSE, cex = 0.14)

conf_mat(
  data = data_train1,
  truth = stroke,
  estimate = pred_large_train
)
acc_test <- accuracy(
  data = data_train1,
  truth = stroke,
  estimate = data_train1$pred_large_train
)
acc_test


# f

#ocupando el data_split de la c), con data folds = 10 

df_recipe = recipe(stroke ~ ., data = data_train )

df_recipe %>%
  prep() %>%
  bake(new_data = data_train)


model_tree_tune<- decision_tree(cost_complexity = tune(),
                                tree_depth = tune())         %>%
  set_engine("rpart")               %>%
  set_mode("classification")


workflow_tree_tune = workflow() %>%
  add_model(model_tree_tune) %>% 
  add_recipe(df_recipe)

min_n()
cost_complexity()

parallel::detectCores()

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)

cv_tree = workflow_tree_tune %>%
  tune_grid(data_folds,
            metrics = metric_set(roc_auc),
            grid=10)

parallel::stopCluster(cl)

cv_tree %>% show_best(metric = 'roc_auc')

## Select best model based on accuracy
best_tree  =  cv_tree %>% 
  select_best(metrics = roc_auc)

best_tree

## Finalize Workflow

final_tree_prune = workflow_tree_tune %>% 
  finalize_workflow(best_tree)

##      5. Fit a Model      ##

tree_prune_fit  =  final_tree_prune %>% 
  fit(data = data_train)


prediccion_prune = predict(tree_prune_fit, new_data = data_test) 
prediccion_prune_train = predict(tree_prune_fit, new_data = data_train) 

data_pred  =  data_test%>% 
  mutate(pred_prune = prediccion_prune$.pred_class)

data_train1  =  data_train1%>% 
  mutate(pred_prune_train = prediccion_prune_train$.pred_class)


##    Decision Tree Plot

tree_prune_fit  =  tree_prune_fit %>% 
  extract_fit_engine()


# par(xpd = TRUE)
# plot(tree_prune_fit, compress = TRUE)
# text(tree_prune_fit, use.n = TRUE)

## Nice plot with2 library(rpart.plot)

rpart.plot(tree_prune_fit) 

conf_mat(
  data = data_train1,
  truth = stroke,
  estimate = pred_large_train
)

conf_mat(
  data = data_pred,
  truth = stroke,
  estimate = pred_prune
)

acc_test <- accuracy(
  data = data_pred,
  truth = stroke,
  estimate = pred_prune
)

acc_train <- accuracy(
  data = data_train1,
  truth = stroke,
  estimate = pred_large_train
)
acc_test
acc_train

# h
library(vip)
tree_fit  =  tree_wf_fit_large %>% 
  pull_workflow_fit()

vip(tree_fit)

# i

# Ocupando la división de los datos en c)

airbnb_recipe  = recipe(stroke ~ ., data = data_train) 

tree_model  =  bag_tree(cost_complexity = tune(),
                        tree_depth = tune(),
                        min_n = tune()) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

## ======================== ##
##   4. Create a Workflow   ##
## ======================== ##

tree_workflow  =  workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(airbnb_recipe)

parallel::detectCores()

cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)

set.seed(124)

cv_fit = tree_workflow %>%
  tune_grid(
    resamples = data_folds, 
    grid = 10, 
    metrics = metric_set(roc_auc))

parallel::stopCluster(cl)

cv_fit %>% show_best(metric = 'roc_auc')


## Select best model based on rmse
best_tree  =  cv_fit %>% 
  select_best(metric = 'roc_auc')

# View the best tree parameters
best_tree


## Finalize Workflow

final_tree_bag =  tree_workflow %>% 
  finalize_workflow(best_tree)

## ======================== ##
##      5. Fit a Model      ##
## ======================== ##

tree_bag_fit  =   final_tree_bag %>% 
  fit(data = data_train)

## Predicción y evaluación del modelo 

prediccion_bag = predict(tree_bag_fit, new_data = data_test) 

data_pred  =  data_test%>% 
  mutate(pred_bag = prediccion_bag$.pred_class)

tree_fit_rf  =  tree_bag_fit %>% 
  extract_fit_parsnip()

vip(tree_fit_rf)

acc_test <- accuracy(
  data = data_pred,
  truth = stroke,
  estimate = data_pred$pred_bag
)
acc_test

#j