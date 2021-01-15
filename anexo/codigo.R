
## INTRODUCCION A LA CIENCIA DE DATOS
## TRABAJO TEORICO-PRACTICO
## CURSO 2020/2021
## ANTONIO MANJAVACAS LUCAS

# En este fichero se incluye todo el codigo empleado en el trabajo

# Solamente incluye codigo y alguna breve explicacion
# Para mayor informacion, vease los ficheros trabajo.Rmd y trabajo.pdf

# Librerias utilizadas:

library(knitr)
library(kableExtra)

library(readr)
library(tidyverse)
library(ggplot2)
library(moments)
library(dlookr)
library(car)
library(corrplot)
library(GGally)
library(lubridate)
library(caret)
library(gridExtra)
library(kknn)
library(reshape2)

######### ANEXO I. EDA ########## 

## Wankara ##

# Carga de datos
wankara <- read.csv('../data/wankara/wankara.dat', comment.char = '@', header = F)

colnames(wankara) <- c(
  'Max_temperature',
  'Min_temperature',
  'Dewpoint',
  'Precipitation',
  'Sea_level_pressure',
  'Standard_pressure',
  'Visibility',
  'Wind_speed',
  'Max_wind_speed',
  'Mean_temperature'
)

kbl(wankara[1:10,], booktabs = T, caption='Primeras filas del dataset wankara') %>%
  kable_styling(latex_options = c('scale_down', 'striped'), font_size=8)

# Analisis del dataset
dim(wankara)
str(wankara)
colSums(is.na(wankara))
wankara[duplicated(wankara),]

# Informacion estadistica
summary(wankara)
apply(wankara, 2, quantile)
apply(wankara, 2, IQR)

# Boxplots
wankara %>% gather(variable, value) %>%
  ggplot(aes(factor(variable), value)) + geom_boxplot(fill = 'steelblue') +
  facet_wrap( ~ variable, scale = 'free') + theme_minimal() +
  labs(
    # title = 'Distribución de los datos',
    # subtitle = 'Boxplots',
    x = 'Variables',
    y = 'N'
  )

# Histogramas
n_bins = 10

wankara %>% gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = n_bins, fill = 'steelblue', color = 'black') +
  facet_wrap(~ variable, scale = 'free') + theme_minimal() +
  labs(
    # title = 'Distribución de los datos',
    # subtitle = 'Histogramas',
    x = 'Valor',
    y = 'N'
  ) + geom_freqpoly(bins = n_bins, col='red')

# Skewness
skewness <- wankara %>% skewness()

kbl(as.data.frame(skewness), booktabs = T, caption='Asimetría de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Kurtosis
kurtosis <- wankara %>% kurtosis()

kbl(as.data.frame(kurtosis), booktabs = T, caption='Curtosis de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Normalidad
normality <- wankara %>% normality()

kbl(as.data.frame(normality), digits=30, booktabs = T, caption='Normalidad de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

wankara %>% normality() %>% filter(p_value > .05)

shapiro.test(wankara$Standard_pressure)

qqPlot(
  wankara$Standard_pressure,
  xlab = 'Cuartiles norm.',
  ylab = 'Standard pressure',
  # main = 'QQplot: Standard Pressure'
  id = FALSE
)

# Escalado de las variables
scaled_wankara <- as.data.frame(scale(wankara))

scaled_wankara %>% gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = n_bins, fill = 'skyblue', color = 'black') +
  facet_wrap(~ variable, scale = 'free') + theme_minimal() +
  labs(
    # title = 'Distribución de los datos estandarizados',
    # subtitle = 'Histogramas',
    x = 'Valor',
    y = 'N'
  ) + geom_freqpoly(bins = n_bins, col='red') 

# Correlacion
wankara %>% cor() %>% corrplot(
  method = 'color',
  addCoef.col = 'black',
  tl.col = 'black',
  number.cex = .7
)

wankara %>% 
  ggpairs(progress = FALSE, title='Correlaciones entre variables') + theme_bw()

# Visibilidad vs temperatura
wankara %>% ggplot(aes(x=Visibility, y=Mean_temperature)) + 
  geom_point(col='brown',alpha=.6) + theme_minimal() + 
  labs(x='Nivel de visibilidad', y='Temperatura media (ºF)')

# Precipitaciones vs temperatura
anom <- wankara %>% filter(Precipitation > 2.0)

kbl(anom, booktabs = T, caption='Días con precipitaciones anómalas') %>%
  kable_styling(latex_options = c('scale_down', 'striped'), font_size=10)

wankara %>% ggplot(aes(x=Precipitation, y=Mean_temperature)) + 
  geom_point(col='navy',alpha=.6) + theme_minimal() + 
  labs(x='Precipitaciones (in)', y='Temperatura media (ºF)')

indexes <- which(wankara$Precipitation > 2.0)

wankara_precip <- wankara[-indexes,]

wankara %>% ggplot(aes(x=Precipitation, y=Mean_temperature)) + 
  geom_point(col='navy',alpha=.6, size=5) + theme_minimal() + 
  labs(x='Precipitaciones (in)', y='Temperatura media (ºF)') +
  scale_x_continuous(breaks = round(seq(0, max(wankara$Precipitation), by = 0.5), 1)) +
  xlim(3.0, 4.0)

wankara_precip %>% ggplot(aes(x=Precipitation, y=Mean_temperature)) + 
  geom_point(col='navy',alpha=.6) + theme_minimal() + 
  labs(x='Precipitaciones (in)', y='Temperatura media (ºF)') +
  scale_x_continuous(breaks = round(seq(0, max(wankara$Precipitation), by = 0.5), 1))

# Presion vs temperatura
wankara %>% ggplot(aes(x=Standard_pressure, y=Mean_temperature)) + 
  geom_point(col='darkcyan',alpha=.6) + theme_minimal() + 
  labs(x='Presión estándar (inHg)', y='Temperatura media (ºF)')

wankara %>% ggplot(aes(x=Sea_level_pressure, y=Mean_temperature)) + 
  geom_point(col='darkcyan',alpha=.6) + theme_minimal() + 
  labs(x='Presión a nivel del mar (inHg)', y='Temperatura media (ºF)')

# Viento vs temperatura
wankara %>% ggplot(aes(x=Wind_speed, y=Mean_temperature)) + 
  geom_point(col='indianred',alpha=.6) + theme_minimal() + 
  labs(x='Velocidad del viento (MPH)', y='Temperatura media (ºF)')

## Newthyroid ##

# Carga de datos
newthyroid <- read.csv('../data/newthyroid/newthyroid.dat', comment.char='@', header = FALSE)

colnames(newthyroid) <- c('T3resin', 'Thyroxin', 'Triiodothyronine', 
                          'Thyroidstimulating', 'TSH_value', 'Class')  

kbl(newthyroid[1:10,], booktabs = T, caption='Primeras filas del dataset newthyroid') %>%
  kable_styling(latex_options = c('scale_down', 'striped'), font_size=11)

# Exploracion del dataset
dim(newthyroid)

str(newthyroid)
summary(newthyroid)

colSums(is.na(newthyroid))
newthyroid[duplicated(newthyroid),]

newthyroid$Class <- as.factor(newthyroid$Class)

# Informacion estadistica
summary(newthyroid)

thyroid_num <- newthyroid[,unlist(lapply(newthyroid, is.numeric)) ]

apply(thyroid_num , 2, quantile)
apply(thyroid_num , 2, IQR)

# Estudio de los niveles de TSH
levels(newthyroid$Class) <- c('Normal', 'Hipertiroidico', 'Hipotiroidico')
tsh_by_class <- newthyroid %>% group_by(Class) %>% summarise(`Mean TSH`=mean(TSH_value), `Median TSH`=median(TSH_value))

kbl(tsh_by_class, booktabs = T, caption='Niveles de TSH medios para cada tipo de paciente') %>%
  kable_styling(latex_options = c('striped'), font_size=10)

newthyroid %>% group_by(Class) %>% ggplot() + 
  geom_boxplot(aes(x=Class, y=TSH_value,fill=Class)) + 
  coord_cartesian(ylim=c(-3,55)) + theme_minimal()

# Frecuencia de clases
freqs <- table(newthyroid$Class)
as.data.frame(freqs) %>% rename(Clase=Var1, N=Freq) %>% 
  ggplot(aes(x=Clase, y=N)) + geom_bar(stat='identity', aes(fill=Clase)) +
  xlab('Clase') + ylab('N') + theme_minimal() +
  geom_text(aes(label=N), vjust=1.6, color='white', size=4) + 
  theme(legend.position = 'none')

# Boxplots
newthyroid %>% gather(variable, value, -Class) %>%
  ggplot(aes(factor(variable), value)) + geom_boxplot(aes(fill = Class)) +
  facet_grid(Class ~ variable, scale = 'free') +
  labs(
    # title = 'Distribución de los datos',
    # subtitle = 'Boxplots',
    x = 'Variables',
    y = 'N'
  )

# Desviacion tipica de TSH
sd_tsh <- newthyroid %>% group_by(Class) %>% select(Clase=Class, TSH_value) %>% summarise(`TSH SD`=sd(TSH_value))

kbl(sd_tsh, booktabs = T, caption='Desviación típica de los niveles de TSH por clase') %>%
  kable_styling(latex_options = c('striped'), font_size=10)

# Histrogramas
n_bins = 9

thyroid_num %>% gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = n_bins, fill = 'steelblue', color = 'black') +
  facet_wrap(~ variable, scale = 'free') + theme_minimal() +
  labs(
    # title = 'Distribución de los datos',
    # subtitle = 'Histogramas',
    x = 'Valor',
    y = 'N'
  ) + geom_freqpoly(bins = n_bins, col='red')

# Skewness
skewness <- thyroid_num %>% skewness()

kbl(as.data.frame(skewness), booktabs = T, caption='Asimetría de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Kurtosis
kurtosis <- thyroid_num %>% kurtosis()

kbl(as.data.frame(kurtosis), booktabs = T, caption='Curtosis de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Normalidad
normality <- thyroid_num %>% normality()

kbl(as.data.frame(normality), digits=30, booktabs = T, caption='Normalidad de las variables') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Box-Cox
box_cox_values <- preProcess(thyroid_num, method = c('BoxCox'))
thyroid_num_box_cox<-predict(box_cox_values, thyroid_num)

box_cox_normality <- normality(thyroid_num_box_cox)

kbl(as.data.frame(box_cox_normality), digits=30, booktabs = T, caption='Normalidad de las variables tras aplicar Box-Cox') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Yeo-Johnson
yeo_johnson_values <- preProcess(thyroid_num, method = c('YeoJohnson'))
thyroid_num_yeo_johnson <-predict(yeo_johnson_values, thyroid_num)

yeo_johnson_normality <- normality(thyroid_num_yeo_johnson)

kbl(as.data.frame(yeo_johnson_normality), digits=30, booktabs = T, caption='Normalidad de las variables tras aplicar Yeo-Johnson') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Datos centrados y escalados
preprocess_values <- preProcess(thyroid_num, method = c('center', 'scale'))
thyroid_num_transformed <- predict(preprocess_values, thyroid_num)

thyroid_num_transformed %>% gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = n_bins, fill = 'yellowgreen', color = 'black') +
  facet_wrap(~ variable, scale = 'free') + theme_minimal() +
  labs(
    # title = 'Distribución de los datos',
    # subtitle = 'Histogramas',
    x = 'Valor',
    y = 'N'
  ) + geom_freqpoly(bins = n_bins, col='red')

# Correlaciones
thyroid_num %>% cor() %>% corrplot(
  method = 'color',
  addCoef.col = 'black',
  tl.col = 'black',
  number.cex = .7
)

newthyroid %>% 
  ggpairs(progress = FALSE, 
          title='Correlaciones entre variables', 
          ggplot2::aes(colour=Class), 
          lower = list(combo = wrap("facethist", binwidth = 0.5))) + theme_bw()

######### ANEXO II. REGRESION ########## 

## Regresion lineal ##

# Modelos
model_1 <- lm(Mean_temperature ~ Max_temperature, data=wankara)
summary(model_1)

model_2 <- lm(Mean_temperature ~ Min_temperature, data=wankara)
summary(model_2)

model_3 <- lm(Mean_temperature ~ Dewpoint, data=wankara)
summary(model_3)

model_4 <- lm(Mean_temperature ~ Sea_level_pressure, data=wankara)
summary(model_4)

model_5 <- lm(Mean_temperature ~ Visibility, data=wankara)
summary(model_5)

# Resumen de los modelos

s1 <- summary(model_1)
s2 <- summary(model_2)
s3 <- summary(model_3)
s4 <- summary(model_4)
s5 <- summary(model_5)

summaries <- list(s1,s2,s3,s4,s5)

# Extraemos R-squared y Adj. R-squared para cada modelo

r_squareds <- unlist(lapply(summaries, function(x) x$r.squared))
adj_r_squareds <- unlist(lapply(summaries, function(x) x$adj.r.squared))

results <- data.frame(
  row.names = c('Model 1: Max_temperature', 
                'Model 2: Min_temperature', 
                'Model 3: Dewpoint', 
                'Model 4: Sea_level_pressure', 
                'Model 5: Visibility'),
  'R-squared'=r_squareds, 
  'Adj R-squared'=adj_r_squareds)

kbl(results, booktabs = T, caption='Resultados de cada modelo de regresión') %>%
  kable_styling(latex_options = 'striped', font_size=10)

# Modelos representados
p1 <- wankara %>% ggplot(aes(x=Max_temperature, y=Mean_temperature)) + geom_point(color='orange') +
  geom_abline(color='blue', slope = coef(model_1)[[2]], intercept = coef(model_1)[[1]]) + theme_minimal()

p2 <- wankara %>% ggplot(aes(x=Min_temperature, y=Mean_temperature)) + geom_point(color='orange') +
  geom_abline(color='blue', slope = coef(model_2)[[2]], intercept = coef(model_2)[[1]]) + theme_minimal()

p3 <- wankara %>% ggplot(aes(x=Dewpoint, y=Mean_temperature)) + geom_point(color='orange') +
  geom_abline(color='blue', slope = coef(model_3)[[2]], intercept = coef(model_3)[[1]]) + theme_minimal()

p4 <- wankara %>% ggplot(aes(x=Sea_level_pressure, y=Mean_temperature)) + geom_point(color='orange') +
  geom_abline(color='blue', slope = coef(model_4)[[2]], intercept = coef(model_4)[[1]]) + theme_minimal()

p5 <- wankara %>% ggplot(aes(x=Visibility, y=Mean_temperature)) + geom_point(color='orange') +
  geom_abline(color='blue', slope = coef(model_5)[[2]], intercept = coef(model_5)[[1]]) + theme_minimal()

grid.arrange(p1, p2, p3, p4, p5, ncol=3, nrow=2)


# Stepwise backward
m1 <- lm(Mean_temperature ~ ., data=wankara)
summary(m1)

m2 <- lm(Mean_temperature ~ .-Precipitation, data=wankara)
summary(m2)

m3 <- lm(Mean_temperature~.-Precipitation+I(Visibility^2), data=wankara)
summary(m3)

m4 <- lm(Mean_temperature ~ .-Precipitation-Wind_speed+I(Visibility^2), 
         data=wankara)
summary(m4)

m5 <- lm(Mean_temperature ~ .-
           Precipitation-Wind_speed-Max_wind_speed+I(Visibility^2), 
         data=wankara)
summary(m5)

m6 <- lm(Mean_temperature ~ .-
           Precipitation-Wind_speed-
           Max_wind_speed-Standard_pressure+I(Visibility^2), 
         data=wankara)
summary(m6)

m7 <- lm(Mean_temperature~Max_temperature+Min_temperature, data=wankara)
summary(m7)

m8 <- lm(Mean_temperature~Max_temperature*Min_temperature, data=wankara)
summary(m8)

# Mejor modelo
best_lm <- m6
best_lm

## k-NN ##

dataset = 'wankara'

# k-NN con CV
run_knn_fold <- function(i, dataset, tt = 'test', form = Y ~ .) {
  # cargar i-esimo fold de train
  file <- paste('../data/wankara/', dataset, '-5-', i, 'tra.dat', sep = '')
  x_tra <- read.csv(file, comment.char = '@', header = FALSE)
  
  # cargar i-esimo fold de test
  file <- paste('../data/wankara/', dataset, '-5-', i, 'tst.dat', sep = '')
  x_tst <- read.csv(file, comment.char = '@', header = FALSE)
  
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ('X', 1:In, sep = '')
  names(x_tra)[In + 1] <- 'Y'
  names(x_tst)[1:In] <- paste ('X', 1:In, sep = '')
  names(x_tst)[In + 1] <- 'Y'
  
  if (tt == 'train') {
    test <- x_tra
  } else {
    test <- x_tst
  }
  
  # Regresion
  fitMulti = kknn(form, x_tra, test)
  yprime = fitMulti$fitted.values
  
  # MSE
  sum(abs(test$Y - yprime) ^ 2) / length(yprime)
}

# con todas las variables:
knnMSEtrain_all <- mean(sapply(1:5, run_knn_fold, dataset, 'train'))
knnMSEtest_all <- mean(sapply(1:5, run_knn_fold, dataset, 'test'))

# con las variables del mejor modelo de lm:
knnMSEtrain_best <-
  mean(sapply(1:5, run_knn_fold, dataset, 'train', 
              Y ~ . - X4 - X6 - X8 - X9 + I(X7 ^ 2)))
knnMSEtest_best <-
  mean(sapply(1:5, run_knn_fold, dataset, 'test', 
              Y ~ . - X4 - X6 - X8 - X9 + I(X7 ^ 2)))

# con las variables Max_temperature y Min_temperature:
knnMSEtrain_maxmin <-
  mean(sapply(1:5, run_knn_fold, dataset, 'train', Y ~ X1 + X2))
knnMSEtest_maxmin <-
  mean(sapply(1:5, run_knn_fold, dataset, 'test', Y ~ X1 + X2))


# Regresion lineal con CV
run_lm_fold <- function(i, dataset, tt = 'test', form = Y ~ .) {
  # cargar i-esimo fold de train
  file <- paste('../data/wankara/', dataset, '-5-', i, 'tra.dat', sep = '')
  x_tra <- read.csv(file, comment.char = '@', header = FALSE)
  
  # cargar i-esimo fold de test
  file <- paste('../data/wankara/', dataset, '-5-', i, 'tst.dat', sep = '')
  x_tst <- read.csv(file, comment.char = '@', header = FALSE)
  
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ('X', 1:In, sep = '')
  names(x_tra)[In + 1] <- 'Y'
  names(x_tst)[1:In] <- paste ('X', 1:In, sep = '')
  names(x_tst)[In + 1] <- 'Y'
  
  if (tt == 'train') {
    test <- x_tra
  } else {
    test <- x_tst
  }
  
  # Regresion
  fitMulti = lm(form, x_tra)
  yprime = predict(fitMulti, test)
  
  # MSE
  sum(abs(test$Y - yprime) ^ 2) / length(yprime)
}

# con las variables del mejor modelo de lm:
lmMSEtrain <-
  mean(sapply(1:5, run_lm_fold, dataset, 'train', 
              Y ~ . - X4 - X6 - X8 - X9 + I(X7 ^ 2)))
lmMSEtest <-
  mean(sapply(1:5, run_lm_fold, dataset, 'test', 
              Y ~ . - X4 - X6 - X8 - X9 + I(X7 ^ 2)))

# Resultados
results_mse <-
  data.frame(
    'MSE train' = c(
      knnMSEtrain_all,
      knnMSEtrain_best,
      knnMSEtrain_maxmin,
      lmMSEtrain
    ),
    'MSE test' = c(knnMSEtest_all,
                   knnMSEtest_best,
                   knnMSEtest_maxmin,
                   lmMSEtest),
    row.names = c(
      'k-NN: todas las variables',
      'k-NN: mejores variables lm',
      'k-NN: max y min temp.',
      'lm: mejores variables'
    )
  )

kbl(results_mse, booktabs = T, caption =
      'MSE para k-NN y regresión lineal múltiple sobre train y test (5-fcv)') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

# k-NN sobre train y test utilizando 5-fcv y todas las variables
knnMSEtrain <- mean(sapply(1:5, run_knn_fold, dataset, 'train'))
knnMSEtest <- mean(sapply(1:5, run_knn_fold, dataset, 'test'))

# lm sobre train y test utilizando 5-fcv y todas las variables
lmMSEtrain <- mean(sapply(1:5, run_lm_fold, dataset, 'train'))
lmMSEtest <- mean(sapply(1:5, run_lm_fold, dataset, 'test'))

## Comparativa ##

# Cargamos los datos del .csv para aplicar los tests estadisticos:

## Tabla con los errores medios de test
resultados <- read.csv("../data/results/regr_test_alumnos.csv")
tablatst <- cbind(resultados[, 2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[, 1]

## Tabla con los errores medios de entrenamiento
resultados <- read.csv('../data/results/regr_train_alumnos.csv')
tablatra <- cbind(resultados[, 2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[, 1]

## Se aplica comparativa por pares para LM y k-NN mediante Wilcoxon:
difs <- (tablatst[, 1] - tablatst[, 2]) / tablatst[, 1]
wilc_1_2 <-
  cbind(ifelse (difs < 0, abs(difs) + 0.1, 0 + 0.1),
        ifelse (difs > 0, abs(difs) + 0.1, 0 + 0.1))
colnames(wilc_1_2) <-
  c(colnames(tablatst)[1], colnames(tablatst)[2])
# head(wilc_1_2)

LMvsKNNtst <-
  wilcox.test(wilc_1_2[, 1],
              wilc_1_2[, 2],
              alternative = "two.sided",
              paired = TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <-
  wilcox.test(wilc_1_2[, 2],
              wilc_1_2[, 1],
              alternative = "two.sided",
              paired = TRUE)
Rmenos <- LMvsKNNtst$statistic

lm_vs_knn <- cbind('R+' = Rmas,
                   'R-' = Rmenos,
                   'p-value' = pvalue)
rownames(lm_vs_knn) <- 'LM vs. k-NN'

kbl(lm_vs_knn, booktabs = T, caption =
      'Comparativa entre LM (R+) y k-NN (R-) aplicando el test de Wilcoxon') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

# No existen diferencias significativas entre ambos
# (23.4% de confianza de que sean distintos)

## Podemos aplicar comparativas multiples utilizando Friedman y Holm como post-hoc:

# comprobamos si hay diferencias significativas entre al menos un par de algoritmos
test_friedman <- friedman.test(as.matrix(tablatst))
# test_friedman
# las hay: p-value < 0.05

# comparamos todos con todos
tam <- dim(tablatst)
groups <- rep(1:tam[2], each = tam[1])
pw <- pairwise.wilcox.test(as.matrix(tablatst),
                           groups,
                           p.adjust = "holm",
                           paired = TRUE)

# pw

# Existen diferencias significativas a favor de M5' (3vs1 0.0805 y 3vs2 0.1077, con aprox.
# 90% de confianza, alpha=0.1) mientras que los otros dos pueden ser considerados equivalentes

lm_vs_knn_vs_m5 <- pw$p.value
rownames(lm_vs_knn_vs_m5) <- c('k-NN', 'M5\'')
colnames(lm_vs_knn_vs_m5) <- c('LM', 'k-NN')

kbl(lm_vs_knn_vs_m5, booktabs = T, caption =
      'Comparativa entre LM, k-NN y M5\' aplicando Holm') %>%
  kable_styling(latex_options = 'striped', font_size = 10)


######### ANEXO III. CLASIFICACION ########## 

## k-NN ##

dataset = 'newthyroid'

do_knn_fold <- function(i, dataset, tt = 'test', k_value = 1) {
  # cargar i-esimo fold de train
  file <- paste('../data/newthyroid/', dataset, '-10-', i, 'tra.dat', sep = '')
  fold_train <- read.csv(file, comment.char = '@', header = FALSE)
  
  # cargar i-esimo fold de test
  file <- paste('../data/newthyroid/', dataset, '-10-', i, 'tst.dat', sep = '')
  fold_test <- read.csv(file, comment.char = '@', header = FALSE)
  
  len <- length(names(fold_train))
  
  # train
  X_train = fold_train[, 1:len - 1]
  y_train = fold_train[, len]
  
  # test
  if (tt == 'train') {
    X_test <- X_train
    y_test <- y_train
  } else if (tt == 'test') {
    X_test <- fold_test[, 1:len - 1]
    y_test <- fold_test[, len]
  }
  
  y_train <- as.factor(y_train)
  y_test <- as.factor(y_test)
  
  # entrenamiento
  clf <- train(
    x = X_train,
    y = y_train,
    method = 'knn',
    metric = 'Accuracy',
    preProc = c('center', 'scale'),
    tuneGrid = expand.grid(k=k_value)
  )
  
  # prediccion
  y_pred = predict(clf, newdata = X_test)
  
  # accuracy
  mean(y_test == y_pred)
}

knn_train_acc_k1 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'train', k_value = 1))
knn_test_acc_k1 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'test', k_value = 1))

knn_train_acc_k3 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'train', k_value = 3))
knn_test_acc_k3 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'test', k_value = 3))

knn_train_acc_k5 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'train', k_value = 5))
knn_test_acc_k5 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'test', k_value = 5))

knn_train_acc_k7 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'train', k_value = 7))
knn_test_acc_k7 <-
  mean(sapply(1:10, do_knn_fold, dataset, 'test', k_value = 7))

results_knn <-
  rbind(
    'KNN (K=1)' = c(knn_train_acc_k1, knn_test_acc_k1),
    'KNN (K=3)' = c(knn_train_acc_k3, knn_test_acc_k3),
    'KNN (K=5)' = c(knn_train_acc_k5, knn_test_acc_k5),
    'KNN (K=7)' = c(knn_train_acc_k7, knn_test_acc_k7)
  )

colnames(results_knn) <- c('Acc. CV train', 'Acc. CV test')

kbl(results_knn, booktabs = T, caption =
      'Resultados obtenidos tras aplicar k-NN con 10-fcv') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

# Resultados k-NN
df <- as.data.frame(results_knn)
df['rowname'] <- rownames(df)

df %>% melt(id.vars = 'rowname',
            value.name = 'value',
            variable.name = 'case') %>%
  ggplot(aes(x = rowname, y = value)) +
  geom_bar(aes(fill = case), stat = 'identity', position = 'dodge') + theme_bw() +
  labs(x = '', y = 'Accuracy') +
  geom_text(
    aes(group = case, label = round(value, 3)),
    vjust = 1.5,
    color = 'white',
    size = 4,
    position = position_dodge(width = 1)
  ) + scale_fill_discrete(name = '', labels = c('Train 10-fcv', 'Test 10-fcv'))

## LDA ##

# Covarianza
nums <- sapply(newthyroid, is.numeric)

normal <- newthyroid %>% filter(Class == 'Normal')
hipert <- newthyroid %>% filter(Class == 'Hipertiroidico')
hipoti <- newthyroid %>% filter(Class == 'Hipotiroidico')

vars_normal <- sapply(normal[,nums], var)
vars_hipert <- sapply(hipert[,nums], var)
vars_hipoti <- sapply(hipoti[,nums], var)

covariance <- data.frame(vars_normal, vars_hipert, vars_hipoti)
colnames(covariance) <- c('Normal', 'Hipertorioidico', 'Hipotiroidico')

kbl(covariance, booktabs = T, caption =
      'Varianzas por variable y clase') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

l1 <- leveneTest(T3resin ~ Class, newthyroid)
l2 <- leveneTest(Thyroxin ~ Class, newthyroid)
l3 <- leveneTest(Triiodothyronine ~ Class, newthyroid)
l4 <- leveneTest(Thyroidstimulating ~ Class, newthyroid)
l5 <- leveneTest(TSH_value ~ Class, newthyroid)

levene_results <-
  data.frame(cbind(
    'Variable' = c(
      'T3resin',
      'Thyroxin',
      'Triiodothyronine',
      'Thyroidstimulating',
      'TSH_value'
    ),
    'p-value' = c(
      l1$`Pr(>F)`[1],
      l2$`Pr(>F)`[1],
      l3$`Pr(>F)`[1],
      l4$`Pr(>F)`[1],
      l5$`Pr(>F)`[1]
    )
  ))

# p-value bajo -> diferencias significativas entre varianzas

kbl(levene_results, booktabs = T, caption =
      'Resultados tras aplicar el Test de Levene') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

# varianza de los predictores
varianza <- sapply(newthyroid[,nums], var)

kbl(as.data.frame(varianza), booktabs = T, caption =
      'Varianzas de los predictores') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

# clasificacion con LDA
dataset = 'newthyroid'

cv_classify <- function(i, dataset, tt = 'test', method, variables) {
  # cargar i-esimo fold de train
  file <- paste('../data/newthyroid/', dataset, '-10-', i, 'tra.dat', sep = '')
  fold_train <- read.csv(file, comment.char = '@', header = FALSE)
  
  # cargar i-esimo fold de test
  file <- paste('../data/newthyroid/', dataset, '-10-', i, 'tst.dat', sep = '')
  fold_test <- read.csv(file, comment.char = '@', header = FALSE)
  
  len <- length(names(fold_train))
  
  # train
  X_train = fold_train[, variables]
  y_train = fold_train[, len]
  
  # test
  if (tt == 'train') {
    X_test <- X_train
    y_test <- y_train
  } else if (tt == 'test') {
    X_test <- fold_test[, variables]
    y_test <- fold_test[, len]
  }
  
  y_train <- as.factor(y_train)
  y_test <- as.factor(y_test)
  
  # entrenamiento
  model <- train(X_train, 
                 y_train, 
                 method=method, 
                 preProcess = c('center', 'scale'))
  
  # prediccion
  y_pred = predict(model, newdata = X_test)
  
  # accuracy
  mean(y_test == y_pred)
}

# utilizando todas las variables
lda_train_acc_all <-
  mean(sapply(1:10, cv_classify, dataset, 'train', method='lda', variables=c(1:5)))
lda_test_acc_all <-
  mean(sapply(1:10, cv_classify, dataset, 'test', method='lda', variables=c(1:5)))

# utilizando un subconjunto de variables
# se elimina la variable Thyroidstimulating, muy relacionada con TSH_value
lda_train_acc_selected <- 
  mean(sapply(1:10, cv_classify, dataset, 'train', method='lda', variables=c(1,2,3,5)))
lda_test_acc_selected <-
  mean(sapply(1:10, cv_classify, dataset, 'test', method='lda', variables=c(1,2,3,5)))

results_lda <-
  rbind(
    'LDA: todas las variables' = c(lda_train_acc_all, lda_test_acc_all),
    'LDA: variables seleccionadas' = c(lda_train_acc_selected, lda_test_acc_selected)
  )

colnames(results_lda) <- c('Acc. CV train', 'Acc. CV test')

kbl(results_lda, booktabs = T, caption =
      'Resultados obtenidos tras aplicar LDA con 10-fcv') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

df <- as.data.frame(results_lda)
df['rowname'] <- rownames(df)

df %>% melt(id.vars = 'rowname',
            value.name = 'value',
            variable.name = 'case') %>%
  ggplot(aes(x = rowname, y = value)) +
  geom_bar(aes(fill = case), stat = 'identity', position = 'dodge') + theme_bw() +
  labs(x = '', y = 'Accuracy') +
  geom_text(
    aes(group = case, label = round(value, 3)),
    vjust = 1.5,
    color = 'white',
    size = 4,
    position = position_dodge(width = 1)
  ) + scale_fill_discrete(name = '', labels = c('Train 10-fcv', 'Test 10-fcv'))

## QDA ##

# Clasificacion mediante QDA

# utilizando todas las variables
qda_train_acc_all <-
  mean(sapply(1:10, cv_classify, dataset, 'train', method='qda', variables=c(1:5)))
qda_test_acc_all <-
  mean(sapply(1:10, cv_classify, dataset, 'test', method='qda', variables=c(1:5)))

# utilizando un subconjunto de variables
qda_train_acc_selected <- 
  mean(sapply(1:10, cv_classify, dataset, 'train', method='qda', variables=c(1,2,3,5)))
qda_test_acc_selected <-
  mean(sapply(1:10, cv_classify, dataset, 'test', method='qda', variables=c(1,2,3,5)))

results_qda <-
  rbind(
    'QDA: todas las variables' = c(qda_train_acc_all, qda_test_acc_all),
    'QDA: variables seleccionadas' = c(qda_train_acc_selected, qda_test_acc_selected)
  )

colnames(results_qda) <- c('Acc. CV train', 'Acc. CV test')

kbl(results_qda, booktabs = T, caption =
      'Resultados obtenidos tras aplicar QDA con 10-fcv') %>%
  kable_styling(latex_options = 'striped', font_size = 10)

df <- as.data.frame(results_qda)
df['rowname'] <- rownames(df)

df %>% melt(id.vars = 'rowname',
            value.name = 'value',
            variable.name = 'case') %>%
  ggplot(aes(x = rowname, y = value)) +
  geom_bar(aes(fill = case), stat = 'identity', position = 'dodge') + theme_bw() +
  labs(x = '', y = 'Accuracy') +
  geom_text(
    aes(group = case, label = round(value, 3)),
    vjust = 1.5,
    color = 'white',
    size = 4,
    position = position_dodge(width = 1)
  ) + scale_fill_discrete(name = '', labels = c('Train 10-fcv', 'Test 10-fcv'))

## Comparativa ##

results <- read_csv('../data/results/clasif_test_alumnos.csv')

# Resultados similares con datos de train:
# results <- read_csv('../data/results/clasif_train_alumnos.csv')

results <- results %>% remove_rownames %>% column_to_rownames(var='X1')

head(results)

#### Friedman ####
friedman <- friedman.test(as.matrix(results))
friedman
# con p-value > 0.05 no podemos rechazar H0
# ningun metodo difiere significativamente del resto


# Aunque no es necesario, podemos confirmarlo aplicando Holm como post-hoc...
groups <- rep(1:ncol(results), each=nrow(results))
results_holm <- pairwise.wilcox.test(as.matrix(results), groups, p.adjust = 'holm', paired = TRUE)
results_holm

comparison <- results_holm$p.value
rownames(comparison) <- c('LDA', 'QDA')
colnames(comparison) <- c('KNN', 'LDA')

comparison

