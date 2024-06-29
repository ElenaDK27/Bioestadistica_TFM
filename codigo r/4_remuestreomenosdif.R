#PAQUETES
library(tidyr)
library(dplyr)
library(ggplot2)
library(nortest)
library(car)
library(ordinal)
library(lavaan)
library(brms)
library(MASS)

#TRANSFORMACION DATOS #############################################
datos<-read.csv("datos_brecha.csv",header=TRUE)
datos<-datos[,c(1,6:27)]
datos$Sexo<-as.factor(datos$Sexo)
datos[,2:23]<-lapply(datos[,2:23],as.numeric)

mezclar_respuestas <- function(datos, pregunta, fraccion = 0.2) {
  indices_mujeres <- which(datos$Sexo == "Mujer")
  indices_hombres <- which(datos$Sexo == "Hombre")
  n_intercambios <- floor(min(length(indices_mujeres), length(indices_hombres)) * fraccion)
  muestras_mujeres <- sample(indices_mujeres, n_intercambios)
  muestras_hombres <- sample(indices_hombres, n_intercambios)
  respuestas_temp <- datos[[pregunta]][muestras_mujeres]
  datos[[pregunta]][muestras_mujeres] <- datos[[pregunta]][muestras_hombres]
  datos[[pregunta]][muestras_hombres] <- respuestas_temp
  
  return(datos)
}

# Aplicar la función a las preguntas con diferencias conocidas
preguntas_con_diferencias <- c("P1", "P2", "P3", "P19", "P22")
for (pregunta in preguntas_con_diferencias) {
  datos <- mezclar_respuestas(datos, pregunta)
}

datos<-data.frame(lapply(datos,factor))
prueba<-datos

## Modelo de regresión ordinal ####################################
datos$sexo<-relevel(datos$Sexo,ref="Hombre")
modelo_ordinal <- clm(P1 ~ Sexo, data = datos,maxit = 100)
summary(modelo_ordinal)

enlaces <- c("logit", "probit", "cloglog", "loglog","cauchit")

modelos <- list()

for (enlace in enlaces) {
  modelo <- clm(P1 ~ sexo, data = datos, link = enlace)
  modelos[[enlace]] <- modelo
}
AICs <- sapply(modelos, AIC)
AICs #no hay diferencia casi

modelos_ordinales <- list()
p_valores <- numeric(22)
for (i in 2:23) {
  variable_likert <- names(datos)[i]
  modelo <- clm(as.formula(paste(variable_likert, "~ Sexo")), data = datos, maxit = 100)
  modelos_ordinales[[variable_likert]] <- modelo
  p_valores[i - 1] <- coef(summary(modelo))["SexoMujer", "Pr(>|z|)"]
}

summary(modelos_ordinales[[1]])
which(p_valores<0.05) #1  2  3 11 12 18 19 20 22

## Modelo de ecuaciones estructurales #############################
modelo <- '
    genero =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 + P10 + P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + P20 + P21 + P22
    
    # Relaciones entre el sexo y las variables observadas
    P1 ~ sexo
    P2 ~ sexo
    P3 ~ sexo
    P4 ~ sexo
    P5 ~ sexo
    P6 ~ sexo
    P7 ~ sexo
    P8 ~ sexo
    P9 ~ sexo
    P10 ~ sexo
    P11 ~ sexo
    P12 ~ sexo
    P13 ~ sexo
    P14 ~ sexo
    P15 ~ sexo
    P16 ~ sexo
    P17 ~ sexo
    P18 ~ sexo
    P19 ~ sexo
    P20 ~ sexo
    P21 ~ sexo
    P22 ~ sexo
'

modelo_sem <- sem(modelo, data = datos,ordered = TRUE)

sem_bueno<-parameterestimates(modelo_sem)[23:44,c(1,7)]
which(sem_bueno$pvalue<0.05) 

## Prueba de Mann-Whitney U #######################################
datos[,2:23]<-lapply(datos[,2:23],as.numeric)
perform_mann_whitney <- function(data, variable) {
  mw_result <- wilcox_test(get(variable) ~ Sexo, data = data)
  return(mw_result)
}
mw_results <- list()

for (i in 2:23) {  
  mw_results[[i-1]] <- perform_mann_whitney(datos, names(datos)[i])
}

names(mw_results) <- paste0("P", 1:22)
mw_results 

## Chi cuadrado ###################################################
datos[,2:23]<-lapply(datos[,2:23],as.factor)
tabla <- table(datos$Sexo, datos$P1)
test_chi <- chisq.test(tabla)
print(test_chi)

resultados_chi <- list()
pvalores_chi <- numeric(22)

for(i in 1:22) {
  variable_likert <- paste0("P", i)
  tabla <- table(datos$Sexo, datos[[variable_likert]])
  test_chi <- try(suppressWarnings(chisq.test(tabla)), silent = TRUE)
  # Si la prueba fue exitosa, extrae el p-valor; de lo contrario, guarda NA
  if(!inherits(test_chi, "try-error") && !is.null(test_chi$p.value)) {
    pvalores_chi[i] <- test_chi$p.value
  } else {
    pvalores_chi[i] <- NA
  }
}
which(pvalores_chi<0.05) 

## Modelo bayesiano de regresión ordinal ##########################
datos_bayes<-data.frame(sexo=as.numeric(as.factor(datos$Sexo)))
datos_transformados <- datos
preguntas <- paste0("P", 1:22)
for (pregunta in preguntas) {
  datos_transformados[[paste0(pregunta)]] <- factor(
    datos[[pregunta]],
    levels = 1:5,
    labels = c("C1", "C2", "C3", "C4", "C5")
  )
}
datos_bayes<-cbind(datos_bayes$sexo,datos_transformados[,2:23])
colnames(datos_bayes)[1]<-"sexo"
datos_bayes[,2:23]<-lapply(datos[,2:23],as.ordered)

modelo_P1 <- brm(
  formula = bf(P1 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P2 <- brm(
  formula = bf(P2 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P3 <- brm(
  formula = bf(P3 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P4 <- brm(
  formula = bf(P4 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P5 <- brm(
  formula = bf(P5 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P6 <- brm(
  formula = bf(P6 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P7 <- brm(
  formula = bf(P7 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P8 <- brm(
  formula = bf(P8 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P9 <- brm(
  formula = bf(P9 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P10 <- brm(
  formula = bf(P10 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P11 <- brm(
  formula = bf(P11 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P12 <- brm(
  formula = bf(P12 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P13 <- brm(
  formula = bf(P13 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P14 <- brm(
  formula = bf(P14 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P15 <- brm(
  formula = bf(P15 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P16 <- brm(
  formula = bf(P16 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P17 <- brm(
  formula = bf(P17 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P18 <- brm(
  formula = bf(P18 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P19 <- brm(
  formula = bf(P19 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P20 <- brm(
  formula = bf(P20 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P21 <- brm(
  formula = bf(P21 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

modelo_P22 <- brm(
  formula = bf(P22 ~ sexo, cmc = TRUE),
  family = cumulative(link = "logit"),
  data = datos_bayes,
  chains = 2,
  iter = 1000,
  warmup = 500,
  control = list(adapt_delta = 0.80),
  seed = 123
)

save(modelo_P1, modelo_P2, modelo_P3, modelo_P4, modelo_P5, modelo_P6, modelo_P7, modelo_P8, modelo_P9, modelo_P10, modelo_P11, modelo_P12, modelo_P13, modelo_P14, modelo_P15, modelo_P16, modelo_P17, modelo_P18, modelo_P19, modelo_P20, modelo_P21, modelo_P22, file = "modelos_brms_menosdif.RData")

load("modelos_brms_menosdif.RData") #-1.009, -1.410, -0.638 P1
resultados <- data.frame(
  modelo = character(22),
  estimacion = numeric(22),
  IC_inferior = numeric(22),
  IC_superior = numeric(22),
  stringsAsFactors = FALSE
)

modelos <- list(modelo_P1, modelo_P2, modelo_P3, modelo_P4, modelo_P5, modelo_P6, modelo_P7, modelo_P8, modelo_P9, modelo_P10, modelo_P11, modelo_P12, modelo_P13, modelo_P14, modelo_P15, modelo_P16, modelo_P17, modelo_P18, modelo_P19, modelo_P20, modelo_P21, modelo_P22)

for (i in 1:length(modelos)) {
  resumen <- summary(modelos[[i]])$fixed[5, c(1, 3, 4)] 
  resultados$modelo[i] <- paste0("P", i)
  resultados$estimacion[i] <- as.numeric(round(resumen[1], 3))
  resultados$IC_inferior[i] <- as.numeric(round(resumen[2], 3))
  resultados$IC_superior[i] <- as.numeric(round(resumen[3], 3))
}

resultados$IC_pasa_por_cero <- with(resultados, IC_inferior < 0 & IC_superior > 0)

resultados$IC_pasa_por_cero <- ifelse(resultados$IC_pasa_por_cero, "No significativo", "Significativo")

