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
library(coin)

#TRANSFORMACION DATOS #############################################
proporciones<-read.csv("proporcios.csv",header=TRUE)
prueba <- data.frame(sexo = rep(c("hombre", "mujer"), each = 300))
num_preguntas <- 22
ajustar_longitud_respuestas <- function(respuestas) {
  if (length(respuestas) > 300) {
    respuestas <- respuestas[1:300]
  } else if (length(respuestas) < 300) {
    respuestas <- c(respuestas, sample(respuestas, 300 - length(respuestas), replace = TRUE))
  }
  return(respuestas)
}

for (i in 1:num_preguntas) {
  inicio_hombres <- ((i - 1) * 10) + 1
  fin_hombres <- inicio_hombres + 4
  inicio_mujeres <- fin_hombres + 1
  fin_mujeres <- inicio_mujeres + 4
  
  n_respuestas_por_categoria_hombres <- round(300 * proporciones$prop[inicio_hombres:fin_hombres])
  n_respuestas_por_categoria_mujeres <- round(300 * proporciones$prop[inicio_mujeres:fin_mujeres])
  
  respuestas_hombres <- c(rep(1, n_respuestas_por_categoria_hombres[1]),
                          rep(2, n_respuestas_por_categoria_hombres[2]),
                          rep(3, n_respuestas_por_categoria_hombres[3]),
                          rep(4, n_respuestas_por_categoria_hombres[4]),
                          rep(5, n_respuestas_por_categoria_hombres[5]))
  
  respuestas_mujeres <- c(rep(1, n_respuestas_por_categoria_mujeres[1]),
                          rep(2, n_respuestas_por_categoria_mujeres[2]),
                          rep(3, n_respuestas_por_categoria_mujeres[3]),
                          rep(4, n_respuestas_por_categoria_mujeres[4]),
                          rep(5, n_respuestas_por_categoria_mujeres[5]))
  
  respuestas_hombres <- ajustar_longitud_respuestas(respuestas_hombres)
  respuestas_mujeres <- ajustar_longitud_respuestas(respuestas_mujeres)
  respuestas_combinadas <- c(respuestas_hombres, respuestas_mujeres)
  prueba[[paste0("P", i)]] <- respuestas_combinadas
}

head(prueba, 10)
tail(prueba, 10)

datos<-prueba
datos<-data.frame(lapply(datos,factor))

#MODELO DE REGRESION ORDINAL ######################################
datos$sexo<-relevel(datos$sexo,ref="hombre")
modelo_ordinal <- clm(P1 ~ sexo, data = datos,maxit = 100)
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
  modelo <- clm(as.formula(paste(variable_likert, "~ sexo")), data = datos, maxit = 100)
  modelos_ordinales[[variable_likert]] <- modelo
  p_valores[i - 1] <- coef(summary(modelo))["sexomujer", "Pr(>|z|)"]
}
which(p_valores<0.05)

###modelo de ecuaciones estructurales #############################
modelo <- '
  bloque1 = ~ P1 + P2 + P3 + P4
  bloque2 = ~ P5 + P6 + P7 + P8
  bloque3 = ~ P9 + P10 + P11
  bloque4 = ~ P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + P20
  bloque5 = ~ P21 + P22
    
    # Relaciones entre el Sexo y las variables observadas
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

modelo <- '
  bloque1 = ~ P1 + P2 + P3 + P4
    # Relaciones entre el Sexo y las variables observadas
    P1 ~ sexo
    P2 ~ sexo
    P3 ~ sexo
    P4 ~ sexo
'

modelo_sem <- sem(modelo, data = datos,ordered = TRUE)

sem_bueno<-parameterestimates(modelo_sem)[23:44,c(1,7)]
which(sem_bueno$pvalue<0.05) 

#prueba de mann whitney #########################################
datos[,2:23]<-lapply(datos[,2:23],as.numeric)
perform_mann_whitney <- function(data, variable) {
  mw_result <- wilcox_test(get(variable) ~ sexo, data = data)
  return(mw_result)
}
mw_results <- list()

for (i in 2:23) {  
  mw_results[[i-1]] <- perform_mann_whitney(datos, names(datos)[i])
}

names(mw_results) <- paste0("P", 1:22)
mw_results #1 2 3 11 12 18 19 20 22

#chi cuadrado ####################################################
tabla <- table(datos$sexo, datos$P1)
test_chi <- chisq.test(tabla)
print(test_chi)


resultados_chi <- list()
pvalores_chi <- numeric(22)

for(i in 1:22) {
  variable_likert <- paste0("P", i)
  tabla <- table(datos$sexo, datos[[variable_likert]])
  test_chi <- try(suppressWarnings(chisq.test(tabla)), silent = TRUE)
  # Si la prueba fue exitosa, extrae el p-valor; de lo contrario, guarda NA
  if(!inherits(test_chi, "try-error") && !is.null(test_chi$p.value)) {
    pvalores_chi[i] <- test_chi$p.value
  } else {
    pvalores_chi[i] <- NA
  }
}
which(pvalores_chi<0.05)

#modelo bayesiano #################################################
datos_bayes<-data.frame(sexo=as.numeric(as.factor(datos$sexo)))
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

save(modelo_P1, modelo_P2, modelo_P3, modelo_P4, modelo_P5, modelo_P6, modelo_P7, modelo_P8, modelo_P9, modelo_P10, modelo_P11, modelo_P12, modelo_P13, modelo_P14, modelo_P15, modelo_P16, modelo_P17, modelo_P18, modelo_P19, modelo_P20, modelo_P21, modelo_P22, file = "modelos_brms_remuestreo.RData")

load("modelos_brms_remuestreo.RData")

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

