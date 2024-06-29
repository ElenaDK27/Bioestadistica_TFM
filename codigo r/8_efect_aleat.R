library(lme4)
datos<-read.csv("datos_brecha.csv",header=TRUE)
datos[,-28]<-lapply(datos[,-28],as.factor)
datos[,6:27]<-lapply(datos[,6:27],as.ordered)

datos<-datos[,c(1,6:27)]

library(tidyverse)
datos <- datos %>%
  mutate(ID = row_number())

datos_largos <- datos %>%
  pivot_longer(
    cols = starts_with("P"), 
    names_to = "Pregunta",
    values_to = "Respuesta"
  )

datos_largos[,1:3]<-lapply(datos_largos[,1:3],as.factor)
datos_largos[,4]<-lapply(datos_largos[,4],as.numeric)

modelo <- lmer(Respuesta ~ Sexo + (1 | ID), data = datos_largos, REML = FALSE)
summary(modelo)
AIC(modelo)

nuevos_datos <- data.frame(Sexo = "Mujer")

prediccion <- predict(modelo, newdata = nuevos_datos, re.form = NA)  # re.form = NA para omitir los efectos aleatorios

print(prediccion)

library(ordinal)
datos_largos$Respuesta<-as.factor(datos_largos$Respuesta)
modelo_clmm <- clmm(Respuesta ~ Sexo + (1 | ID), data = datos_largos, link = "logit")

summary(modelo_clmm)
AIC(modelo_clmm)

modelo_ordinal<-clm(Respuesta ~ Sexo, data = datos_largos, link = "logit")
summary(modelo_ordinal)
AIC(modelo_ordinal)
