Trabajo de fin de máster de bioestadística:
================
Comparación de métodos paramétricos y no paramétricos para variables Likert.

## Modelos y Análisis en la Carpeta "codigo r"

Los siete primeros archivos de la carpeta `codigo r` contienen las siguientes modelizaciones:

1. **Modelo frecuentista de regresión ordinal**: Análisis de datos ordinales utilizando métodos frecuentistas.
2. **Modelo bayesiano de regresión ordinal**: Análisis de datos ordinales utilizando métodos bayesianos.
3. **Modelo de ecuaciones estructurales**: Modelado de relaciones entre variables observadas y latentes.
4. **Prueba de Mann Whitney U**: Prueba no paramétrica para comparar dos muestras independientes.
5. **Prueba de Chi cuadrado**: Prueba estadística para determinar la independencia entre variables categóricas.

El último archivo contiene la modelización con efectos aleatorios utilizando un modelo simple y un modelo de regresión ordinal.

## Codigo R:
1. Los cinco modelos aplicados a los datos originales de la encuesta de magisterio TPACK de la Cátedra de Brecha de Género
2. Los cinco modelos aplicados a un remuestreo a partir de las proporciones de los datos originales de la encuesta de magisterio TPACK de la Cátedra de Brecha de Género, simulando 300 datos para hombres y 300 datos para mujeres
3. Los cinco modelos aplicados a un remuestreo a partir de las proporciones de los datos originales de la encuesta de magisterio TPACK de la Cátedra de Brecha de Género, simulando 50 datos para hombres y 100 datos para mujeres
4. Los cinco modelos aplicados a un remuestreo de los datos originales, aplicando una menor diferencia entre hombres y mujeres del 20% en preguntas seleccionadas
5. Los cinco modelos aplicados a un remuestreo a partir de las proporciones de los datos originales de la encuesta de magisterio TPACK de la Cátedra de Brecha de Género, simulando 200 datos para hombres y 200 datos para mujeres, aplicando una menor diferencia entre hombres y mujeres del 20%
6. Los cinco modelos aplicados a la encuesta "Percepción de la ciencia y tecnología" (ECIS3406) realizada por el Centro de Investigaciones Sociológicas (CIS) realizado en mayo de 2023
7. Los cinco modelos aplicados a la encuesta "Young People Survey" realizado en 2013 por la facultad de ciencias económicas y sociales de la Universidad de Bratislava
8. Un modelo de efectos aleatorios simple y de regresión ordinal aplicado a los datos originales de la encuesta de magisterio TPACK de la Cátedra de Brecha de Género

## Datos
1. El archivo "3406_num" contiene los datos de la encuesta del CIS
2. El archivo "datos_brecha" contiene los datos originales de la encuesta de magisterio
3. El archivo "proporcios" contiene las proporciones de las respuestas de hombres y mujeres de la encuesta de magisterio
4. El archivo "responses" contiene los datos de la encuesta realizada en Bratislava

## Versión de R Utilizada
version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

## Paquetes utilizados y su función en el TFM
- **tidyr**: Limpieza y estructura de datos.
- **dplyr**: Manipulación y transformación de datos.
- **ggplot2**: Creación de gráficos y visualizaciones.
- **nortest**: Pruebas de normalidad en los datos.
- **car**: Análisis de regresión y análisis lineales avanzados.
- **ordinal**: Modelos de regresión ordinal.
- **lavaan**: Modelado de ecuaciones estructurales.
- **brms**: Modelos bayesianos con interfaz de Stan.
- **MASS**: Funciones y datasets para análisis estadístico.
- **lme4**: Modelos lineales y no lineales mixtos.
