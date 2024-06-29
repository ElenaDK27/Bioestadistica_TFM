Trabajo de fin de máster de bioestadística:
================
Comparación de métodos paramétricos y no paramétricos para variables Likert.

# Archivos
Los siete primeros archivos de la carpeta "codigo r" contienen las siguientes modelizaciones:
1. Modelo frecuentista de regresión ordinal
2. Modelo bayesiano de regresión ordinal
3. Modelo de ecuaciones estructurales
4. Prueba de Mann Whitney U
5. Prueba de Chi cuadrado

El último archivo contiene la modelización con efectos aleatorios con un modelo simple y otro de regresión ordinal

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

## Paquetes utilizados y su función en el TFM
1. tidyr: Para limpiar y dar forma a los datos de manera estructurada.
2. dplyr: Para manipular y transformar datos de manera eficiente.
3. ggplot2: Para crear gráficos y visualizaciones de datos de alta calidad.
4. nortest: Para realizar pruebas de normalidad en los datos.
5. car: Para realizar análisis de regresión y otros análisis lineales avanzados.
6. ordinal: Para modelos de regresión ordinal.
7. lavaan: Para modelado de ecuaciones estructurales.
8. brms: Para modelos bayesianos utilizando la interfaz de Stan.
9. MASS: Para funciones y datasets en análisis estadístico aplicados.
10. lme4: Para ajustar modelos lineales y no lineales mixtos.
