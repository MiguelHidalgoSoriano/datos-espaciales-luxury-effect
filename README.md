# Evaluación del Luxury Effect en el Municipio de Valencia (2023)

Este repositorio contiene el código fuente en R desarrollado para la elaboración de la tarea final de la asignatura Datos espaciales y esapciotemporales del tercer curso del grado BIA (UV). El estudio evalúa la relación entre la dotación del arbolado viario público y los indicadores socioeconómicos locales a nivel de sección censal.

##  Estructura del Código
El script `analisis_luxury_effect.R` está organizado de forma modular siguiendo el índice del documento general:
1. **Carga y Limpieza de Datos:** Procesamiento del censo georreferenciado y la Renta Media por Hogar (INE).
2. **Análisis Descriptivo Espacial:** Generación de cartografía base y visualizaciones de densidad con `ggplot2` y `patchwork`.
3. **Análisis de Procesos Puntuales:** Modelado espacial mediante las funciones G y K de Ripley con simulaciones `envelope` (`spatstat`).
4. **Geoestadística Local:** Cálculo del Índice de Moran y mapeo de clústeres socio-ambientales LISA (`rgeoda` y `tmap`).

## Librerías Necesarias
Para reproducir este análisis es necesario contar con R y tener instalados los siguientes paquetes:
`sf`, `dplyr`, `readxl`, `patchwork`, `ggplot2`, `spatstat`, `spdep`, `rgeoda`, `tmap`, `RColorBrewer`.
