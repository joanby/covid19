# Cambios de la rama `update-2026`

> Esta rama es el mismo curso —Aprende a analizar los datos del COVID-19 con R y Python—, con el código
> adaptado a R y los paquetes de hoy (R 4.5.3, tidyverse 2.0.0, readr 2.2.0, forecast 9.0.2, reticulate 1.47.0,
> rmarkdown 2.32; octubre de 2026). La rama principal sigue exactamente como en el vídeo.

> **Cómo se ha comprobado.** Cada `.R` se ha ejecutado línea a línea (como en RStudio) y cada `.Rmd` se ha
> generado, dos veces: con R y paquetes de finales de 2019 y con los de hoy. Lo que solo fallaba hoy se ha
> arreglado: **no queda nada que funcionara entonces y falle hoy.**
> **No comprobado (necesita internet, no accesible desde el entorno de prueba):** las descargas del Banco
> Mundial con `wbstats` (estudios χ², SIR y serie temporal; ojo: `wbstats` 1.0 cambió `wb()` por `wb_data()`),
> el scraping de los informes MoMo del ISCIII (`scraping_MOMO.Rmd`, con `tabulizer` y Java) y los mapas de
> Natural Earth. La aplicación Shiny (`COVID_Report/app.R`) no se ha lanzado: es un servidor.

## Qué ha cambiado y por qué

- **readr 2:** `write_csv(datos, path = …)` → `write_csv(datos, file = …)` (`Fuentes_de_datos.Rmd`).
- **reticulate:** `use_python("/Users/juangabriel/anaconda3/bin/python")` apunta a una ruta de mi ordenador y
  hoy reticulate **para el script** si no existe. Ahora lleva `required = FALSE`: si en tu ordenador no
  existe, reticulate usa el Python que encuentre. Si quieres uno concreto, cambia la ruta por la tuya.
- **Cachés de knitr subidas al repositorio** (`scraping_MOMO_cache`): se han borrado; RStudio las vuelve a
  crear.
