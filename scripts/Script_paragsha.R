# Analizar contaminantes del aire
# Librería necesaria
library(openair)
library(tidyverse)
library(readxl)
# Para hacer animaciones
library(gganimate)

# Desactivar la notación científica
options(scipen = 999)

# leer archivo
PM10_paragsha <- read_excel("PM10_paragsha.xlsx", 
                            col_types = c("date", "numeric", "numeric", "numeric"))

str(PM10_paragsha)


# Rosa de viento
rwindRose(PM10_paragsha, ws = "ws", wd = "wd", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2, 2.4), angle = 20, cols = "jet", paddle = FALSE,
         offset = 5, key.header = "Wind speed", key.footer = "m/s",
         key.position = "right",
         main = "Wind Rose\nEnvironment monitoring station: Paragsha - OEFA (2019)")


# Rosa de contaminantes
pollutionRose(PM10_paragsha, pollutant = "PM10_1h_ugm3", type = "month", statistic = "prop.mean", breaks = c(0,10,20,30,40,50,60,70,80,90,100),
              angle = 20, key.header = "PM10",key.footer = "ug m-3", layaout = c(4,3)) 




# Gráfica polar de contaminantes
polarPlot(PM10_paragsha, pollutant = "PM10_1h_ugm3", x = "ws", wd = "wd",
          exclude.missing = FALSE, angle.scale = 135, ws.int = 0.2,
          resolution = "fine", units = "m/s", main = "Distribución de las medianas de PM10",
          key.header = "Medianas", key.footer = "Nivel de PM10")

# Gráfica anular de contaminantes
polarAnnulus(PM10_paragsha, pollutant = "PM10_1h_ugm3" , period = "hour", type = "season",
             width = "fat", exclude.missing = FALSE, cols = "increment", layout = c(4,1),
             key.header = "Concentración de PM10", key.footer = "", key.position = "bottom",
             main = "Evolución horaria de las concentraciones de PM10 por estaciones")


# Gráfico de tiempo de concentraciones
timePlot(selectByDate(PM10_paragsha, year = 2019, month = c(1,2,3,4,5,6,7,8,9,10,11,12)),
         pollutant = "PM10_1h_ugm3", avg.time = "day",
         statistic = "mean", cols = "darkorange",
         smooth = TRUE, ci = TRUE, date.breaks = 6,
         ylab = "Concentración de PM10 (ug/m3)",
         main = "Evolución medias diarias PM10 - Otoño de 2019\nEstación de vigilancia ambiental: Paragsha - OEFA"
         )


# Gráfica de calendario de contaminantes
calendarPlot(PM10_paragsha, pollutant = "PM10_1h_ugm3", year = 2019,
             annotate = "value", cols = c("white", "yellow", "orange", "red", "black"),
             limits = c(0,60), lim = 50, col.lim = c("black","white"),
             font.lim = c(1,2), digits = 0, cex.lim = c(0.9,1.2),
             main = "Evolución de PM10 en 2019\nEstación de vigilancia ambiental: Paragsha - OEFA")


# Gráfico de percentil
percentileRose(PM10_paragsha, pollutant = "PM10_1h_ugm3", smooth = TRUE)

# Gráfica de densidad de Kernel para superaciones medias diarias
kernelExceed(PM10_paragsha, x = "wd", y = "ws", pollutant = "PM10_1h_ugm3",
             by = "day", limit = 50, data.thresh = 75, ylab = "Velocidad del viento",
             xlab = "Dirección del viento",
             main = "Gráfica de densidad para la superación del valor límite diario de PM10")

# Regresión lineal (TheilSen)
# Recomendación, se debe tener data de varios años
# Tendencia sin desestacionalizar
TheilSen(PM10_paragsha, pollutant = "PM10_1h_ugm3")

# Tendencia una vez desestacionalizado
TheilSen(PM10_paragsha, pollutant = "PM10_1h_ugm3", deseason = TRUE)







