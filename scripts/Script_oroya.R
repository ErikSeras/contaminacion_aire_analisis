# Analizar contaminantes del aire
# Librería necesaria
library(openair)
library(openairmaps)
library(tidyverse)
library(ggmap)


# leer archivo
library(readxl)

SO2_oroya <- read_excel("data/SO2_oroya.xlsx",
                        col_types = c("date", "numeric", "numeric", "numeric", "numeric"))


# Remover la columna de SO2 ppb
SO2_oroya <- select(SO2_oroya, -SO2_1h_ppb)
# Cambiar SO2_1h_ugm3
names(SO2_oroya)[4] <- "so2"

# Agregar una columna de año
SO2_oroya <- mutate(SO2_oroya,
                    year = substr(date, 1, 4))

# Data frame de coordenadas
ubicacion <- data.frame(
  year = c("2019"),
  latitude = c(-11.519952),
  longitude = c(-75.900882)
)

# Agregar coordenadas a las mediciones de aire
SO2_oroya <- SO2_oroya %>%
  left_join(ubicacion, by = "year", all.x = TRUE)

# Eliminar la columna year
SO2_oroya <- select(SO2_oroya, -year)





# Gráfica de resumen de datos de SO2
aa <- summaryPlot(subset(SO2_oroya, select = c("date", "ws", "wd", "so2")),
            na.len = 6, percentile = 0.95,
            col.trend = "gold3", col.data = "green", col.mis = "red", col.hist = "gold2",
            period = "months",
            xlab = c("Gráficas de evolución mensual", "Histogramas"),
            ylab = c("","Porcentaje del total"),
            main = "")


# Gráfica de concentraciones en el tiempo
a <- timeVariation(SO2_oroya, pollutant = "so2",
              hemisphere = "southern", latitude = -11.519952, longitude = -75.900882)



# Rosa de viento por mes
b <- windRose(SO2_oroya, fontsize = 15,
         ws = "ws", wd = "wd", breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3),
         statistic = "prop.count",
         type = "daylight",
         hemisphere = "southern", latitude = -11.519952, longitude = -75.900882,
         angle = 15, cols = "jet", paddle = FALSE,
         grid.line = list(value = 10, lty = 2, col = "dodgerblue"),
         width = 1,
         offset = 30, key.header = "Velocidad del viento", key.footer = "m/s",
         key.position = "right", angle.scale = 315,
         main = "(b)")





# Rosa de contaminantes
c <- pollutionRose(SO2_oroya, pollutant = "so2", type = "year", statistic = "prop.count",
              grid.line = list(value = 5, lty = 2, col = "dodgerblue"),
              hemisphere = "southern", latitude = -11.519952, longitude = -75.900882,
              breaks = c(0,50,100,150,200,250), normalise = FALSE, offset = 30,
              angle = 15, key.header = "SO2",key.footer = "ug m-3") 



# Rosa de percentiles
# Para method = "cpf" poner un solo percentil, percentile = 95
d <- percentileRose(SO2_oroya, pollutant = "so2",
               type = c("season", "daylight"), percentile = c(25, 50, 75, 90, 95),
               hemisphere = "southern",  latitude = -11.519952, longitude = -75.900882,
               smooth = TRUE, cols = "jet",
               mean = TRUE, mean.lty = 1, mean.lwd = 2, mean.col = "black", 
               key.position = "right")

# Para mostral funciones de probabilidad condicional (CPF)
# Posible gráfico en el artículo
d1 <- percentileRose(SO2_oroya, pollutant = "so2",
                     method = "cpf",
                     type = c("season", "daylight"),
                     hemisphere = "southern",  latitude = -11.519952, longitude = -75.900882,
                     smooth = TRUE, cols = "green", percentile = 95,
                     key.position = "right")







# Gráfica de calendario de contaminantes
# Este va al artículo
e <- calendarPlot(SO2_oroya, pollutant = "so2", year = 2019,
             annotate = "value", cols = "increment", statistic = "mean",
             lim = 250, col.lim = c("black","white"),
             font.lim = c(1, 2), digits = 0, cex.lim = c(0.6, 1),
             w.shift = 2,
             main = "",
             key.position = "right", key.header = "SO2", key.footer = "ug m-3",
             layout = c(4, 3))





# Gráfica de calendario de contaminantes dividido por categorías
f <- calendarPlot(SO2_oroya, pollutant = "so2", breaks = c(0, 100, 150, 250, 300),
             annotate = "value", statistic = "mean",
             lim = 250, col.lim = c("black","white"), font.lim = c(1, 2), digits = 0, cex.lim = c(0.6, 1), w.shift = 2,
             labels = c("Very low", "Low", "High", "Very High"),
             cols = c("lightblue", "green", "yellow",  "red"))


# Distribución de máximos de SO2 según la velocidad
# y dirección del viento
g <- polarFreq(SO2_oroya, pollutant = "so2", statistic = "mean",
               type = "year", trans = FALSE,
               hemisphere = "southern",  latitude = -11.519952, longitude = -75.900882,
               ws.int = 0.4, ws.upper = 5, wd.nint = 36, min.bin = 5,
               grid.line = 2, cols = "jet", offset = 20,
               border.col = "white")

# Contribución por direcciones de viento en frecuencia solar
# El SO2 está dominado por los vientos del sureste
g1 <- polarFreq(SO2_oroya, pollutant = "so2", statistic = "weighted.mean",
                hemisphere = "southern",  latitude = -11.519952, longitude = -75.900882,
                type = "daylight", ws.int = 8, offset = 80, trans = FALSE, cols = "heat")






# Gráfica Polar de particiones de SO2
h <- polarCluster(SO2_oroya, pollutant = "so2", x= "ws", wd = "wd", n.clusters = 6,
             main = "Polar partition graphic of SO2\nEnvironment monitoring station: La Oroya - OEFA (2019)")


# Gráfica Polar de SO2
i <- polarPlot(SO2_oroya, pollutant = "so2", x= "ws", wd = "wd",
          main = "Polar graph of SO2\nEnvironment monitoring station: La Oroya - OEFA (2019)",
          key.header = "Mean", key.footer = "ug m-3")

# Gráfico Polar de SO2 mediante CPF
i2 <- polarPlot(SO2_oroya, pollutant = "so2", type = "year",
                statistic = "cpf", percentile = 90,
                resolution = "fine", uncertainty = TRUE, min.bin = 1,
                units = "m/s")

polarPlot(SO2_oroya, pollutant = "so2", statistic = "cpf",
          percentile = 90)


# Grágico polar con regresión no parámetrica del viento
# NWR se usa cuando no hay datos para usar GAM
polarPlot(SO2_oroya, pollutant = "so2",
          statistic = "nwr")

# Determinar los quantiles de concentraciones de SO2
quantile(SO2_oroya$so2, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Gráfico polar en diferentes rangos de percentiles
# Para ver concentraciones son más posibles en
# ciertas direcciones y velocidades de viento
# Posibles gráficas del libro
pp01 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(0, 10))
pp02 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(10, 20))
pp03 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(20, 30))
pp04 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(30, 40))
pp05 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(40, 50))
pp06 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(50, 60))
pp07 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(60, 70))
pp08 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(70, 80))
pp09 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(80, 90))
pp10 <- polarPlot(SO2_oroya, poll = "so2", stati = "cpf", percentile = c(90, 100))

# Imprimir los 10 deciles
print(pp01, split = c(1, 1, 4, 3))
print(pp02, split = c(2, 1, 4, 3), newpage = FALSE)
print(pp03, split = c(3, 1, 4, 3), newpage = FALSE)
print(pp04, split = c(4, 1, 4, 3), newpage = FALSE)
print(pp05, split = c(1, 2, 4, 3), newpage = FALSE)
print(pp06, split = c(2, 2, 4, 3), newpage = FALSE)
print(pp07, split = c(3, 2, 4, 3), newpage = FALSE)
print(pp08, split = c(4, 2, 4, 3), newpage = FALSE)
print(pp09, split = c(2, 3, 4, 3), newpage = FALSE)
print(pp10, split = c(3, 3, 4, 3), newpage = FALSE)


# Medianas de SO2
j <- polarPlot(SO2_oroya, pollutant = "so2", statistic = "median",
          exclude.missing = FALSE, angle.scale = 135, resolution = "fine",
          units = "m/s", main = "Distribución de las medianas de SO2",
          key.header = "Medianas", key.footer = "SO2 (ugm3, 1h)")

# Gráfico de los datos en el tiempo
k <- timePlot(selectByDate(SO2_oroya, year = 2019, month = 1),
              pollutant = c("so2", "ws"))

# Ver máxima concentración de SO2 en enero de 2019
SO2_oroya[with(selectByDate(SO2_oroya, year = 2019, month = 1), order(-selectByDate(SO2_oroya, year = 2019, month = 1)$so2)),]


# Cluster de zonas polarCluster identificación y extracción de características
i <- polarCluster(SO2_oroya, pollutant = "so2",
                  n.clusters = 6, cols = "Accent",
                  units = "m/s")

# Ver resultados del cluster
head(i[["data"]])

# Nuevo dataframe
ii <- i[["data"]]
# Ver cuantas mediciones están en este cluster
table(ii[, "cluster"])
# Variación temporal por clUster en columnas
timeProp(ii, pollutant = "so2", avg.time = "day",
         proportion = "cluster", col = "Accent", key.position = "top",
         key.columns = 6, date.breaks = 10, ylab = "so2 (ug/m3)")


# Variación temporal por cluster por horas y por otros tiempos
timeVariation(ii, pollutant="so2", group = "cluster",
              col = "Accent", ci = FALSE, lwd = 3)

#
# Gráfico anular polarAnnulus
polarAnnulus(SO2_oroya, pollutant = "so2", period = "hour",
             cols = "jet", width = "fat", exclude.missing = FALSE)

# Graficar por cierto rango de velocidad de viento
polarAnnulus(subset(SO2_oroya, ws > 1 & ws < 3), poll="so2", exclude.missing = FALSE)
# Graficar por cuantiles de la velocidad de viento
quantile(SO2_oroya$ws, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
polarAnnulus(subset(SO2_oroya, ws > quantile(ws, probs = 0.1, na.rm = TRUE)),
             poll="so2", exclude.missing = FALSE)


# Trazar series temporales de los datos
timePlot(SO2_oroya, pollutant = c("ws", "so2"),
         y.relation = "free")

timePlot(SO2_oroya, pollutant = "so2",
         avg.time = "month", normalise = "1/1/2019",
         lwd = 4, lty = 1)

timePlot(SO2_oroya, pollutant = "so2", avg.time = "day",
         statistic = "percentile", percentile = 95)

# Para poner flechas con velocidad y dirección del viento
# en la línea temporal
timePlot(SO2_oroya, pollutant = "so2", avg.time = "month",
         windflow = list(scale = 0.05, lwd = 2, col = "orange"),
         lwd = 3, ylab = "concentration (ug/m3)")


# Gráficos de series de tiempo como barra apilada
timeProp(SO2_oroya, pollutant = "so2", avg.time = "week",
         proportion = "wd",
         key.position = "top", key.columns = 8,
         ylab = "so2 (ug/m3)")









# Y más gráfica
# gráficas posibles
print(i, split = c(1, 1, 2, 1))
print(a, split = c(2, 1, 2, 1), subset = "hour", newpage = FALSE)


print(b, position = c(0, 0, 0.5, 0.5), more = TRUE)
print(j, position = c(0.5, 0.5, 1, 1))





# Ejemplo
# Estadísticas por pares
library(worldmet)
har <- importAURN("har", year = 2013)
# import met data from nearby site (Benson)
met <- importNOAA(code = "036580-99999", year = 2013)
# merge AQ and met but don't use modelled ws and wd
har <- inner_join(
  select(har, -ws, -wd),
  met,
  by = "date"
)

polarPlot(har, poll = c("pm2.5", "pm10"),
          statistic = "robust_slope",
          col = "jet",
          limits = c(0, 1),
          ws_spread = 1.5,
          wd_spread = 10)


























