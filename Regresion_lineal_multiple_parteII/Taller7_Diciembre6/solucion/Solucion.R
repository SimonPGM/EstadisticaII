library(car)
#solucion
#Ejercicio 2
gen_dat <- function(n) { #funcion generadora de datos
  x1 <- runif(n=n, min=0, max=10)
  x2 <- x1 * 2 + rnorm(n=n, sd=0.01) # x2 es el doble de x1 + ruido
  y <- rnorm(n=n, mean= - 3 + 2 * x1 - 4 * x2, sd=2)
  data.frame(y, x1, x2)
}

set.seed(12345)
datos <- gen_dat(n=40)
datos1 <- datos[1:20, ]
datos2 <- datos[21:40, ]

#recordemos que beta0 = -3, beta1 = 2, beta2 = -4 y sigma = 2
#ajustando los modelos
mod1 <- lm(y ~ ., data = datos1)
mod2 <- lm(y ~ ., data = datos2)

#vamos a ver los coeficientes
summary(mod1)
summary(mod2)

#factores de inflacion de varianza (hay multicolinealidad demasiado grave)
car::vif(mod1) #alternativa 1
car::vif(mod2)

source("funciones1.R") #cargando las funciones del script funciones1
myCoefficients(mod1, dataset = datos1) #alternativa2
myCoefficients(mod2, dataset = datos2)

#analisis del espectro
myCollinDiag(lm.model = mod1, center = F)
myCollinDiag(lm.model = mod1, center = T)

#Ejercicio 3
datos.best <- MPV::earthquake
library(leaps) #para poder usar funciones
mod.best <- lm(depth ~ ., data = datos.best)
#para el cp de Mallows
myCp_criterion(mod.best)

#transformando la tabla
library(tidyverse)
allreg <- myAllRegTable(mod.best)
n <- nrow(datos.best)
allreg <- allreg %>%
  mutate(Cp = abs(as.numeric(Cp) - (as.numeric(k)+1)),
  MSE = as.numeric(SSE)/(n - (as.numeric(k)+1)))

#ordenando por Cp de Mallows
allreg %>%
  arrange(as.numeric(Cp))

#ordenando por el MSE
allreg %>%
  arrange(MSE)

#ordenando por el R2 ajustado
allreg %>%
  arrange(desc(as.numeric(adj_R_sq)))

#ordenando por el R2 
allreg %>%
  arrange(desc(as.numeric(R_sq)))
