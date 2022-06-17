#el link donde estan los datos
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(url, sep = "\t", header = T)

#Ejercicio 2
#a) ajustando el modelo de regresion
names(datos)
mod <- lm(Estatura ~ ., data = datos[, -2])

#b) esta en las anotaciones

#c) verifiquemos los supuestos del modelo y determinemos si hay
#puntos atipicos, de balanceo o de influencia
plot(mod)

#puntos atipicos
res.stud <- round(rstandard(mod), 4) #calculando los residuales

#puntos de influencia
Cooks.D <- round(cooks.distance(mod), 4) #calculando las distancias de cook
Dffits <- round(dffits(mod), 4) #calculando los dffits

#puntos de balanceo
hii <- round(hatvalues(mod), 4)

diagnosticos <- data.frame(res.stud, Cooks.D, Dffits, hii)

diagnosticos
#Haciendo el analisis

#puntos atipicos
with(diagnosticos, 
     plot(res.stud, ylim = c(-3.5, 3.5), pch = 20,
          xlab = "Observación", ylab = "Residuales estudentizados",
          main = "Residuales estudentizados vs índice de las observaciones"))
abline(h = 3, col = "red")
abline(h = -3, col = "red")

#no hay puntos atipicos, pues ningun residual está por fuera del intervalo [-3, 3]

#puntos de influencia
#distancia de Cook
with(diagnosticos, 
     plot(Cooks.D, pch = 20, ylim = c(0, 1), 
          xlab = "Observación", ylab = "Distancias de Cook",
          main = "Distancias de Cook vs índice de las observaciones"))
abline(h = 1, col = "red")

#segun el criterio de las distancias de cook no hay puntos influenciales

#dffits
threshold <- 2*sqrt(4/26)
with(diagnosticos, 
     plot(Dffits, pch = 20, ylim = c(-1, 1), 
          xlab = "Observación", ylab = "Dffits",
          main = "Dffits vs índice de las observaciones"))
abline(h = threshold, col = "red")
abline(h = -threshold, col = "red")

#segun el criterio de los dffits la tercera observacion es influencial

#puntos de balanceo

threshold.balanceo <- 2*(4/26) #2*(p/n) < 1
with(diagnosticos, 
     plot(hii, pch = 20, 
          xlab = "Observación", ylab = "hii",
          main = "hii vs índice de las observaciones"))
abline(h = threshold.balanceo, col = "red")

#encontramos que tres observaciones resultaron ser puntos de balanceo
