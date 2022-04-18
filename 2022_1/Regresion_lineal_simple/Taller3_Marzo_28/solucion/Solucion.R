#Ejercicio 1
#funcion que simula los datos
gen_dat <- function(n, seed = 7) {
  varianza <- 16
  set.seed(seed)
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x + 2 * x^2
  set.seed(seed^2)
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}
datos <- gen_dat(75) #generando los datos
#Ejercicio 2
mod <- lm(y ~ x, data = datos) #ajustando el modelo

#Ejercicio 3
summary(mod)
#ambos coeficientes son significativos

#Ejercicio 4
residuales <- residuals(mod) #extrayendo los residuales
mean(residuales) #la media de los residuales da 0

#Ejercicio 5
ajustados <- fitted(mod) #valores ajustados
plot(ajustados, residuales) #hay no linealidad
plot(mod, which = 1) #forma rapida

#Ejercicio 6
#usando un histograma
hist(residuales) #no tiene cuando distribuirse normal (via histograma)
#qqplot
qqnorm(residuales)
qqline(residuales)
plot(mod, which = 2) #forma rapida del qqplot
#esto nos da otro indicio de que no hay normalidad
#prueba de hipotesis
shapiro.test(residuales) #prueba de bondad de ajuste de normalidad

#conclusion, los residuales no son normales

#Ejercicio 7
plot(residuales) #los residuales vs el tiempo no presentan patrones, son independientes

#Ejercicio 8
install.packages(c("MPV", "rsm")) #instalando los paquetes necesarios
datos.ajuste <- MPV::table.b3
#graficando los datos
with(datos.ajuste, plot(x4, y)) #plot(datos.ajuste$x4, datos.ajuste$y)

#cargando el paquete rsm
library(rsm)
mod.falta.ajuste <- rsm(y ~ FO(x4), data = datos.ajuste)
summary(mod.falta.ajuste) #ver notas

#Ejercicio 9
datos.transform <- readxl::read_xlsx("decaimiento.xlsx")
with(datos.transform, plot(t, sustancia))
#ajustando el modelo
mod.transform <- lm(log(sustancia) ~ t, data = datos.transform)
#viendo el summary
summary(mod.transform)
