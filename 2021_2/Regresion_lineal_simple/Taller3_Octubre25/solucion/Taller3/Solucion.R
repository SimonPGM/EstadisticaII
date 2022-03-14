library(tidyverse)

#Ejercicio 1
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
datos <- gen_dat(75)

#Ejercicio 2
x <- datos$x
y <- datos$y
mod <- lm(y ~ x)

#Ejercicio 3
summary(mod)

#Ejercicio 4
residuales <- residuals(mod)
mean(residuales)

#Ejercicio 5
plot(mod, which = 1) #forma rapida

ajustados <- fitted(mod)

dfresid <- data.frame(ajust = ajustados, resids = residuales) 

ggplot(dfresid, aes(ajust, resids)) +
  geom_point() +
  labs(x = "Valores ajustados", y = "Residuales",
       title = "Ajustados vs Residuales") +
  geom_smooth(se = F)

#Ejercicio 6
plot(mod, which = 2) #forma rapida grafico cuantil - cuantil
hist(residuales) #forma rapida histograma

ggplot(dfresid, aes(sample = resids)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Cuantiles teóricos",
       y = "Cuantiles muestrales",
       title = "Gráfico cuantil - cuantil")

n <- nclass.Sturges(dfresid$resids) #numero de intervalos para histograma

ggplot(dfresid, aes(resids)) +
  geom_histogram(bins = n, color = "blue", fill = "cyan")+
  labs(x = "Residuales", y = "Frecuencia",
       title = "Histograma para los residuales")

shapiro.test(dfresid$resids) #prueba de normalidad

#Ejercicio 7

dfresid <- dfresid %>%
  mutate(tiempo = 1:nrow(dfresid)) #añadiendo el tiempo a la base de datos

# dfresid$tiempo <- 1:nrow(dfresid) forma equivalente

ggplot(dfresid, aes(tiempo, resids)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")

#Ejercicio 8
install.packages("MPV")
datos.ajuste <- MPV::table.b3
mod.ajuste <- lm(y ~ x4, data = datos.ajuste)
table(datos.ajuste$x4) #mirando valores repetidos en la covariable
# unique(datos.ajuste$x4) opcion alternativa
ggplot(datos.ajuste, aes(x4, y)) +
  geom_point()+
  geom_smooth(method = "lm", formula = "y~x", se = F)
#haciendo la prueba de falta de ajuste
install.packages("rsm")
library(rsm)
mod.falta.ajuste <- rsm(y ~ FO(x4), data = datos.ajuste) 
summary(mod.falta.ajuste) #se puede ver el ANOVA de la falta de ajuste
pf(0.7539, df1 = 5, df2 = 25, lower.tail = F) #valor p falta de ajuste