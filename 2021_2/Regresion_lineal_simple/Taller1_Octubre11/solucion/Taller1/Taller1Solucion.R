#como instalar librerias
install.packages("tidyverse") #instalando el tidyverse
library(tidyverse) #cargando el tidyverse
datos <- read.csv("android-games.csv") #leyendo la base de datos

names(datos) #viendo los nombres de las variables

#primer ejercicio
#primero vamos a filtar y luego a seleccionar de la base de datos
datos.modelo <- datos %>%
  filter(total_ratings <  4121627, category == "GAME ACTION") %>%
  select(total_ratings, five_star_rating)

#segundo ejercicio
  #forma manual
x <- datos.modelo$total_ratings
y <- datos.modelo$five_star_rating
plot(x, y, xlab = "Numero total de calificaciones",
     ylab = "Número total de calificaciones con 5 estrellas",
     main = "Gráfico de y vs x",
     pch = 20)
  #usando ggplot
(p <- ggplot(datos.modelo, aes(total_ratings, five_star_rating)) +
  geom_point() +
  labs(x = "Numero total de calificaciones",
       y = "Número total de calificaciones con 5 estrellas",
       title = "Gráfico de y vs x") +
  theme_light())

#tercer punto 

  #forma manual
xbarra <- mean(x)
Sxx <- sum((x - xbarra)^2)
ybarra <- mean(y)
Sxy <- sum((x - xbarra)*(y - ybarra))
beta1.man <- Sxy/Sxx #estimacion del beta1
beta0.man <- ybarra - xbarra*beta1.man
n <- length(x)
yestimado.man <- beta0.man + beta1.man*x
sigma2.man <- sum((y - yestimado.man)^2)/(n-2)

  #usando lm
mod <- lm(five_star_rating ~ total_ratings, data = datos.modelo)
(resumen <- summary(mod))
beta0 <- coefficients(mod)[1]
beta1 <- coefficients(mod)[2]
sigma2 <- resumen$sigma^2

#adicionando la recta de regresion a los graficos
plot(x, y, xlab = "Numero total de calificaciones",
     ylab = "Número total de calificaciones con 5 estrellas",
     main = "Gráfico de y vs x",
     pch = 20)
abline(mod, col = "red")

p +
  geom_smooth(method = "lm", formula = "y ~ x", se = F)

#cuarto y quinto
beta0  #mirar solo el valor de los coeficientes no dice nada
beta1 #mirar solo el valor de los coeficientes no dice nada
resumen

#sexto

  #manual
alpha <- 0.05
beta0.upper <- beta0.man + qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man*sum(x^2)/(n*Sxx))
beta0.lower <- beta0.man - qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man*sum(x^2)/(n*Sxx))
c(beta0.lower, beta0.upper)

beta1.upper <- beta1.man + qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man/Sxx)
beta1.lower <- beta1.man - qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man/Sxx)
c(beta1.lower, beta1.upper)


#usando confint
confint(mod) #intervalo de confianza para cada uno de los parametros
confint(mod, "(Intercept)", level = 0.95) #calculando intervalo de confianza para beta0
confint(mod, "total_ratings", level = 0.95) #calculando intervalo de confianza para beta1
