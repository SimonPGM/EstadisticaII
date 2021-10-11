library(tidyverse)
#primer ejercicio
datos <- read.csv("./Taller1/android-games.csv")
datos_modelo <- datos %>% 
  filter(category == "GAME ACTION",
         total_ratings <= 4121627) %>%
  select(total_ratings, five_star_rating)

#segundo ejercicio

  #Solucion nativa
x <- datos_modelo$total_ratings
y <- datos_modelo$five_star_rating

plot(x, y, main = "Cantidad total de calificaciones con 5 estrellas\nvs cantidad total de calificaciones",
     xlab = "Cantidad total de calificaciones", ylab = "Cantidad total de calificaciones con 5 estrellas",
     pch = 19)

  #solución ggplot
(p <- ggplot(datos_modelo, aes(x = total_ratings, y = five_star_rating)) +
  geom_point()+
  labs(x = "Cantidad total de calificaciones [millones]",
       y = "Cantidad total de calificaciones con 5 estrellas [millones]",
       title = "Cantidad total de calificaciones con 5 estrellas\nvs cantidad total de calificaciones") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(1e6, 4e6, by = 1e6), labels = 1:4) +
  scale_y_continuous(breaks = seq(1e6, 2.5e6, by = 0.5e6), labels = seq(1, 2.5, by = 0.5)))

#se puede ver una relación lineal entre ambas variables, donde estas están correlacionadas
#de manera positiva, es decir, a mayor número de calificaciones se tiene un crecimiento
#en las calificaciones con 5 estrellas

#tercer ejercicio

  #parametros manuales
n <- length(x)
xbarra <- mean(x)
Sxx <- sum((x - xbarra)^2)
Sxy <- sum((x - xbarra)*y)
beta1.man <- Sxy/Sxx
ybarra <- mean(y)
beta0.man <- ybarra - beta1.man*xbarra
ygorro.man <- beta0.man + beta1.man*x
residuos.man <- y - ygorro.man 
sigma2.man <- sum(residuos.man^2)/(n-2)


  #parametros con funciones
mod <- lm(five_star_rating ~ total_ratings, data = datos_modelo)
beta0 <- coef(mod)[1]
beta1 <- coef(mod)[2]
resumen <- summary(mod)
sigma2 <- resumen$sigma^2

plot(x, y, main = "Cantidad total de calificaciones con 5 estrellas\nvs cantidad total de calificaciones",
     xlab = "Cantidad total de calificaciones", ylab = "Cantidad total de calificaciones con 5 estrellas",
     pch = 19)
abline(mod, col = "red", lty = "dashed")


(p2 <- p +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  geom_segment(aes(xend = total_ratings, yend = mod$fitted.values),
               color = "red", linetype = "dashed"))

#cuarto y quinto

# beta0 no tiene interpretación y no es significativo tiene unidades de calificaciones de
# 5 estrellas

# beta1 es la tasa de cambio de la cantidad de calificaciones de 5 estrellas por la
# cantidad de calificaciones totales, además es importante traer a colación que
# el parámetro es inferior a 1, atributo que se espera (¿por qué?)
# Finalmente, el parámetro beta1 es significativamente distinto de cero (¿esto qué implica?)

#sexto
  #
alpha <- 0.05
beta0.upper <- beta0.man + qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man*sum(x^2)/(n*Sxx))
beta0.lower <- beta0.man - qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man*sum(x^2)/(n*Sxx))
c(beta0.lower, beta0.upper)

beta1.upper <- beta1.man + qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man/Sxx)
beta1.lower <- beta1.man - qt(alpha/2, n-2, lower.tail = F)*sqrt(sigma2.man/Sxx)
c(beta1.lower, beta1.upper)
  #funcion confint
confint(mod)
confint(mod, "(Intercept)", level = 0.95)
confint(mod, "total_ratings", level = 0.95) 
