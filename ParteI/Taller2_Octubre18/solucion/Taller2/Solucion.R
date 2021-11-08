library(tidyverse) #cargando el tidyverse
#Leyendo la base de datos

#Ejercicio 1
datos <- read.csv("Ecommerce_Customers.csv") 
str(datos) #esto nos muestra la estructura de la base de datos
datos.modelo <- datos %>%
  select(Avg..Session.Length:Length.of.Membership, Yearly.Amount.Spent)

#Ejercicio 2
plot(datos.modelo) #haciendo matriz de dispersion

datos.modelo.final <- datos.modelo %>%
  select(Length.of.Membership, Yearly.Amount.Spent)

#Ejercicio 3

#seleccionando el 80% de los datos
n <- nrow(datos.modelo.final) #extrayendo el numero de filas
set.seed(314159) #fijando la muestra a seleccionar
index <- sample(1:n, 0.8*n) #seleccionando la muestra

datos.ajuste <- datos.modelo.final[index,] #seleccionando filas del 80%
datos.prediccion <- datos.modelo.final[-index, ] #seleccionando filas del 20% restante

#ajustando el modelo
# Yearly.Amount.Spent_i = beta0 + beta1*Length.of.Membership_i + epsilon_i
# epsilon ~ N(0, sigma^2)

mod <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = datos.ajuste)

ggplot(datos.ajuste, aes(Length.of.Membership, Yearly.Amount.Spent)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x", se = F)

#Ejercicio 4

resumen <- summary(mod) #mirando prueba significancia de la pendiente
anova(mod) #mirando prueba significancia de la regresion

#Ejercicio 5

coef(mod) #coefficients(mod)
#beta 0 no tiene interpretacion util
#beta 1 por cada año que aumenta la suscripcion se estima un aumento de 
#63.58399 dolares en el gasto anual promedio

#Ejercicio 6

resumen$r.squared #extrayendo el R²
#sacar los valores estimados de y
y.estimado <- fitted(mod)
y.real <- datos.ajuste$Yearly.Amount.Spent
R.2 <- cor(y.estimado, y.real)^2 #R² con correlaciones
#Recordar que se puede sacar de la tabla ANOVA

#El R² nos inidca que la recta logra explicar el 66.25% de la variabilidad
#asociada a los datos

#Ejercicio 7

predict(mod, newdata = datos.prediccion, interval = "confidence")[1:5, ] #intervalos de confianza para la respuesta media
predict(mod, newdata = datos.prediccion, interval = "prediction")[1:5, ] #intervalos de prediccion

#Los intervalos de prediccion son SIEMPRE mas anchos que los intervalos de confianza para
#la respuesta media