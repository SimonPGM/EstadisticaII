#Ejercicio 2
mtcars #esta es la base de datos
str(mtcars) #aca vemos que cyl es numerica
unique(mtcars$cyl) #mirar los valores unicos de cyl en mtcars
datos <- mtcars #guardando mtcars en una variable
datos$cyl <- factor(datos$cyl)
str(datos) #verificando que cyl sea factor
mod1 <- lm(mpg ~ cyl + wt, data = datos) #modelo sin interaccion
mod2 <- lm(mpg ~ cyl*wt, data = datos) #modelo con interaccion

library(leaps)
source("funciones1.R")
myAllRegTable(lm(area ~ ., data = rock)) #tabla de todas las regresiones
modnaive <- lm(area ~ 1, data = rock) #modelo naive
anova(modnaive) #SSE del modelo naive
