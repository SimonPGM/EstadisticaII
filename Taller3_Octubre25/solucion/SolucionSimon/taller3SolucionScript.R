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
mod <- lm(y ~ x, data = datos)

#Ejercicio 3
summary(mod) #Ambos parametro son significativos

#Ejercicio4 
residuales <- residuals(mod) #tambien con mod$residuals
mean(residuales)

#Ejercicio5

#plot(mod, which = 1) alternativa 1
#alternativa 2
ajustados <- fitted(mod)
dfresid <- data.frame(ajust = ajustados, resids = residuales)

residvsfit <- ggplot(dfresid, aes(ajust, resids)) +
  geom_point() +
  labs(x = "Valores ajustados", y = "Residuales",
       title = "Ajustados vs residuales") +
  geom_smooth(se = F) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))
#Ni a palo hay varianza constante, tambien hay señales de no linealidad

#Ejercicio 6
nbreaks <- nclass.Sturges(dfresid$resids)

residhist <- ggplot(dfresid, aes(resids)) +
  geom_histogram(bins = nbreaks, fill = "cyan", color = "darkblue") +
  theme_minimal()+
  labs(x = "Residuales", y = "Frecuencia",
       title = "Histograma para los residuales") +
  theme(plot.title = element_text(hjust = 0.5))

qq <- ggplot(dfresid, aes(sample = resids)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Cuantiles teóricos", y = "Cuantiles muestrales",
       title = "Gráfico cuantil - cuantil") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

valorp <- shapiro.test(dfresid$resids)$p.value

#Ejercicio 7
dfresid <- dfresid %>%
  mutate(tiempo = 1:nrow(dfresid))

residvstime <- ggplot(dfresid, aes(tiempo, resids)) +
  geom_point() +
  labs(x = "Orden de recolección", y = "Residuales",
       title = "Residuales vs tiempo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))

#No se ve gravemente afectado el supuesto de independencia

#Ejercicio 8
library(rsm)
datos.ajuste <- MPV::table.b3
tabla.repetidos <- table(datos.ajuste$x4) #ni
m <- length(tabla.repetidos)
mod.ajuste <- rsm(y ~ FO(x4), data = datos.ajuste)
resumen.ajuste <- summary(mod.ajuste)
anova.ajuste <- resumen.ajuste$lof
