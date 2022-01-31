#Bloque 1
#Pregunta 1
n <- 50; N <- 750
ybar <- 10.31; S2 <- 2.25
# manual
varybar <- (1 - n/N)*S2/n #varianza estimada de ybarra
li <- ybar - qt(0.05/2, df = n-1, lower.tail = F)*sqrt(varybar)
ls <- ybar + qt(0.05/2, df = n-1, lower.tail = F)*sqrt(varybar)
c(li, ls)

#automatico
est_mu_tau(n, N, ybar, S2)

#Pregunta 2
tau <- N*ybar
tau 

#Pregunta 3
litau <- N*li #limite inferior de tau
lstau <- N*ls #limite superior de tau
print(c(litau, lstau), digits = 10) #observando el IC
rm(list = ls()) #borrando todo

#Bloque 2
#guardar los tamanos de estratos y de muestras en los estratos
Ni <- c(86, 72, 52, 30) #tamano del estrato
ni <- c(14, 12, 9, 5) #tamano de la muestra en el estrato
N <- sum(Ni) #tamano de la poblacion
n <- sum(ni) #tamano de la muestra
#Pregunta 4
#definiendo los datos de los estratos
estrato1 <- c(97, 67, 42, 125, 25, 92, 105, 86, 27, 43, 45, 59, 53, 21)
estrato2 <- c(125, 155, 67, 96, 256, 47, 310, 326, 220, 352, 142, 190)
estrato3 <- c(142, 256, 310, 440, 495, 510, 320, 396, 196)
estrato4 <- c(167, 220, 780, 655, 540)
#se trabaja sobre los estratos 1 y 2 como si fuera MAS
#para el estrato 1
ybar1 <- mean(estrato1) #obteniendo la media muestral del estrato 1
S21 <- var(estrato1) #varianza muestral del estrato 1
varybar1 <- (1 - ni[1]/Ni[1])*S21/ni[1]
liest1 <- ybar1 - qt(0.05/2, df = ni[1]-1, lower.tail = F)*sqrt(varybar1)
lsest1 <- ybar1 + qt(0.05/2, df = ni[1]-1, lower.tail = F)*sqrt(varybar1)
c(liest1, lsest1)
#o
est_mu_tau(ni[1], Ni[1], ybar1, S21)

#para el estrato 2
ybar2 <- mean(estrato2); S22 <- var(estrato2)
est_mu_tau(ni[2], Ni[2], ybar2, S22)

#Ejercicio 5
#ya tenemos info de los estratos 1 y 2, falta estimar en los estratos 3 y 4
ybar3 <- mean(estrato3); S23 <- var(estrato3) #media y varianza muestrales en el estrato 3
ybar4 <- mean(estrato4); S24 <- var(estrato4) #media y varianza muestrales en el estrato 4

#estimando
ybari <- c(ybar1, ybar2, ybar3, ybar4) #se meten todos los ybarras en un vector
S2i <- c(S21, S22, S23, S24) #se hace lo mismo con los S2
mi <- Ni/N #calculando la proporcion de la poblacion que representa el estrato
ybar <- sum(mi*ybari)
ybar
#calculando el ic
varybari <- mi^2*(1 - ni/Ni)*S2i/ni #calculando la varianza de ybarra_i
varybar <- sum(varybari)
liest <- ybar - qt(0.05/2, df = n-4, lower.tail = F)*sqrt(varybar)
lsest <- ybar + qt(0.05/2, df = n-4, lower.tail = F)*sqrt(varybar)
c(liest, lsest)

#o
est_mu_taumae(ni, Ni, ybari, S2i) 

#Pregunta 6
#Ojo, como la confianza no es la misma, no se puede multiplicar los de mu por N y ya
tauest <- N*ybar #estimacion de tau
setauest <- N*sqrt(varybar) #error estandar de tau
litauest <- tauest - qt(0.1/2, n-4, lower.tail = F)*setauest
lstauest <- tauest + qt(0.1/2, n-4, lower.tail = F)*setauest
c(litauest, lstauest)
rm(list = ls())

#Bloque 3
#Ejercicio 7
#Tamanos de estratos y de muestra en los estratos
Ni <- c(110, 70, 45, 25); N <- sum(Ni)
ni <- c(40, 28, 19, 11); n <- sum(ni)

#Proporciones estimadas en los estratos
p_i <- c(0.15, 0.5, 0.52, 0.63)

#Estimando p
mi <- Ni/N
pest <- sum(mi*p_i) #estimando p
pest

#Calculando la varianza estimada de p
varp_i <- (1 - ni/Ni)*p_i*(1-p_i)/(ni-1)
varpest <- sum(mi^2*varp_i)

#Calculando el IC
lipest <- pest - qt(0.05/2, df = n-4, lower.tail = F)*sqrt(varpest)
lspest <- pest + qt(0.05/2, df = n-4, lower.tail = F)*sqrt(varpest)
c(lipest, lspest)

#o
est_p_Amae(ni, Ni, p_i)

#Ejercicio 8
liAest <- N*lipest #limite inferior para A (estratificado)
lsAest <- N*lspest #limite superior para A (estratificado)
c(liAest, lsAest) #a nivel global no es = 43

#para el estrato1
est_p_A(n, N, p_i[1])
#(23.47312371, 51.5268763) como 43 esta contenido no se rechaza HO

#Ejercicio 9
si <- sqrt(p_i * (1-p_i)/(ni-1)) #desviaciones para afijaciones
#usando afijacion de Neyman
psiNeyman <- Ni*si/sum(Ni*si) #afijaciones de Neyman
D <- (0.1/qnorm(0.05/2, lower.tail = F))^2
nNeyman <- sum((Ni*si)^2/psiNeyman)/(N^2*D + sum(Ni*si^2))
ceiling(nNeyman)
#usando afijacion proporcional
psiprop <- Ni/sum(Ni)
nprop <- sum((Ni*si)^2/psiprop)/(N^2*D + sum(Ni*si^2))
ceiling(nprop)
rm(list = ls())

#borrar
p_i <- c(0.08, 0.075, 0.03125)
varpi <- c(12, 17, 10)^2
Ni <- c(500, 1000, 4000)
N <- sum(Ni)
psii <- Ni/N
D <- (3/qnorm(0.05/2, lower.tail = F))^2
N*sum(Ni*varpi)/(N^2*D + sum(Ni*varpi))
sum(Ni^2*varpi/psii)/(N^2*D + sum(Ni*varpi))
