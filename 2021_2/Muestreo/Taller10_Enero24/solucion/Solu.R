#Ejercicio 1
Ni <- c(500, 1000, 4000) #tamano estratos
ni <- c(50, 80, 160) #tamano de muestras en los estratos
ybari <- c(40, 60, 100) #ybarra de los estratos
S2i <- c(400, 300, 100) #S2 de los estratos

#Vamos a estimar
mi <- Ni/(N <- sum(Ni)) #proporcion de unidades de estratos
ybar <- sum(mi*ybari) #Esta es la estimacion puntual de ybarra

#Calculando la varianza estimada
Vybari <- (1 - ni/Ni)*S2i/ni #varianza estimada de ybari
varybar <- sum(mi^2*Vybari) #esta es la varianza estimada de ybarra

#Construyendo el intervalo de confianza
n <- sum(ni) #tamano total de la muestra
ls <- ybar + qt(0.05/2, n - 3, lower.tail = F)*sqrt(varybar) #limite superior
li <- ybar - qt(0.05/2, n - 3, lower.tail = F)*sqrt(varybar) #limite inferior
c(li, ls) #viendo el IC

#Para tau sale gratis, solo hay que multiplicar todo por N
tau <- N*ybar #estmacion de tau
lstau <- N*ls #limite superior del ic para tau
litau <- N*li #limite inferior del ic para tau
c(lstau, litau)

rm(list = ls())

#Ejercicio 2
n <- 210 #tamano de muestra
Ni <- c(1000, 2000, 5000) #tamano de los estratos
N <- sum(Ni) #tamano de la poblacion 
Si2 <- c(100, 81, 36) #varianzas estimadas para los estratos

#Aijacion de Neyman 
psii <- Ni*sqrt(Si2)/sum(Ni*sqrt(Si2)) #Afijaciones
ni <- round(n*psii) #Distribucion de la muestra en los estratos
ni
sum(ni) #esto debe dar igual a n

#Afijacion proprcional
psii2 <- Ni/sum(Ni) #Afijaciones
ni2 <- round(n*psii2) #Distribucion de la muestra en los estratos
ni2
sum(ni2)
#Arreglemos el problema
psii2
ni2 <- ni2 + c(0, 0, 1) #dejando ni tal que su suma sea igual a n
sum(ni2)

rm(list = ls())

#Ejercicio 3
psii <- c(0.3, 0.2, 0.25) #psi del uno al tres
#hallemos el que falta
psii[4] <- 1 - sum(psii)
psii
Ci <- c(10, 25, 50, 5) #costos unitarios por estrato
n <- 1500/sum(Ci*psii)
n
floor(n) #se saca el piso porque hay una restriccion presupuestal
rm(list = ls())

#Ejercicio 4

#hacienolo con MAE
mi <- c(0.6, 0.4)
ni <- c(38, 62) #tamano de muestra en los estratos
pi <- c(6, 10)/ni #proporciones por estrato
pest <- sum(pi*mi) #estimacion de la proporcion de defectuosos
pest
  
#Haciendolo con MAS
n <- sum(ni)
p <- (6 + 10)/n
p

#vamos a calcular intervalos de confianza
#para MAS
limas <- p - qt(0.05/2, n-1, lower.tail = F)*sqrt(p*(1-p)/(n-1)) #limite inferior 
lsmas <- p + qt(0.05/2, n-1, lower.tail = F)*sqrt(p*(1-p)/(n-1)) #limite superior
c(limas, lsmas)

#para MAE
varpi <- pi*(1-pi)/(ni-1) #varianza estimada de pi
varpmae <- sum(mi^2*varpi) #varianza estimada de pest
limae <- pest - qt(0.05/2, n-2, lower.tail = F)*sqrt(varpmae) #limite inferior
lsmae <- pest + qt(0.05/2, n-2, lower.tail = F)*sqrt(varpmae) #limite superior
c(limae, lsmae)

rm(list = ls())

#Ejercicio 5 (i = 1 son pequenas, i = 2 son grandes)
Ni <- c(26, 20) #tamano de las poblaciones
sigmai <- c(100, 200-10)/6 
delta <- 100 #limite para el error de estimacion
#Calculemos las afijaciones
psii <- Ni*sigmai/sum(Ni*sigmai)
N <- sum(Ni)
D <- (delta/(N*qnorm(0.05/2, lower.tail = F)))^2
num <- sum((sigmai*Ni)^2/psii) #numerador tamano de muestra
den <- N^2*D + sum(Ni*sigmai^2) #denominador del tamano de muestra
n <- num/den #tamano de muestra
n <- ceiling(n) #redondeando por encima
n #tamano de muestra
ni <- round(n*psii)
ni
sum(ni)
rm(list = ls())
