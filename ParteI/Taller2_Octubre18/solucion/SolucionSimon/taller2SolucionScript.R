library(tidyverse)
#Ejercicio 1
datos <- read.csv("Ecommerce_Customers.csv")

datosnum <- datos %>%
  select(Email, Address, Avatar, everything()) %>%
  select(-(Email:Avatar))

#Ejercicio 2
p1 <- GGally::ggpairs(datosnum)

#Ejercicio 3
modeldf <- datosnum %>%
  select(Yearly.Amount.Spent, Length.of.Membership)
n <- dim(modeldf)[1]
set.seed(314159)
index <- sample(1:n, n*0.8)
datamodel <- modeldf[index, ]

mod <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = datamodel)

p2 <- ggplot(datamodel, aes(x = Length.of.Membership, y = Yearly.Amount.Spent))+
  geom_point()+
  geom_smooth(method = "lm", se = F, color = "red", formula = "y~x")+
  labs(x = "Duración de la membresía", 
       y = "Cantidad anual gastada por cliente",
       title = "Cantidad anual gastada por cliente vs\nDuración de la membresía")+
  theme_minimal()

#Ejercicio 4
df4 <- data.frame(Wald = c("27.95", "<2e-16"),
                  ANOVA = c("781.2", "< 2.2e-16"))
row.names(df4) <- c("Estadístico de prueba",
                    "P-valor")

#Ejercicio 6

df6 <- data.frame(R = c("0.6625", "0.66247", "0.66247"))
row.names(df6) <- c("Salida de R", "Correlación", "Suma de cuadrados")

#Ejercicio 7
datapredict <- datosnum[-index, ]
confidence <- predict(mod, newdata = datapredict,
                       interval = "confidence")
prediction <- predict(mod, newdata = datapredict,
                      interval = "prediction")

joint <- cbind(confidence[1:5, ], prediction[,-1][1:5, ])
