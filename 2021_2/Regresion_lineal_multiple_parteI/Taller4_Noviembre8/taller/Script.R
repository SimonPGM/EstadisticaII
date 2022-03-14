library(tidyverse)
datos <- MPV::table.b7
datos <- datos %>%
  select(-x4) %>%
  select(y, everything())
#a
var(datos)
cov(datos$y, datos$x1)
cov(datos)

#b
cor(datos)

#c (mostrat latex)

#d
datos <- as.matrix(datos)
X <- cbind("(intercepto)" = 1, datos[, -1])
y <- as.matrix(datos[, 1], ncol = 1)

#e
first <- t(X) %*% X
second <- solve(first)
third <- second %*% t(X) %*% y #betas
fourth <- X %*% third #ygorro
fifth <- y - fourth #residuales
