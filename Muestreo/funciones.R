# MAS
est_mu_tau <- function(n, N, ybar, S2, alpha = 0.05) {
  var.ybar <- (1-n/N)*S2/n; var.tau <- N^2*var.ybar
  bmu <- qt(alpha/2, n-1, lower.tail = F)*sqrt(var.ybar)
  limu <- ybar - bmu; lsmu <- ybar + bmu
  tau <- N*ybar
  btau <- N*bmu
  litau <- N*limu; lstau <- N*lsmu
  df <- data.frame(estimacion = c(ybar, tau), lee = c(bmu, btau),
                   li = c(limu, litau), ls = c(lsmu, lstau),
                   vars = c(var.ybar, var.tau))
  rownames(df) <- c("mu", "tau")
  return(df)
}

est_p_A <- function(n, N, p, alpha = 0.05) {
  var.p <- (1-n/N)*p*(1-p)/(n-1); var.A <- N^2*var.p
  bp <- qt(alpha/2, n-1, lower.tail = F)*sqrt(var.p)
  lip <- p - bp; lsp <- p + bp;
  A <- N*p
  bA <- N*bp
  liA <- N*lip; lsA <- N*lsp
  df <- data.frame(estimacion = c(p, A), lee = c(bp, bA),
                   li = c(lip, liA), ls = c(lsp, lsA),
                   vars = c(var.p, var.A))
  rownames(df) <- c("p", "A")
  return(df)
}

samplesize <- function(N, sigma2, D, total = F) {
  nhat <- N*sigma2/((N-1)*D + sigma2)
  df <- data.frame(ns = c(nhat, ceiling(nhat)))
  rownames(df) <- c("estimado", "real")
  df
}

#MAE
est_mu_taumae <- function(ni, Ni, ybari, S2i, alpha = 0.05) {
  L <- length(Ni)
  N <- sum(Ni)
  n <- sum(ni)
  varybari <- (1 - ni/Ni)*S2i/ni
  mi <- Ni/N
  ybar <- sum(mi*ybari)
  varybar <- sum(mi^2*varybari)
  liybar <- ybar - qt(alpha/2, n-L, lower.tail = F)*sqrt(varybar)
  lsybar <- ybar + qt(alpha/2, n-L, lower.tail = F)*sqrt(varybar)
  tau <- N*ybar
  litau <- N*liybar
  lstau <- N*lsybar
  df <- data.frame(Estimacion = c(ybar, tau), vars = c(varybar, N^2 * varybar), li = c(liybar, litau), ls = c(lsybar, lstau))
  rownames(df) <- c("mu", "tau")
  return(list(datos = df, muestra = n))
}

est_p_Amae <- function(ni, Ni, p_i, alpha = 0.05) {
  L <- length(Ni)
  N <- sum(Ni)
  n <- sum(ni)
  S2i <- p_i*(1-p_i)
  varp_i <- (1 - ni/Ni)*S2i/(ni-1)
  mi <- Ni/N
  p <- sum(mi*p_i)
  varp <- sum(mi^2*varp_i)
  lip <- p - qt(alpha/2, n-L, lower.tail = F)*sqrt(varp)
  lsp <- p + qt(alpha/2, n-L, lower.tail = F)*sqrt(varp)
  A <- N*p
  liA <- N*lip
  lsA <- N*lsp
  df <- data.frame(Estimacion = c(p, A), vars = c(varp, N^2 * varp), li = c(lip, liA), ls = c(lsp, lsA))
  rownames(df) <- c("p", "A")
  return(list(datos = df, muestra = n))
}
