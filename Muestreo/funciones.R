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
