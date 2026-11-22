arp_mm <- function(x, p){
  mu <- mean(x)
  sigma2 <- var(x)
  autocor <- acf(x, p, plot = FALSE)
  rho_vec <- autocor$acf[2:(p+1)]
  matrix_rho = diag(p)
  for(n in 1:p){
    for(m in 1:p){
      if(m != n){
        matrix_rho[n,m] = rho_vec[abs(m-n)]
      }
    }
  }
  phi_vec = solve(matrix_rho, rho_vec)
  sigma2_eps = sigma2 * (1 - phi_vec%*%rho_vec)
  df_mus = data.frame(Mu = mu, Sigma2 = sigma2, Sigma2_eps = sigma2_eps)
  df_phis = data.frame(Phi = phi_vec, Rho = rho_vec)
  lista = list(df_mus, df_phis)
  return(lista)
}

set.seed(1234)
x <- 0.05 + arima.sim(n = 1000, list(ar = 0.5), sd = 1)
arp_mm(x, 1)