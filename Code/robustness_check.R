# Configurações iniciais ####
source('Code/Old/IRF_VLSTAR.R')
source('Code/Old/rootsSTVAR.R')

# Redefinindo as variáveis do modelo
modelo_endo_df <- data.frame(modelo_endo)
cambio_switching <- modelo_endo$cambio
cambio_switching_zoo <- zoo(hp_filter(as.matrix(cambio_switching), 14400)[[1]][,])
plot(ts(cambio_switching_zoo, start = c(2000,1), frequency = 12))
gamma = 30
switching_function_3 <- exp(-gamma*cambio_switching_zoo)/(1+exp(-gamma*cambio_switching_zoo))

par(mfrow=c(3,1))
plot(ts(switching_function_1, start = c(2000,1), frequency = 12))
plot(ts(switching_function_2, start = c(2000,1), frequency = 12))
plot(ts(switching_function_3, start = c(2000,1), frequency = 12))


# Estimando modelo
start_values <- startingVLSTAR(
  y = modelo_endo_df,
  exo = NULL,
  p = lag_endog,
  m = 2,
  st = cambio_switching_zoo,
  constant = TRUE,
  n.combi = 10,
  ncores = 1,
  singlecgamma = F
)
start_values

fit_VLSTAR <- VLSTAR(
  y = modelo_endo,
  exo = NULL,
  p = lag_endog,
  m = 2,
  st = cambio_switching_zoo,
  singlecgamma = F,
  starting = start_values,
  n.iter = 500,
  method = 'NLS',
  epsilon = 10^(-3)
)

fit_VLSTAR$Gammac
list(matrix(rep(c(6,0),each = 4), ncol = 2))
start_values

fit_VLSTAR_lp <- VLSTAR(
  y = modelo_endo,
  exo = NULL,
  p = lag_endog,
  m = 2,
  st = cambio_switching_zoo,
  singlecgamma = F,
  starting = list(matrix(rep(c(6,0),each = 3), ncol = 2)),
  n.iter = 500,
  method = 'ML',
  epsilon = 10^(-3)
)

fit_VLSTAR_lp$Gammac

str(fit_VLSTAR)
fit_VLSTAR$obs <- nrow(fit_VLSTAR$fitted)
fit_VLSTAR$totobs <- nrow(modelo_endo)
fit_VLSTAR$y <- as.matrix(modelo_endo)
start_values
str(fit_VLSTAR_lp)
fit_VLSTAR_lp$obs <- nrow(fit_VLSTAR_lp$fitted)
fit_VLSTAR_lp$totobs <- nrow(modelo_endo)
fit_VLSTAR_lp$y <- as.matrix(modelo_endo)
# # a few methods for VLSTAR
# print(fit_VLSTAR)
# summary(fit_VLSTAR)
# plot(fit_VLSTAR)
# predict(fit_VLSTAR, st.num = 1, n.ahead = 1)
# logLik(fit_VLSTAR, type = 'Univariate')
# coef(fit_VLSTAR)

fit_VAR <- VAR(modelo_endo, p=2)

rootsSTVAR(fit_VLSTAR)
rootsSTVAR(fit_VLSTAR_lp)

irf_v1 <- irf_VLSTAR(fit_VLSTAR, n.ahead = 18)
plot.ts(irf_v1$Regime_1$cambio[,2])
plot.ts(irf_v1$Regime_2$cambio[,2])

plot.ts(fit_VLSTAR_lp$st)
plot.ts(cambio_switching_zoo)


# 
K <- ncol(x$Data[[1]])
nstep <- nstep
p <- x$p

m <- x$m

coefs_VLSTAR_r1 <- list(t(x$Bhat[(2:(K+1)),]),
                        t(x$Bhat[((K+2):(K*p+1)),]))

coefs_VLSTAR_r2 <- list(t(x$Bhat[11:14,]),
                        t(x$Bhat[15:18,]))


coefficients_loop <- list(coefs_VLSTAR_r1,coefs_VLSTAR_r2)
Phi_list <- list()
counter <- 0
for (i in coefficients_loop) {
  A <- i
  if (nstep >= p) {
    As <- array(0, dim = c(K, K, nstep + 1))
    for (i in (p + 1):(nstep + 1)) {
      As[, , i] <- matrix(0, nrow = K, ncol = K)
    }
  } else {
    As <- array(0, dim = c(K, K, p))
  }
  for (i in 1:p) {
    As[, , i] <- A[[i]]
  }
  Phi <- array(0, dim = c(K, K, nstep + 1))
  Phi[, , 1] <- diag(K)
  Phi[, , 2] <- Phi[, , 1] %*% As[, , 1]
  if (nstep > 1) {
    for (i in 3:(nstep + 1)) {
      tmp1 <- Phi[, , 1] %*% As[, , i - 1]
      tmp2 <- matrix(0, nrow = K, ncol = K)
      idx <- (i - 2):1
      for (j in 1:(i - 2)) {
        tmp2 <- tmp2 + Phi[, , j + 1] %*% As[, , idx[j]]
      }
      Phi[, , i] <- tmp1 + tmp2

    }
  }
  counter <- counter + 1
  Phi_list[[counter]] <- Phi
}
return(Phi_list)

