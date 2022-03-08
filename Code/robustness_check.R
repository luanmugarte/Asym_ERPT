# Configurações iniciais ####
source('Code/Old/IRF_VLSTAR.R')
source('Code/Old/rootsSTVAR.R')

# Redefinindo as variáveis do modelo
modelo_endo <- model_data$modelo_endo
modelo_endo_df <- data.frame(modelo_endo)
cambio_switching <- model_data$cambio_switching
cambio_switching_zoo <- zoo(hp_filter(as.matrix(cambio_switching), 14400)[[1]][,])
plot(ts(cambio_switching_zoo, start = c(2000,1), frequency = 12))
gamma = 12
switching_function_2 <- 1/(1+exp(-start_values[[1]][1]*(cambio_switching_zoo-start_values[[1]][2])))

par(mfrow=c(3,1))
plot(ts(switching_function_3, start = c(2000,1), frequency = 12))
plot(ts(switching_function_2, start = c(2000,1), frequency = 12))

library(starvars)
# Estimando modelo
start_values <- startingVLSTAR(
  y = modelo_endo_df,
  exo = NULL,
  p = lag_endog,
  m = 2,
  st = cambio_switching_zoo,
  constant = TRUE,
  n.combi = 25,
  ncores = 2,
  singlecgamma = F
)
start_values_1 <- start_values
start_values_1
start_values

plot(cambio_switching_zoo)
cambio_switching_zoo %>%
  filter(cambio > 0.08)

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
  singlecgamma = T,
  starting = list(matrix((c(6,0)), ncol = 2)),
  n.iter = 500,
  method = 'NLS',
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
plot(fit_VLSTAR_lp$st)

# # a few methods for VLSTAR
print(fit_VLSTAR_lp)
summary(fit_VLSTAR_lp)
plot(fit_VLSTAR_lp)
predict(fit_VLSTAR_lp, st.num = 1, n.ahead = 1)
logLik(fit_VLSTAR_lp, type = 'Univariate')
coef(fit_VLSTAR_lp)

fit_VAR <- VAR(modelo_endo, p=2)
modelo_endo
rootsSTVAR(fit_VLSTAR)
rootsSTVAR(fit_VLSTAR_lp)


irf_v1 <- irf_VLSTAR(fit_VLSTAR, n.ahead = 18)
coefs_VLSTAR <- get_coefs_VLSTAR(fit_VLSTAR)
coefs_VLSTAR

par(mfrow=c(2,1))
plot.ts(irf_v1$Regime_1$cambio[,4])
abline(h = 0)
plot.ts(irf_v1$Regime_2$cambio[,4])
abline(h = 0)
fit_VLSTAR$singlecgamma


par(mfrow=c(4,1))

for (i in 1:nrow(start_values[[1]])){
  print(i)
  switching_function_2 <- 1/(1+exp(-start_values[[1]][i,1]*(cambio_switching_zoo-start_values[[1]][i,2])))
  plot(ts(switching_function_2, start = c(2000,1), frequency = 12))
}
