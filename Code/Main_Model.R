# Configurações iniciais ####

if (!exists("dadosbrutos")) {
  source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Packages.R', echo=F, verbose = F)
  source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Cleaning_data_monthly.R', echo=F, verbose = F)
}

if (!exists("dadosbrutos.trim")) {
  source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Cleaning_data_quarterly.R', echo=F)
}


# Estabelecendo diretório padrão
#   (necessário pois é alterado futuramente para exportar gráficos em diretórios separados)
setwd('/home/luanmugarte/Artigos/Asym_ERPT/')

# Resgatando as variáveis do modelo
dadosbrutos.trim
dadosbrutos
length(dadosbrutos)

# Configurações do modelo ####

# Comm exógeno ou endógeno
comm_endo = F

# Frequência (mensal ou trim)
modelo = 'mensal'

# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 0

# Variável de demanda agregada (capacidade, pib ou pimpf)
DA_variable = 'pimpf'

# Horizonte das LP's
hor_lps <- 18

# Gamma da função de transição
gamma_transition = 3

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 1

# Lags das variáveis exógenas
lag_exog = 2

# Estimação #### 

# Configurações extras ####
# Condicional para a determinação da frequencia das variáveis
if (modelo == 'mensal'){
  dados <- dadosbrutos
  date <- seq(1999.77,2020.23,1/12)
  lambda_hp = 14400
} else   {
  dados <- dadosbrutos.trim
  date <- seq(2000.26,2019.99,1/4)
  lambda_hp = 1600
}

# Variável de comm para o caso de ser exógeno
comm_df <- dados %>%
  dplyr::select(comm) %>%
  mutate(across(everything(), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
  drop_na()
  
# Precisa ser dataframe para a função lp_nl
modelo_exog <- data.frame(comm_df) 
modelo_exog

# Escolha das variáveis do modelo
if (comm_endo == T) {
  modelo_endo <- dados %>%
    dplyr::select(comm,cambio, DA_variable,desemprego,ipcaindice) %>%
    mutate(desemprego = desemprego + 1) %>%
    mutate(across(everything(), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    drop_na()
  modelo_endo
} else {

  modelo_endo <- dados %>%
    dplyr::select(cambio, DA_variable,desemprego,ipcaindice) %>%
    mutate(desemprego = desemprego + 1) %>%
    mutate(across(everything(), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    drop_na()
  
}

# Definição dos choques e respostas
response <- grep('ipcaindice', colnames(modelo_endo))
cambio_shock <- grep('cambio', colnames(modelo_endo))
  
# Escolha das variáveis exógenas, caso exista
# OBS: é necessário ajustar a configuração na função de estimação

# Nome do Modelo para exportar figuras

# Primeira parte: variáveix exógenas
# Segunda parte: variáveis endógenas de demanda agregada
if (model_trend == 1){
  name_trend = 'trend'
} else {
  name_trend = 'notrend'
}
name_trend
if (contemp_effect == 1){
  contemp_effect_lp = modelo_exog
  contemp_effect = '_CE'
} else {
  contemp_effect_lp = NULL
  contemp_effect = ''
}

if (comm_endo == T) {
  nome_modelo = paste0(toupper(modelo),
                       '_exo',
                       '[',
                       name_trend,
                       ']_endo[',
                       DA_variable,
                       paste0('(',as.character(lag_endog),')]'),
                       paste0('_gamma[',as.character(gamma_transition),']')
  )
} else {
nome_modelo = paste0(toupper(modelo),
                     '_exo',
                     contemp_effect,
                     '[',
                     'comm',
                     paste0('(',as.character(lag_exog),')_'),
                     name_trend,
                     ']_endo[',
                     DA_variable,
                     paste0('(',as.character(lag_endog),')]'),
                     paste0('_gamma[',as.character(gamma_transition),']')
                     )
}
# Função lp_nl ####

# Seleção de defasagem ótima
VARselect(modelo_endo)
cambio_switching <- dados %>%
  dplyr::select(cambio) %>%
  slice(-1)
  


lag_endog <- VARselect(modelo_endo)$selection[2]


lag <- stats::lag
# Estimando as projeções locais
?lp_nl
## Parâmetros
if (comm_endo == T) {
  results_nl <- lp_nl(
    modelo_endo, # Variáveis endógenas
    lags_endog_lin = lag_endog, # Lags do modelo
    lags_endog_nl = lag_endog, # Lags do modelo
    shock_type = 0, # Tipo de choques: no caso, 0 é de 1 desvio padrão
    confint = 1.65, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = hor_lps, # Horizonte para as LP
    switching = modelo_endo['cambio'], # Definição da série de transição
    lag_switching = T, # Uso da variável de transição de forma defasada
    use_hp = T, # Usar filtro de HP para decompor 
    lambda = lambda_hp, # Lambda para o filtro HP, 14400 é mensal
    trend = model_trend, # Sem variável de tendência
    gamma = gamma_transition, # Definição de gamma para a função de transição
    contemp_data = NULL, # Variáveis exógenas com efeito contemporâneo
    exog_data = NULL, # Variáveis exógenas com efeitos defasados
    lags_exog = NULL # Lags das variáveis exógenas
    )
} else {
  results_nl <- lp_nl(
    modelo_endo, # Variáveis endógenas
    lags_endog_lin = lag_endog, # Lags do modelo
    lags_endog_nl = lag_endog, # Lags do modelo
    shock_type = 0, # Tipo de choques: no caso, 0 é de 1 desvio padrão
    confint = 1.65, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = hor_lps, # Horizonte para as LP
    switching = cambio_switching, # Definição da série de transição
    lag_switching = T, # Uso da variável de transição de forma defasada
    use_hp = T, # Usar filtro de HP para decompor 
    lambda = lambda_hp, # Lambda para o filtro HP, 14400 é mensal
    trend = model_trend, # Sem variável de tendência
    gamma = gamma_transition, # Definição de gamma para a função de transição
    contemp_data = contemp_effect_lp, # Variáveis exógenas com efeito contemporâneo
    exog_data = modelo_exog, # Variáveis exógenas com efeitos defasados
    lags_exog = lag_exog # Lags das variáveis exógenas
)
}

#######################################################################
#                                                                     #
#                               GRÁFICOS                              #
#                                                                     #
#######################################################################

if (comm_endo == T) {
source('~/Dissertacao/Scripts/NonlinearLP_graphs_5_variables.R', echo=F)
} else {
source('~/Dissertacao/Scripts/NonlinearLP_graphs_4_variables.R', echo=F)
}
nome_modelo
