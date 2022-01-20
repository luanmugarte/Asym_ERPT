# Configurações iniciais ####

if ( (!exists("dadosbrutos")) & (!exists("dadosbrutos_trim")) )  {
  source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Packages.R', verbose = F)
  source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Cleaning_data.R', verbose = F)
}



# Estabelecendo diretório padrão
#   (necessário pois é alterado futuramente para exportar gráficos em diretórios separados)
setwd('/home/luanmugarte/Artigos/Asym_ERPT/')

# Resgatando as variáveis do modelo
dadosbrutos_trim
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
contemp_effect = 1

# Variável de demanda agregada (capacidade ou pimpf).
# Caso o modelo seja trim, também podem ser (pib ou pib.hiato)
DA_variable = 'capacidade'

# Horizonte das LP's
hor_lps <- 18

# Gamma da função de transição
gamma_transition = 3

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 1

# Lags das variáveis exógenas
lag_exog = 1

# Lags da variável de transição
lag_switch_variable = F

# Seleção automática das variáveis do modelo ####
# Condicional para a determinação da frequencia das variáveis
if (modelo == 'mensal'){
  dados <- dadosbrutos
  date <- seq(1999.77,2020.23,1/12)
  lambda_hp = 14400
} else   {
  dados <- dadosbrutos_trim
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

# Ajuste dos dados da variável de transição (taxa de câmbio)
cambio_switching <- dados %>%
  dplyr::select(cambio) %>%
  slice(-1)

if(lag_switch_variable == T){
  lag_fz <- 1
} else {
  lag_fz <- 0
}


if (comm_endo == T) {
  if (DA_variable == "pib.hiato") {
  modelo_endo <- dados %>%
    dplyr::select(comm,cambio, DA_variable,desemprego,ipca) %>%
    mutate(across(!c(desemprego,ipca,DA_variable), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    mutate(across(c(desemprego,DA_variable,ipca), ~ as.numeric(.))) %>%
    mutate(ipca = ipca/100) %>%
    drop_na()
  } else {
  modelo_endo <- dados %>%
    dplyr::select(comm,cambio, DA_variable,desemprego,ipca) %>%
    mutate(across(!c(desemprego,ipca), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    mutate(desemprego = as.numeric(desemprego)) %>%
    mutate(ipca = as.numeric(ipca)/100) %>%
    drop_na()
  }
} else {
  
  modelo_endo <- dados %>%
    dplyr::select(cambio, DA_variable,desemprego,ipca) %>%
    mutate(across(!c(ipca,desemprego), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    mutate(ipca = as.numeric(ipca)/100) %>%
    mutate(desemprego = as.numeric(desemprego)) %>%
    drop_na()
  
}
modelo_endo

# Definição dos choques e respostas ####
response <- grep('ipca', colnames(modelo_endo))
cambio_shock <- grep('cambio', colnames(modelo_endo))

# Estimação #### 

# Configurações extras ####

# Escolha das variáveis exógenas, caso exista
# OBS: é necessário ajustar a configuração na função de estimação

# Nome do Modelo para exportar figuras

# Primeira parte: variáveis exógenas
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

# Seleção de defasagem ótima
VARselect(modelo_endo)

# lag_endog <- VARselect(modelo_endo)$selection[2]

lag <- stats::lag
# Estimando as projeções locais ####
dados

# Parâmetros e configurações
if (comm_endo == T) {
  results_nl <- lp_nl(
    modelo_endo, # Variáveis endógenas
    lags_endog_lin = lag_endog, # Lags do modelo
    lags_endog_nl = lag_endog, # Lags do modelo
    shock_type = 0, # Tipo de choques: no caso, 0 é de 1 desvio padrão
    confint = 1.96, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = hor_lps, # Horizonte para as LP
    switching = cambio_switching, # Definição da série de transição
    lag_switching = lag_switch_variable, # Uso da variável de transição de forma defasada
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
    confint = 1.96, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = hor_lps, # Horizonte para as LP
    switching = cambio_switching, # Definição da série de transição
    lag_switching = lag_switch_variable, # Uso da variável de transição de forma defasada
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

# Exportando figuras ####
if (comm_endo == T) {
source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Graphs_5_variables.R', echo = F, verbose = F)
} else {
source('/home/luanmugarte/Artigos/Asym_ERPT/Code/Graphs_4_variables.R', verbose =F)
}

nome_modelo
