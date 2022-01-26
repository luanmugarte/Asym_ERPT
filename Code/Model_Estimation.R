# Configurações iniciais ####

if ( (!exists("dadosbrutos")) & (!exists("dadosbrutos_trim")) )  {
  source('Code/Packages.R', verbose = F)
  source('Code/Cleaning_data_2000.R', verbose = F)
}

# Resgatando as variáveis do modelo
dadosbrutos_trim
dadosbrutos
length(dadosbrutos)
length(dadosbrutos_trim)
modelo = 'mensal'
# Seleção automática das variáveis do modelo ####
# Condicional para a determinação da frequencia das variáveis
if (modelo == 'mensal'){
  dados <- dadosbrutos
  date <- seq(2000,2020.23,1/12)
  lambda_hp = 14400
} else   {
  dados <- dadosbrutos_trim
  date <- seq(2000.26,2019.99,1/4)
  lambda_hp = 1600
}

# Variável de comm para o caso de ser exógeno
ext_inflation_df <- dados %>%
  dplyr::select(all_of(ext_inflation)) %>%
  mutate(across(everything(), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
  drop_na()
ext_inflation_df
# Precisa ser dataframe para a função lp_nl
modelo_exog <- data.frame(ext_inflation_df)

if (include_gfc_dummy == T & comm_endo == T) {
  # Caso em que só a dummy da GFC é incluída
  modelo_exog <- data.frame(dadosbrutos$gfc_dummy[2:length(dadosbrutos$gfc_dummy)])
  colnames(modelo_exog) <- 'gfc_dummy'
  contemp_effect_lp <- NULL
  lag_exog <- 1
} else if (include_gfc_dummy == T & comm_endo == F) {
  # Caso em que há dummy e inflação externa como variáveis exógenas
  modelo_exog <- data.frame(ext_inflation_df,dadosbrutos$gfc_dummy[2:length(dadosbrutos$gfc_dummy)])
  colnames(modelo_exog) <- 'gfc_dummy'
  contemp_effect_lp <- NULL
  lag_exog <- 1
} else if (include_gfc_dummy == F & comm_endo == F) {
  # Caso em que há somente inflação externa como variável exógena
  modelo_exog <- data.frame(ext_inflation_df)
  colnames(modelo_exog) <- 'ext_inflation'
  if (contemp_effect == 1){
    contemp_effect_lp = modelo_exog
    contemp_effect_name = '_CE'
  } else {
    contemp_effect_lp = NULL
    contemp_effect_name = ''
  }
} else {
  # Caso sem variáveis exógenas
  modelo_exog <- NULL
  contemp_effect_lp <- NULL
  lag_exog <- NULL
}

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

if (desemprego_diff == T) {
  if (comm_endo == T) {
    if (DA_variable == "pib_hiato") {
      modelo_endo <- dados %>%
        dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,ipca) %>%
        mutate(across(!c(ipca,all_of(DA_variable)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
        mutate(across(c(all_of(DA_variable),ipca), ~ as.numeric(.))) %>%
        mutate(ipca = ipca/100) %>%
        drop_na()
    } else {
      modelo_endo <- dados %>%
        dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,ipca) %>%
        mutate(desemprego = (1+as.numeric(desemprego))) %>%
        mutate(across(!c(ipca), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
        mutate(ipca = as.numeric(ipca)/100) %>%
        drop_na()
    }
  } else {
    
    modelo_endo <- dados %>%
      dplyr::select(cambio, all_of(DA_variable), desemprego, ipca) %>%
      mutate(desemprego = (1+as.numeric(desemprego))) %>%
      mutate(across(!c(ipca), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
      mutate(ipca = as.numeric(ipca)/100) %>%
      drop_na()
    
  }
  } else {
  
  if (comm_endo == T) {
    if (DA_variable == "pib_hiato") {
    modelo_endo <- dados %>%
      dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,ipca) %>%
      mutate(across(!c(desemprego,ipca,all_of(DA_variable)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
      mutate(across(c(desemprego,all_of(DA_variable),ipca), ~ as.numeric(.))) %>%
      mutate(ipca = ipca/100) %>%
      drop_na()
    } else {
    modelo_endo <- dados %>%
      dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,ipca) %>%
      mutate(across(!c(desemprego,ipca), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
      mutate(across(c(desemprego,ipca), ~ as.numeric(.))) %>%
      mutate(ipca = ipca/100) %>%
      drop_na()
    }
  } else {
    
    modelo_endo <- dados %>%
      dplyr::select(cambio, all_of(DA_variable), desemprego, ipca) %>%
      mutate(across(!c(ipca,desemprego), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
      mutate(across(c(desemprego,ipca), ~ as.numeric(.))) %>%
      mutate(ipca = ipca/100) %>%
      drop_na()
    
  }
}

if (desemprego_on == F) {
  modelo_endo <- modelo_endo %>%
    dplyr::select(!desemprego)
}

modelo_endo
modelo_exog

# Definição dos choques e respostas ####
response <- grep('ipca', colnames(modelo_endo))
cambio_shock <- grep('cambio', colnames(modelo_endo))

# Significancia do IC
 
if (sig_IC == 90){
  sig_conf_int = 1.68
} else {
  sig_conf_int = 1.96
}

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



if (comm_endo == T) {
  nome_modelo = paste0(toupper(modelo),
                       '_exo',
                       '[',
                       name_trend,
                       ']_endo[',
                       ext_inflation,
                       '_',
                       DA_variable,
                       paste0('(',as.character(lag_endog),')]'),
                       paste0('_gamma[',as.character(gamma_transition),']'),
                       sig_IC
  )
} else {
nome_modelo = paste0(toupper(modelo),
                     '_exo',
                     contemp_effect_name,
                     '[',
                     ext_inflation,
                     paste0('(',as.character(lag_exog),')_'),
                     name_trend,
                     ']_endo[',
                     DA_variable,
                     paste0('(',as.character(lag_endog),')]'),
                     paste0('_gamma[',as.character(gamma_transition),']'),
                     sig_IC
                     )
}
nome_modelo
# Seleção de defasagem ótima
VARselect(modelo_endo)

lag <- stats::lag
# Estimando as projeções locais ####

# Parâmetros e configurações
results_nl <- lp_nl(
  modelo_endo, # Variáveis endógenas
  lags_endog_lin = lag_endog, # Lags do modelo
  lags_endog_nl = lag_endog, # Lags do modelo
  shock_type = 0, # Tipo de choque: no caso, 0 é de 1 desvio padrão
  confint = sig_conf_int, # Intervalo de confiança de 95%
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

# 
# modelo_exog <- data.frame(ext_inflation_df)
# colnames(modelo_exog) <- 'ext_inflation'
# results_nl <- lp_nl(
#   modelo_endo, # Variáveis endógenas
#   lags_endog_lin = lag_endog, # Lags do modelo
#   lags_endog_nl = lag_endog, # Lags do modelo
#   shock_type = 0, # Tipo de choque: no caso, 0 é de 1 desvio padrão
#   confint = sig_conf_int, # Intervalo de confiança de 95%
#   use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
#   hor = hor_lps, # Horizonte para as LP
#   switching = cambio_switching, # Definição da série de transição
#   lag_switching = lag_switch_variable, # Uso da variável de transição de forma defasada
#   use_hp = T, # Usar filtro de HP para decompor 
#   lambda = lambda_hp, # Lambda para o filtro HP, 14400 é mensal
#   trend = model_trend, # Sem variável de tendência
#   gamma = gamma_transition, # Definição de gamma para a função de transição
#   contemp_data = contemp_effect_lp, # Variáveis exógenas com efeito contemporâneo
#   exog_data = modelo_exog, # Variáveis exógenas com efeitos defasados
#   lags_exog = lag_exog # Lags das variáveis exógenas
# )
  
#######################################################################
#                                                                     #
#                               GRÁFICOS                              #
#                                                                     #
#######################################################################

# Exportando figuras ####
source('Code/Plot_results.R', verbose = F, echo = F)
