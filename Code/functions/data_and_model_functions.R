# Configurações iniciais ####

load_packages_and_data <- function() {
  # Function to load packages and data from scripts.
  if ( (!exists("dadosbrutos")) & (!exists("dadosbrutos_trim")) )  {
    tryCatch(expr = {
      source('Code/process/Packages.R', verbose = F)
    },
    error = function(error_in_function){
      message("Error in packages file!")
      print(error_in_function)
    }
    )
    tryCatch(expr = {
      source('Code/process/Cleaning_data.R', verbose = F)
    },
    error = function(error_in_function){
      message("Error in data cleaning file!")
      print(error_in_function)
    }
    )
  }
  
  if  (!exists("dadosbrutos")) {
    return(dadosbrutos_trim)
  } else {
    return(dadosbrutos)
  }
}

get_model_specification <- function(raw_data) {
  # Function to automatically build the arguments selected previously for the Local
  # Projections estimation functions (both linear and nonlinear versions). This function
  # also builds the naming convention for output figures directories.
  dados <- raw_data
  
  if ( model_frequency == 'mensal'){
    date <- seq(2000,2020.23,1/12)
    # lambda_hp = 129600
  } else   {
    date <- seq(2000.26,2019.99,1/4)
    lambda_hp = 1600
  }
  
  # Variável de comm para o caso de ser exógeno
  ext_inflation_df <- dados %>%
    dplyr::select(all_of(ext_inflation)) %>%
    mutate(across(everything(), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
    drop_na()
  
  # Precisa ser dataframe para a função lp_nl
  modelo_exog <- data.frame(ext_inflation_df)
  
  if (include_gfc_dummy == T & comm_endo == T) {
    # Caso em que só a dummy da GFC é incluída
    modelo_exog <- data.frame(dadosbrutos$gfc_dummy[2:length(dadosbrutos$gfc_dummy)])
    colnames(modelo_exog) <- 'gfc_dummy'
    contemp_effect_lp <- NULL
    lag_exog <- 16
  } else if (include_gfc_dummy == T & comm_endo == F) {
    # Caso em que há dummy e inflação externa como variáveis exógenas
    modelo_exog <- data.frame(ext_inflation_df,dadosbrutos$gfc_dummy[2:length(dadosbrutos$gfc_dummy)])
    colnames(modelo_exog) <- c(ext_inflation,'gfc_dummy')
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
  
  
  
  
  if (desemprego_diff == T) {
    if (comm_endo == T) {
      if (DA_variable == "pib_hiato_real" | DA_variable == "pib_hiato_nom") {
        modelo_endo <- dados %>%
          dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,all_of(inflation_index)) %>%
          mutate(across(!c(all_of(inflation_index),all_of(DA_variable)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
          mutate(across(c(all_of(DA_variable),all_of(inflation_index)), ~ as.numeric(.))) %>%
          drop_na()
      } else {
        modelo_endo <- dados %>%
          dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,all_of(inflation_index)) %>%
          mutate(desemprego = (1+as.numeric(desemprego))) %>%
          mutate(across(!c(all_of(inflation_index)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
          drop_na()
      }
    } else {
      
      modelo_endo <- dados %>%
        dplyr::select(cambio, all_of(DA_variable), desemprego, all_of(inflation_index)) %>%
        mutate(desemprego = (1+as.numeric(desemprego))) %>%
        mutate(across(!c(all_of(inflation_index)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
        drop_na()
      
    }
  } else {
    
    if (comm_endo == T) {
      if (DA_variable == "pib_hiato_real" | DA_variable == "pib_hiato_nom") {
        modelo_endo <- dados %>%
          dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,all_of(inflation_index)) %>%
          mutate(across(!c(desemprego,all_of(inflation_index),all_of(DA_variable)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
          mutate(across(c(desemprego,all_of(DA_variable),all_of(inflation_index)), ~ as.numeric(.))) %>%
          drop_na()
      } else {
        modelo_endo <- dados %>%
          dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),desemprego,all_of(inflation_index)) %>%
          mutate(across(!c(desemprego,all_of(inflation_index)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
          mutate(across(c(desemprego,all_of(inflation_index)), ~ as.numeric(.))) %>%
          drop_na()
      }
    } else {
      
      modelo_endo <- dados %>%
        dplyr::select(cambio, all_of(DA_variable), desemprego, all_of(inflation_index)) %>%
        mutate(across(!c(all_of(inflation_index),desemprego), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
        mutate(across(c(desemprego,all_of(inflation_index)), ~ as.numeric(.))) %>%
        drop_na()
      
    }
  }
  
  if (desemprego_on == F & desemprego_exog == F) {
    modelo_endo <- modelo_endo %>%
      dplyr::select(!desemprego)
  } else if (desemprego_on == F & desemprego_exog == T) {
    modelo_endo <- modelo_endo %>%
      dplyr::select(!desemprego)
    if (comm_endo == T | include_gfc_dummy == T){
      modelo_exog$desemprego <- dadosbrutos$desemprego[2:length(dadosbrutos$desemprego)]
      contemp_effect_lp <- NULL
      lag_exog <- 1
    } else {
      modelo_exog <- data.frame(dadosbrutos$desemprego[2:length(dadosbrutos$desemprego)])
      contemp_effect_lp <- NULL
      lag_exog <- 1
    }
  }
  
  if (include_interest_rate == T){
    modelo_endo$taxa_juros <- dados$taxa_juros[2:nrow(dados)]
  }
  
  # Ajuste dos dados da variável de transição (taxa de câmbio)
  cambio_switching <- dados %>%
    dplyr::select(cambio) %>%
    slice(-1)
  # cambio_switching <- modelo_endo %>%
  #   dplyr::select(cambio) 
  
  if(lag_switch_variable == T){
    lag_fz <- 1
  } else {
    lag_fz <- 0
  }
  
  modelo_endo <- modelo_endo %>%
    dplyr::select(c(everything(),-ipca),taxa_juros,ipca)
  
    # Definição dos choques e respostas ####
  response <- grep(paste0("^",inflation_index,"$"), colnames(modelo_endo))
  cambio_shock <- grep('cambio', colnames(modelo_endo))
  
  # Configurações extras #
  
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
  if (include_gfc_dummy == T) {
    dummy_gfc = 'GFC'
  } else {
    dummy_gfc = ''
  }
  
  
  if (comm_endo == T) {
    nome_modelo = paste0(toupper(model_frequency),
                         '_exo',
                         '[',
                         name_trend,
                         dummy_gfc,
                         ']_endo[',
                         ext_inflation,
                         '_',
                         DA_variable,
                         paste0('(',as.character(lag_endog),')]'),
                         paste0('_gamma[',as.character(gamma_transition),']'),
                         lambda_hp
    )
  } else {
    nome_modelo = paste0(toupper(model_frequency),
                         '_exo',
                         contemp_effect_name,
                         '[',
                         ext_inflation,
                         paste0('(',as.character(lag_exog),')_'),
                         name_trend,
                         dummy_gfc,
                         ']_endo[',
                         DA_variable,
                         paste0('(',as.character(lag_endog),')]'),
                         paste0('_gamma[',as.character(gamma_transition),']'),
                         lambda_hp
    )
  }
  
  
  # Number of endogenous variables
  n_endo_variables = ncol(modelo_endo)
  endo_variables = colnames(modelo_endo)
  
  model_specs <- list(model_frequency     = model_frequency,
                      include_gfc_dummy   = include_gfc_dummy,
                      comm_endo           = comm_endo,
                      contemp_effect_lp   = contemp_effect_lp,
                      lag_switch_variable = lag_switch_variable,
                      desemprego_diff     = desemprego_diff,
                      desemprego_on       = desemprego_on,
                      desemprego_exog     = desemprego_exog,
                      DA_variable         = DA_variable,
                      inflation_index     = inflation_index,
                      ext_inflation       = ext_inflation,
                      model_trend         = model_trend,
                      lag_endog           = lag_endog,
                      lag_exog            = lag_exog,
                      nome_modelo         = nome_modelo,
                      chol_decomp         = chol_decomp,
                      lambda_hp           = lambda_hp,
                      hor_lps             = hor_lps,
                      gamma_transition    = gamma_transition,
                      n_endo_variables    = n_endo_variables,
                      endo_variables      = endo_variables,
                      response            = response,
                      cambio_shock        = cambio_shock
                  
                      )
    
  model_data <- list( modelo_endo         = modelo_endo,
                      modelo_exog         = modelo_exog,
                      cambio_switching    = cambio_switching)
                      
                      
    
  return(list(model_specs,model_data))
}
# get_model_specification()

run_models <- function(data,specs){
  # Evitando conflitos entre pacotes
  lag <- stats::lag
  
  results_nl <- lp_nl(
    data$modelo_endo, # Variáveis endógenas
    lags_endog_lin = specs$lag_endog, # Lags do modelo
    lags_endog_nl = specs$lag_endog, # Lags do modelo
    shock_type = 0, # Tipo de choque: no caso, 0 é de 1 desvio padrão
    confint = 1.96, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = specs$hor_lps, # Horizonte para as LP
    switching = data$cambio_switching, # Definição da série de transição
    lag_switching = specs$lag_switch_variable, # Uso da variável de transição de forma defasada
    use_hp = T, # Usar filtro de HP para decompor 
    lambda = specs$lambda_hp, # Lambda para o filtro HP, 14400 é mensal
    trend = specs$model_trend, # Sem variável de tendência
    gamma = specs$gamma_transition, # Definição de gamma para a função de transição
    contemp_data = specs$contemp_effect_lp, # Variáveis exógenas com efeito contemporâneo
    exog_data = data$modelo_exog, # Variáveis exógenas com efeitos defasados
    lags_exog = specs$lag_exog, # Lags das variáveis exógenas
    nw_prewhite = F,
    adjust_se = F,
    chol_decomp = specs$chol_decomp
  )
  
  
  results_lin <- lp_lin(
    data$modelo_endo, # Variáveis endógenas
    lags_endog_lin = specs$lag_endog, # Lags do modelo
    shock_type = 0, # Tipo de choque: no caso, 0 é de 1 desvio padrão
    confint = 1.96, # Intervalo de confiança de 95%
    use_nw = T, # Usar erros padrão de Newey-West para as respostas ao impulso (correção de viés)
    hor = specs$hor_lps, # Horizonte para as LP
    trend = specs$model_trend, # Sem variável de tendência
    contemp_data = specs$contemp_effect_lp, # Variáveis exógenas com efeito contemporâneo
    exog_data = data$modelo_exog, # Variáveis exógenas com efeitos defasados
    lags_exog = specs$lag_exog, # Lags das variáveis exógenas
    nw_prewhite = F,
    adjust_se = F,
    chol_decomp = specs$chol_decomp
  )
  
  return(list(results_nl,results_lin))
}