#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#

# Loading functions 
source(here::here('Code','functions','data_and_model_functions.R'))
source(here::here('Code','functions','plot_functions_old.R'))

# Running main functions once
raw_data <- load_packages_and_data()

# Rodando modelo específico ####

# Comm exógeno ou endógeno
comm_endo = T

# Frequência (mensal ou trim)
model_frequency = 'mensal'

# Índice de inflação
inflation_index = 'ipca'

# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 0

# Variável de demanda agregada (capacidade ou pimpf).
# Caso o modelo seja trim, também podem ser (pib ou pib_hiato)
DA_variable = 'pimpf'
# Horizonte das LP's
hor_lps <- 18

# Gamma da função de transição
gamma_transition <- 12

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog <- 4

# Lags das variáveis exógenas
lag_exog <- 1

# Variável de inflação externa
ext_inflation = 'comm'

# Lags da variável de transição
lag_switch_variable = T

# Incluir dummy da GFC
include_gfc_dummy = F

# Intervalo de confiança das IRFs
sig_IC = 95

# Incluir taxa de desemprego?
desemprego_on = F

# Taxa de Desemprego em variação percentual
desemprego_diff = F

# Desemprego como variável exógena
desemprego_exog = F

# Parâmetro lambda do filtro HP
lambda_hp = 192600

# Incluir taxa de juros
include_interest_rate = T
include_selic = F

# NW Pre-white
pre_white = F

# Adjust Standard Erros
adjust_se = F

# Decomposição de Cholesky
# chol_decomp = diag(as.character(NA), nrow = 5, ncol = 5)

chol_decomp = NULL

# Ordem das variáveis endógenas
vars_order <- c(ext_inflation, 'cambio',DA_variable,'taxa_juros',inflation_index)

# Rodando função de estimação
model_obj <- get_model_specification(raw_data)
model_specs <- model_obj[[1]]
model_data <- model_obj[[2]]

model_results <- run_models(model_data,model_specs)
results_nl <- model_results[[1]]
results_lin <- model_results[[2]]

# Exportando figures
export_figures(results_nl,results_lin,model_specs)

VARselect(model_data$modelo_endo, lag.max = 12)

# Rodando vários modelos ####

# Escolhas consolidadas 
# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 0
 
# Lags da variável de transição
lag_switch_variable = T

# Frequência (mensal ou trim)
model_frequency = 'mensal'

# Horizonte das LP's
hor_lps <- 18

# Intervalo de confiança das IRFs
sig_IC = 95

# Inflação externa endógena 
comm_endo = T

# Variável de inflação externa
ext_inflation = 'comm'
 
# # Variável endógena
# DA_variable = 'pib_dessaz'

# Índice de inflação
inflation_index = 'ipca'

# Lags das variáveis exógenas
lag_exog = 1

# Outras escolhas #

# Incluir dummy da GFC
include_gfc_dummy = F

# Taxa de desemprego no modelo
desemprego_on = F

# Taxa de Desemprego em variação percentual
desemprego_diff = F

# Taxa de desemprego como variável exógena (mais apropriado para comparação de índices de inflação)
desemprego_exog = F

# Include interest rate as a endogenous variable
include_interest_rate = T

# Include
include_selic = F

# Decomposição de Cholesky

# Relações contemporâneas
chol_decomp_diag = diag(as.character(NA), nrow = 5, ncol = 5)
 
chol_decomp_LT = NULL

# NW Pre-white
pre_white = F

# Adjust Standard Erros
adjust_se = F

# Rodando for loop para gerar os modelos #

# Lista de outras opções
lags_option <- c(2:4)
DA_option <- c('pib_hiato_real', 'pimpf','capacidade','pib')
gamma_option <- c(6,8,10,12)
chol_option <- list(chol_decomp_diag, chol_decomp_LT)
chol_option

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
first_loop <- DA_option
second_loop <- chol_option

for (i in first_loop){
  for (j in second_loop) {
    lag_endog = 4
    gamma_transition = 12
    nome_modelo = 'default'
    lambda_hp = 14400
    DA_variable = i
    chol_decomp = j
    include_interest_rate = T

    # Ordem das variáveis
    vars_order <- c(ext_inflation, 'cambio',DA_variable,'taxa_juros',inflation_index)
    
    model_obj <- get_model_specification(raw_data)
    model_specs <- model_obj[[1]]
    model_data <- model_obj[[2]]
    
    model_results <- run_models(model_data,model_specs)
    results_nl <- model_results[[1]]
    results_lin <- model_results[[2]]
    
    # Exportando figures
    # export_figures(results_nl,results_lin,model_specs)
    
    lag_endog = 4
    gamma_transition = 12
    nome_modelo = 'default'
    lambda_hp = 192600
    DA_variable = i
    chol_decomp = j
    include_selic = F
    include_interest_rate = T
    
    
    # Ordem das variáveis
    vars_order <- c(ext_inflation, 'cambio',DA_variable,'taxa_juros',inflation_index)
    
    model_obj <- get_model_specification(raw_data)
    model_specs <- model_obj[[1]]
    model_data <- model_obj[[2]]
    
    model_results <- run_models(model_data,model_specs)
    results_nl <- model_results[[1]]
    results_lin <- model_results[[2]]
    
    # Exportando figures
    # export_figures(results_nl,results_lin,model_specs)
    
    if (dir.exists(file.path('Output/Figures', model_specs$nome_modelo))) {
      counter = counter + 1
      print(paste0(counter," model(s) run!"))
    }
      loop_counter <- loop_counter + 1
    }
  if (loop_counter == 2*length(first_loop)*length(second_loop)) {
    print("Done!")
  }
}      

cl            <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
RC_result[[1]] <- foreach( i        = 1:length(RC_DA),
                           .packages = pkgs_list) %dopar%{
                             
                             for (j in chol_option) {
                               
                               

                               counter <- counter + 1
                               RC_DA[[counter]] <- paste0(counter,j,'OK')
                               list(counter)
                             }
                           }

RC_result
RC_DA <-   vector("list", length = 3*4*2)
parallel::stopCluster(cl)
