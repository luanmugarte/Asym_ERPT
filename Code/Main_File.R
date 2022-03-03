#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#


# Caminho para o diretório padrão ####
setwd('~/Artigos/Asym_ERPT')

# Loading functions 
source(here::here('Code','functions','data_and_model_functions.R'))
source(here::here('Code','functions','plot_functions.R'))

# Running main functions once
load_packages_and_data()

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
gamma_transition <- 8

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 4

# Lags das variáveis exógenas
lag_exog = 1

# Variável de inflação externa
ext_inflation = 'comm'

# Lags da variável de transição
lag_switch_variable = F

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
lambda_hp = 14400

# Decomposição de Cholesky
# chol_decomp = diag(as.character(NA), nrow = 4, ncol = 4)

chol_decomp = NULL

# Rodando função de estimação
model_obj <- get_model_specification()
model_specs <- model_obj[[1]]
model_data <- model_obj[[2]]

model_results <- run_models(model_data,model_specs)
results_nl <- model_results[[1]]
results_lin <- model_results[[2]]

# Exportando figures
export_figures(results_nl,results_lin,model_specs)

setwd('~/Artigos/Asym_ERPT')

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
# lag_exog = 1

# Outras escolhas #

# Incluir dummy da GFC
include_gfc_dummy = F

# Taxa de desemprego no modelo
desemprego_on = F

# Taxa de Desemprego em variação percentual
desemprego_diff = F

# Taxa de desemprego como variável exógena (mais apropriado para comparação de índices de inflação)
desemprego_exog = F

# Decomposição de Cholesky
# chol_decomp = diag(as.character(NA), nrow = 4, ncol = 4)

chol_decomp = matrix(c(c(NA,0,0,0),
                       c(0,NA,0,0),
                       c(0,0,NA,0),
                       c(NA,NA,0,NA)),
                     nrow= 4, ncol =4, byrow= T)
# chol_decomp = NULL

# Rodando for loop para gerar os modelos #


# Lista de outras opções
lags_option <- c(2,3,4,5)
DA_option <- c('pib','pib_hiato_real','pimpf')

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
first_loop <- lags_option
second_loop <- DA_option

# Caso interrompa loop
path_directory <- '/home/luanmugarte/Artigos/Asym_ERPT'
setwd(path_directory)

for (i in first_loop){
  for (j in second_loop) {

    lag_endog = i
    gamma_transition = 8
    nome_modelo = 'default'
    lambda_hp = 192600
    DA_variable = j
    
    model_obj <- get_model_specification()
    model_specs <- model_obj[[1]]
    model_data <- model_obj[[2]]
    
    model_results <- run_models(model_data,model_specs)
    results_nl <- model_results[[1]]
    results_lin <- model_results[[2]]
    
    # Exportando figures
    export_figures(results_nl,results_lin,model_specs)
    
    if (dir.exists(file.path('Output/Figures', model_specs$nome_modelo))) {
      counter = counter + 1
      print(paste0(counter," model(s) run!"))
    }
      loop_counter <- loop_counter + 1
    }
  if (loop_counter == length(first_loop)*length(second_loop)) {
    print("Done!")
  }
      
}

VARselect(modelo_endo, lag.max =24)
