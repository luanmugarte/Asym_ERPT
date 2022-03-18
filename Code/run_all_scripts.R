#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#

# Loading functions 
source(here::here('Code','functions','data_and_model_functions.R'))
source(here::here('Code','functions','plot_functions.R'))

# Loading packages and getting the data
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
lag_endog = 4

# Lags das variáveis exógenas
lag_exog = 1

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

# Decomposição de Cholesky
# chol_decomp = diag(as.character(NA), nrow = 5, ncol = 5)

chol_decomp = NULL

# Ordem das variáveis endógenas
vars_order <- c(ext_inflation, 'cambio',DA_variable,inflation_index,'taxa_juros')

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
