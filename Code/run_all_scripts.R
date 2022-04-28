#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#

# Loading functions 
source(here::here('Code','functions','data_and_model_functions.R'), verbose = F)
source(here::here('Code','functions','plot_functions_v2.R'), verbose = F)

# Running main functions once
raw_data <- load_packages_and_data()

# Rodando modelo específico ####

# Índice de inflação
inflation_index = 'ipca'

# Variável de demanda agregada
DA_variable = 'pimpf'

# Gamma da função de transição
gamma_transition <- 12

# Variável de inflação externa
ext_inflation = 'comm'

# Incluir taxa de juros
include_interest_rate = T

# Incluir selic 
include_selic = F

# Endogenous variables lag structure
lag_endog = 4

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

# Gráficos --------

# Função de transição
plot_transition_function(results_nl,model_specs)

# IRFs
plot_IRFs(results_nl, model_specs)

# ERPT - Belaisch
plot_Belaisch_ERPT(results_lin, results_nl, model_specs)

# Robustness checks
source(here::here('Code','robustness_checks.R'), verbose = F, echo = F)
