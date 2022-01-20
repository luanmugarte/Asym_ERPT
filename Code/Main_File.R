#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#


# Caminho para o diretório padrão ####

path_directory <- '/home/luanmugarte/Artigos/Asym_ERPT'
setwd(path_directory)


# Rodando modelo específico ####

# Comm exógeno ou endógeno
comm_endo = F

# Frequência (mensal ou trim)
modelo = 'mensal'

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
gamma_transition = 3

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 1

# Lags das variáveis exógenas
lag_exog = 1

# Variável de inflação externa
ext_inflation = 'petro'

# Lags da variável de transição
lag_switch_variable = F

# Incluir dummy da GFC
include_gfc_dummy = T

# Intervalo de confiança das IRFs
sig_IC = 95

# Rodando código de estimação
source('Code/Model_Estimation.R', verbose = F)

# Rodando vários modelos ####

# Escolhas consolidadas ####
# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 1
 
# Lags das variáveis exógenas
lag_exog = 1

# Lags da variável de transição
lag_switch_variable = F

# Frequência (mensal ou trim)
modelo = 'mensal'

# Horizonte das LP's
hor_lps <- 18

# Intervalo de confiança das IRFs
sig_IC = 95

# Outras escolhas ####

# Gamma da função de transição
gamma_transition = 3

# Lags das variáveis endógenas
lag_endog = 4

# Variável de inflação externa
ext_inflation = 'petro'

# Incluir dummy da GFC
include_gfc_dummy = T

# Rodando for loop para gerar os modelos ####
# Caso interrompa loop
setwd(path_directory)

# Lista de variáveis endógenas
variables_list <- c('capacidade','pimpf','pib','pib_hiato')

# Lista de outras opções
comm_endo_option <- c(TRUE,FALSE)
lags_option <- c(1:4)
gamma_option <- c(2:7)

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
for (i in variables_list){
  for (j in lags_option) {
    
    DA_variable = i
    gamma_transition = 4
    lag_endog = j
    nome_modelo = 'Loop'
    comm_endo = T
    try( source('Code/Model_Estimation.R', verbose = F), silent = T )
    
    if (dir.exists(file.path('Output/Figures', nome_modelo))) {
      counter = counter + 1
      print(paste0(counter," model(s) run!"))
    }
      loop_counter <- loop_counter + 1
    }
  if (loop_counter == length(variables_list)*length(comm_endo_option)) {
    print("Done!")
  }
      
}

sig_IC
