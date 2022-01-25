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
comm_endo = T

# Frequência (mensal ou trim)
modelo = 'mensal'

# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 0

# Variável de demanda agregada (capacidade ou pimpf).
# Caso o modelo seja trim, também podem ser (pib ou pib_hiato)
DA_variable = 'pib'

# Horizonte das LP's
hor_lps <- 18

# Gamma da função de transição
gamma_transition = 3

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 2

# Lags das variáveis exógenas
lag_exog = 1

# Variável de inflação externa
ext_inflation = 'petro'

# Lags da variável de transição
lag_switch_variable = T

# Incluir dummy da GFC
include_gfc_dummy = T

# Intervalo de confiança das IRFs
sig_IC = 95

# Incluir taxa de desemprego?
desemprego_on = T

# Taxa de Desemprego em variação percentual
desemprego_diff = F


# Rodando código de estimação

source('Code/Model_Estimation.R', verbose = F)

modelo_endo
VARselect(modelo_endo)
nome_modelo
var_modelo <- VAR(modelo_endo, p = 1)
roots(var_modelo)
# Rodando vários modelos ####

# Escolhas consolidadas ####
# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 1
 
# Lags da variável de transição
lag_switch_variable = T

# Frequência (mensal ou trim)
modelo = 'mensal'

# Horizonte das LP's
hor_lps <- 18

# Intervalo de confiança das IRFs
sig_IC = 95

# Inflação externa endógena 
comm_endo = T

# Outras escolhas ####

# Lags das variáveis exógenas
lag_exog = 1

# Variável de inflação externa
ext_inflation = 'comm'

# Taxa de Desemprego em variação percentual
desemprego_diff = F

# Rodando for loop para gerar os modelos ####
# Caso interrompa loop
path_directory <- '/home/luanmugarte/Artigos/Asym_ERPT'
setwd(path_directory)

# Lista de variáveis endógenas
endo_list <- c('capacidade','pimpf','pib','pib_hiato')
exo_list <- c('comm','petro')

# Incluir dummy da GFC
include_gfc_dummy = T

# Lista de outras opções
lags_option <- c(1:3)
gamma_option <- c(3:6)
CI_option <- c(90,95)

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
first_loop <- lags_option
second_loop <- gamma_option

for (i in first_loop){
  for (j in second_loop) {
    
    lag_endog = i
    DA_variable = 'pib'
    ext_inflation = 'petro'
    gamma_transition = j
    sig_IC = 90
    nome_modelo = 'default'
    desemprego_on = T
    
    try( source('Code/Model_Estimation.R', verbose = F), silent = T )
    
    if (dir.exists(file.path('Output/Figures', nome_modelo))) {
      counter = counter + 1
      print(paste0(counter," model(s) run!"))
    }
      loop_counter <- loop_counter + 1
    }
  if (loop_counter == length(first_loop)*length(second_loop)) {
    print("Done!")
  }
      
}

