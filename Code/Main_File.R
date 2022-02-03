#---------------------------------------------------------------------#
#                                                                     #
#                         ARQUIVO PRINCIPAL                           #
#                                                                     #
#---------------------------------------------------------------------#


# Caminho para o diretório padrão ####
setwd(here::here('Artigos','Asym_ERPT'))


# Rodando modelo específico ####

# Comm exógeno ou endógeno
comm_endo = T

# Frequência (mensal ou trim)
modelo = 'mensal'

# Índice de inflação
inflation_index = 'ipca'

# Tendência (1) ou sem tendência (0)
model_trend = 0

# Efeito contemporâneo presente (1) ou ausente  (0) da variável exógena
contemp_effect = 0

# Variável de demanda agregada (capacidade ou pimpf).
# Caso o modelo seja trim, também podem ser (pib ou pib_hiato)
DA_variable = 'pib_dessaz'

# Horizonte das LP's
hor_lps <- 18

# Gamma da função de transição
gamma_transition = 60

# Lags das variáveis endógenas
# Escolhido endogenamento pelo criterio HQ
lag_endog = 2

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
desemprego_on = T

# Taxa de Desemprego em variação percentual
desemprego_diff = F

# Desemprego como variável exógena
desemprego_exog = F

# Rodando código de estimação
source(here::here('Code','Model_Estimation.R'), verbose = F)

VARselect(modelo_endo)

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

# Variável de inflação externa
ext_inflation = 'comm'
 
# # Variável endógena
# DA_variable = 'pib_dessaz'

# Índice de inflação
inflation_index = 'ipca'

# Lags das variáveis exógenas
lag_exog = 1

# Outras escolhas ####

# Incluir dummy da GFC
include_gfc_dummy = T

# Taxa de desemprego no modelo
desemprego_on = T

# Taxa de Desemprego em variação percentual
desemprego_diff = T

# Taxa de desemprego como variável exógena (mais apropriado para comparação de índices de inflação)
desemprego_exog = T

# Rodando for loop para gerar os modelos ####


# Lista de outras opções
lags_option <- c(2,3)
gamma_option <- c(6,8,12)

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
first_loop <- lags_option
second_loop <- gamma_option

for (i in first_loop){
  for (j in second_loop) {
    # Caso interrompa loop
    path_directory <- '/home/luanmugarte/Artigos/Asym_ERPT'
    setwd(path_directory)

    
    lag_endog = i
    gamma_transition = j
    nome_modelo = 'default'
    DA_variable = 'pib'
    

    try(source('Code/Model_Estimation.R', verbose = F), silent = F )
    
    
    if (dir.exists(file.path('Output/Figures', nome_modelo))) {
      counter = counter + 1
      print(paste0(counter*2," model(s) run!"))
    }
      loop_counter <- loop_counter + 1
    }
  if (loop_counter == length(first_loop)*length(second_loop)) {
    print("Done!")
  }
      
}


modelo_exog
DA_variable = 'pib_ipca'
modelo_endo <- dados %>%
  dplyr::select(all_of(ext_inflation),cambio,all_of(DA_variable),all_of(inflation_index)) %>%
  mutate(across(!c(all_of(inflation_index)), ~ (as.numeric(.) - dplyr::lag(as.numeric(.)))/dplyr::lag(as.numeric(.)))) %>%
  mutate(across(c(all_of(inflation_index)), ~ as.numeric(.))) %>%
  drop_na()
modelo_endo
VARselect(modelo_endo, lag.max = 24)
