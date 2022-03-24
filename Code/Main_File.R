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
chol_decomp = diag(as.character(NA), nrow = 5, ncol = 5)

# chol_decomp = NULL

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
include_interest_rate = F

# Include
include_selic = T

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
    export_figures(results_nl,results_lin,model_specs)
    
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
    export_figures(results_nl,results_lin,model_specs)
    
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

# Gráficos ####
specs <- model_specs
regime_2 <- plot_transition_function(results_nl, specs)$regime_2

plot_lst <- vector("list", length = specs$n_endo_variables)

for (i in 1:specs$n_endo_variables) {
  tryCatch(expr = {
    IRF_s2 <- suppressMessages(tibble(bind_cols(results_nl$irf_s2_mean[specs$response,,i],
                                                results_nl$irf_s2_up[specs$response,,i],
                                                results_nl$irf_s2_low[specs$response,,i]),
                                      .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
      # mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
      # mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
      # dplyr::select(!c(IRF_upper_base,IRF_lower_base))
      mutate(IRF_upper = IRF_upper_base) %>%
      mutate(IRF_lower = IRF_lower_base) %>%
      dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    
  },
  error = function(error_in_function){
    message("Error in tibble 5!")
    print(error_in_function)
  }
  )
  
  
  plot_lst[[i]] <- ggplot(IRF_s2)  + 
    geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY80") +
    geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY80") +
    geom_ribbon(IRF_s2 %>% filter(if_all(everything(), ~ .x > 0) |
                                    if_all(everything(), ~ .x < 0)),
                aes(x=c(0:hor_lps), ymin=IRF_low, ymax =IRF_upper), fill = "GREY90") +
    # geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY80") +
    geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
    geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'bisque3', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1), 
    ) +
    labs(title = paste0('Resposta de ',
                        stringr::str_to_upper(specs$inflation_index), 
                        ' ao choque de ',
                        specs$endo_variables[i],
                        ' - ',regime_2)) +
    ylab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_text(hjust = 0.5),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
            plot.title = ggtext::element_markdown(size = 9, colour = 'black'),
            axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
}


# ggplot(IRF_s2)  + 
#   geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
#   geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
#   geom_point(data = IRF_s2 %>% add_column(horizon = c(0:(hor_lps))) %>%
#                 filter(if_all(everything(), ~ .x > 0) |
#                          if_all(everything(), ~ .x < 0)),
#               aes(x=horizon, y = IRF),  colour = 'bisque3', size = 3,
#              shape = 24, fill = 'bisque3', stroke = 2) +
#   # geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY80") +
#   geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
#   geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
#   geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
#   geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'bisque3', size = 0.75) +
#   scale_x_continuous(name = "",breaks=seq(0,18,1), 
#   ) +
#   labs(title = paste0('Resposta de ',
#                       stringr::str_to_upper(specs$inflation_index), 
#                       ' ao choque de ',
#                       specs$endo_variables[i],
#                       ' - ',regime_2)) +
#   ylab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="right",
#           legend.title = element_text(hjust = 0.5),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
#           plot.title = ggtext::element_markdown(size = 9, colour = 'black'),
#           axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
#           axis.text.y = element_text(size=11,colour = 'black'))
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.67, height = .33, width = .5) +
#   draw_plot(plot_lst[[2]], x = 0.5, y = 0.67, height = .33, width = .5) +
#   draw_plot(plot_lst[[3]], x = 0, y = 0.33, height = .33, width = .5) +
#   draw_plot(plot_lst[[4]], x = 0.5, y = 0.33, height = .33, width = .5) +
#   draw_plot(plot_lst[[5]], x = 0.25, y = 0, height = .33, width = .5)

results_nl
specs <- model_specs

# Determinando os nomes dos regimes
regime_1 <- plot_transition_function(results_nl, specs)$regime_1
regime_2 <- plot_transition_function(results_nl, specs)$regime_2

# IRF - Regime 1

# Criando um vetor que contem os objetos dos plots
plot_lst <- vector("list", length = specs$n_endo_variables)

# for (i in 1:specs$n_endo_variables) {
  IRF_df <- bind_cols(0:18,
                    results_nl$irf_s1_mean[specs$response,,i],
                    results_nl$irf_s1_up[specs$response,,i],
                    results_nl$irf_s1_low[specs$response,,i],
                    results_nl$irf_s2_mean[specs$response,,i],
                    results_nl$irf_s2_up[specs$response,,i],
                    results_nl$irf_s2_low[specs$response,,i],
                                      .name_repair = ~ c('horizon',
                                                         'IRF s1',
                                                         'IRF_upper s1',
                                                         'IRF_lower s1',
                                                         'IRF s2',
                                                         'IRF_upper s2',
                                                         'IRF_lower s2'))
  
  IRF_df <- IRF_df %>%
    pivot_longer(cols = c(everything(),-horizon))
  
  IRF_df
  IRF_df %>%
    pivot_wider(names_from = variable)
  
  library(magrittr)
  IRF_df %>% filter(str_detect(variable,"[A-Za-z]*r s1*$"))
  IRF_df %>% filter(str_detect(variable,"[A-Za-z]*r s2*"))
  IRF_df %>% pivot_wider(names_from = variable) %>%
    select(matches("[A-Za-z]*r s1$"))
                    
  unique(IRF_df$horizon)
  ggplot(IRF_df)  + 
      geom_ribbon(data = IRF_df %>% pivot_wider(names_from = variable) %>%
                    select(matches("[A-Za-z]*r s1$")),
                  aes(x=0:18, ymin=`IRF_lower s1`, ymax =`IRF_upper s1`),
                  fill = "cadetblue", alpha = 0.4) +
      geom_ribbon(data = IRF_df %>% pivot_wider(names_from = variable) %>%
                    select(matches("[A-Za-z]*r s2$")),
                  aes(x=0:18, ymin=`IRF_lower s2`, ymax =`IRF_upper s2`),
                  fill = "indianred", alpha = 0.4) +
      geom_line(data = IRF_df %>% filter(str_detect(variable,"[A-Za-z]*r s1*$")),
                aes(x=horizon, y=value, linetype = variable),
                color = "cadetblue", alpha =  0.4, show.legend = F) +
      geom_line(data = IRF_df %>% filter(str_detect(variable,"[A-Za-z]*r s2*$")),
                aes(x=horizon, y=value, linetype = variable), 
                color = "indianred", alpha = 0.4, show.legend = F) +
      # geom_point(data = IRF_df %>% add_column(horizon = c(0:(hor_lps))) %>%
      #              filter(if_all(-horizon, ~ .x > 0) |
      #                       if_all(-horizon, ~ .x < 0)) %>%
      #              slice(-1),
      #            aes(x=horizon, y = IRF),  colour = 'cadetblue', size = 3,
      #            shape = 24, fill = 'cadetblue', stroke = 2) +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(data = IRF_df %>% filter(str_detect(variable,"[A-Za-z]*IRF s*")),
               aes(x=horizon, y=value, color = variable, linetype = variable)) +
      scale_x_continuous(name = "",breaks=seq(0,18,1), 
      ) +
      labs(title = paste0('Resposta de ',
                          stringr::str_to_upper(specs$inflation_index),
                          ' ao choque de ',
                          specs$endo_variables[i],
                          ' - ',
                          regime_1)) +
      guides()  +
      ylab('') +
      theme_classic() +
      theme(  panel.grid = element_blank(), 
              panel.border = element_blank(),
              legend.position="right",
              legend.title = element_text(hjust = 0.5),
              legend.text = element_text(size=10),
              legend.key = element_rect(colour = "black"),
              legend.box.background = element_rect(colour = "black", size = 1),
              plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
              plot.title = ggtext::element_markdown(size = 9, colour = 'black'),
              axis.text.x = element_text(angle = 45,
                                         vjust = 0.6, 
                                         hjust = 0.6,
                                         size=11,
                                         colour = 'black'),
              axis.text.y = element_text(size=11,colour = 'black'))
  
# }

ggdraw() +
  draw_plot(plot_lst[[1]], x = 0, y = 0.67, height = .33, width = .5) +
  draw_plot(plot_lst[[2]], x = 0.5, y = 0.67, height = .33, width = .5) +
  draw_plot(plot_lst[[3]], x = 0, y = 0.33, height = .33, width = .5) +
  draw_plot(plot_lst[[4]], x = 0.5, y = 0.33, height = .33, width = .5) +
  draw_plot(plot_lst[[5]], x = 0.25, y = 0, height = .33, width = .5)



ggsave(paste0('IRF_',
              stringr::str_to_upper(specs$inflation_index),
              '_',regime_1,'.png'),
       path = file.path('Output/Figures/', specs$nome_modelo),
       device = "png",width = 12, height = 8, units = "cm",scale = 2.5)


# IRF - Regime 2

# Criando um vetor que contem os objetos dos plots
plot_lst <- vector("list", length = specs$n_endo_variables)

for (i in 1:specs$n_endo_variables) {
  tryCatch(expr = {
    IRF_s2 <- suppressMessages(tibble(bind_cols(results_nl$irf_s2_mean[specs$response,,i],
                                                results_nl$irf_s2_up[specs$response,,i],
                                                results_nl$irf_s2_low[specs$response,,i]),
                                      .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
      # mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
      # mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
      # dplyr::select(!c(IRF_upper_base,IRF_lower_base))
      mutate(IRF_upper = IRF_upper_base) %>%
      mutate(IRF_lower = IRF_lower_base) %>%
      dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    
  },
  error = function(error_in_function){
    message("Error in tibble 5!")
    print(error_in_function)
  }
  )
  
  
  plot_lst[[i]] <- ggplot(IRF_s2)  + 
    geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
    geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
    geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
    geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
    geom_point(data = IRF_s2 %>% add_column(horizon = c(0:(hor_lps))) %>%
                 filter(if_all(-horizon, ~ .x > 0) |
                          if_all(-horizon, ~ .x < 0)) %>%
                 slice(-1),
               aes(x=horizon, y = IRF),  colour = 'bisque3', size = 3,
               shape = 24, fill = 'bisque3', stroke = 2) +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'bisque3', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1), 
    ) +
    labs(title = paste0('Resposta de ',
                        stringr::str_to_upper(specs$inflation_index), 
                        ' ao choque de ',
                        specs$endo_variables[i],
                        ' - ',regime_2)) +
    ylab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_text(hjust = 0.5),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
            plot.title = ggtext::element_markdown(size = 9, colour = 'black'),
            axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
}

ggdraw() +
  draw_plot(plot_lst[[1]], x = 0, y = 0.67, height = .33, width = .5) +
  draw_plot(plot_lst[[2]], x = 0.5, y = 0.67, height = .33, width = .5) +
  draw_plot(plot_lst[[3]], x = 0, y = 0.33, height = .33, width = .5) +
  draw_plot(plot_lst[[4]], x = 0.5, y = 0.33, height = .33, width = .5) +
  draw_plot(plot_lst[[5]], x = 0.25, y = 0, height = .33, width = .5)

ggsave(paste0('IRF_',
              stringr::str_to_upper(specs$inflation_index),
              '_',regime_2,'.png'),
       path = file.path('Output/Figures/', specs$nome_modelo),
       device = "png",width = 12, height = 8, units = "cm",scale = 2.5)