# Construção dos gráficos dos resultados das LP's

# Funções para os modelos não lineares -----------------------------------------

# Função de transição da estimação ####

# Regime 1 é a probabilidade de (1 - evento da variável de transição).
# Regime 2 é a probabilidade do evento da variável de transição. 
plot_transition_function <- function(results_nl,specs) {
  transition_function <- as.xts(ts(results_nl$fz,
                                     start = c(2000,2),
                                     end = c(2020,1),
                                     frequency = 12))
  
  date <- time(transition_function)
  if ( (transition_function['2002:8'] < 0.2) &&
       (transition_function['2002:9'] < 0.2) &&
       (transition_function['2002:10'] < 0.2)) {
    regime_1 = 'Regime de Depreciação'
    regime_2 = 'Regime de Apreciação' 
  } else {
    regime_1 = 'Regime de Apreciação'
    regime_2 = 'Regime de Depreciação' 
  }
 
  
  date <- date[(max(c(lag_endog,lag_exog))):(length(date)-1)]

  # Criando dataframe dos dados
  # tryCatch é usado para evitar uma mensagem chata de "New names"
  tryCatch(expr = {df <- suppressMessages(
    as_tibble(bind_cols(results_nl$fz,date),
              .name_repair = ~vctrs::vec_as_names(c('transition_function',
                                                    'date'), 
                                                  repair = "unique", 
                                                  quiet = TRUE)))},
           error = function(error_in_function){
             message("Error in tibble!")
             print(error_in_function)
           }
  )
  df
  
  # Plotando o gráfico da função de transição
  ggplot(df)  + 
    geom_line(aes(x=date, y=transition_function),
              size = 0.75, color = 'darkred') +
    scale_x_continuous(breaks=seq(2000,2020.23,1),                       
                       labels=paste0(c(""),
                                     c(rep(2000:2019,each=1),2020)),
                       expand = c(0.01, 0)) +
    labs(title = '') +
    scale_fill_brewer(palette="Blues") +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.5,
                                       hjust = 0.6,
                                       size=12, 
                                       colour = 'black',
                                       face = 'bold'),
            axis.text.y = element_text(size=12,face = 'bold')) 
  
  ggsave(paste0('Função de Transição - ',
                       regime_2,
                       ' - gamma = ',
                       as.character(gamma_transition)),
         path = file.path('Output/Figures/'),
         device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  # End function
  return(list(regime_1 = regime_1,regime_2 = regime_2))
}


# Funções de Impulso Resposta com 5 variáveis endógenas (5 gráficos de IRFs) ####
plot_IRFs <- function(results_nl,specs) {
  # Elabora os 5 gráficos das IRF's 
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #                     Funções Impulso Resposta                        #
  #                                                                     #
  #---------------------------------------------------------------------#
  

  
  # Determinando os nomes dos regimes
  transition_function <- as.xts(ts(results_nl$fz,
                                   start = c(2000,2),
                                   end = c(2020,1),
                                   frequency = 12))
  
  date <- time(transition_function)
  if ( (transition_function['2002:8'] < 0.2) &&
       (transition_function['2002:9'] < 0.2) &&
       (transition_function['2002:10'] < 0.2)) {
    regime_1 = 'Regime de Depreciação'
    regime_2 = 'Regime de Apreciação' 
  } else {
    regime_1 = 'Regime de Apreciação'
    regime_2 = 'Regime de Depreciação' 
  }
 
  # Criando um vetor que contem os objetos dos plots
  plot_lst <- vector("list", length = specs$n_endo_variables)
  
  for (i in 1:specs$n_endo_variables) {
    tryCatch(expr = {
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
      
      IRF_df

    },
    error = function(error_in_function){
      message("Error in tibble 5!")
      print(error_in_function)
    }
    )
    
    
    IRF_df %>%
      select(matches('* s2$*') | matches('horizon')) %>%
      filter(if_all(-horizon, ~ .x > 0) |
               if_all(-horizon, ~ .x < 0)) %>%
      slice(-1)
    
    ggplot(IRF_df)  + 
      geom_ribbon(aes(x=0:18, ymin=`IRF_lower s1`, ymax =`IRF_upper s1`),
                  fill = "indianred", alpha = 0.3) +
      geom_line(aes(x=horizon, y=`IRF_lower s2`),color = "GREY50", alpha =  1, show.legend = F, size = 1) +
      geom_line(aes(x=horizon, y=`IRF_upper s2`),color = "GREY50", alpha =  1, show.legend = F, size = 1) +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=horizon, y=`IRF s1`), color = 'indianred',linetype = 'dashed',size = 1) +
      geom_line(aes(x=horizon, y=`IRF s2`), color = 'GREY10', linetype = 'solid' , size = 1) +
      geom_point(data = IRF_df %>%
                   select(matches('* s1$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s1`),  size = 4,
                 shape = 24, fill = 'indianred') +
      geom_point(data = IRF_df %>%
                   select(matches('* s2$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s2`),  size = 4,
                 shape = 24, fill = 'GREY50') +
      scale_x_continuous(name = "",breaks=seq(0,18,1)) +
      scale_y_continuous(name = "",
                         breaks = scales::pretty_breaks(n = 8),
                         expand = c(0, 0)) +
      
      labs(title = '') +
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
    
    ggsave(paste0('Resposta de ',
                  stringr::str_to_upper(specs$inflation_index),
                  ' ao choque de ',
                  specs$endo_variables[i],
                  ' - ',
                  regime_1,'.png'),
           path = file.path('Output/Figures/'),
           device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  }
}

# Funções lineares -----------

# plot_lin_results <- function(results_lin,specs) {
#   # Elabora os 5 gráficos das IRF's e do cálculo do repasse cambial conforme Belaisch (2003). 
#   
#   #---------------------------------------------------------------------#
#   #                                                                     #
#   #                     Funções Impulso Resposta                        #
#   #                                                                     #
#   #---------------------------------------------------------------------#
#   
#   # IRF - CAMBIO -> IPCA
#   
#   # Criando um vetor que contem os objetos dos plots
#   plot_lst <- vector("list", length = specs$n_endo_variables)
#   
#   for (i in 1:specs$n_endo_variables) {
#     tryCatch(expr = {
#       IRF_lin <- suppressMessages(tibble(bind_cols(results_lin$irf_lin_mean[specs$response,,i],
#                                                   results_lin$irf_lin_up[specs$response,,i],
#                                                   results_lin$irf_lin_low[specs$response,,i]),
#                                         .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
#       #   mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
#       #   mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
#       #   dplyr::select(!c(IRF_upper_base,IRF_lower_base))
#         mutate(IRF_upper = IRF_upper_base) %>%
#         mutate(IRF_lower = IRF_lower_base) %>%
#         dplyr::select(!c(IRF_upper_base,IRF_lower_base))
#     },
#     error = function(error_in_function){
#       message("Error in tibble 8!")
#       print(error_in_function)
#     }
#     )
#     
#     
#     
#     plot_lst[[i]] <- ggplot(IRF_lin)  + 
#       geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
#       geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
#       geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
#       geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
#       geom_point(data = IRF_lin %>% add_column(horizon = c(0:(hor_lps))) %>%
#                    filter(if_all(-horizon, ~ .x > 0) |
#                             if_all(-horizon, ~ .x < 0)) %>%
#                    slice(-1),
#                 aes(x=horizon, y = IRF),  colour = 'cadetblue', size = 3,
#                 shape = 24, fill = 'cadetblue', stroke = 2) +
#       geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
#       geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'cadetblue', size = 0.75) +
#       scale_x_continuous(name = "",breaks=seq(0,18,1), 
#       ) +
#       labs(title = paste0('Resposta de ',
#                           stringr::str_to_upper(inflation_index),
#                           ' ao choque de ',
#                           specs$endo_variables[i])) +
#       ylab('') +
#       theme_classic() +
#       theme(  panel.grid = element_blank(), 
#               panel.border = element_blank(),
#               legend.position="right",
#               legend.title = element_text(hjust = 0.5),
#               legend.text = element_text(size=10),
#               legend.key = element_rect(colour = "black"),
#               legend.box.background = element_rect(colour = "black", size = 1),
#               plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
#               plot.title = ggtext::element_markdown(size = 9, colour = 'black'),
#               axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
#               axis.text.y = element_text(size=11,colour = 'black'))
#     
#   }
#   
#   if (results_lin$specs$endog == 4) { 
#     ggdraw() +
#       draw_plot(plot_lst[[1]], x = 0, y = 0.5, height = .5, width = .5) +
#       draw_plot(plot_lst[[2]], x = 0.5, y = 0.5, height = .5, width = .5) +
#       draw_plot(plot_lst[[3]], x = 0, y = 0, height = .5, width = .5) +
#       draw_plot(plot_lst[[4]], x = 0.5, y = 0, height = .5, width = .5)
#   } else {
#     ggdraw() +
#       draw_plot(plot_lst[[1]], x = 0, y = 0.67, height = .33, width = .5) +
#       draw_plot(plot_lst[[2]], x = 0.5, y = 0.67, height = .33, width = .5) +
#       draw_plot(plot_lst[[3]], x = 0, y = 0.33, height = .33, width = .5) +
#       draw_plot(plot_lst[[4]], x = 0.5, y = 0.33, height = .33, width = .5) +
#       draw_plot(plot_lst[[5]], x = 0.25, y = 0, height = .33, width = .5)
#   }
#   
#   ggsave(paste0('IRF_linear_',
#                 stringr::str_to_upper(specs$inflation_index),'.png'),
#          path = file.path('Output/Figures/', specs$nome_modelo),
#          device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
#   
#   
#   #---------------------------------------------------------------------#
#   #                                                                     #
#   #             Repasse cambial conforme Belaisch (2003)                #
#   #                                                                     #
#   #---------------------------------------------------------------------#
#   
#   # Criando tibble com os dados de CI e da LP (Regime 1)
#   tryCatch(expr = {
#     RC_lin <- suppressMessages(tibble(bind_cols(cumsum((results_lin$irf_lin_mean[specs$response,,specs$cambio_shock])
#                                                       /cumsum(results_lin$irf_lin_mean[specs$cambio_shock,,specs$cambio_shock])),
#                                                results_lin$irf_lin_mean[specs$response,,specs$cambio_shock],
#                                                results_lin$irf_lin_up[specs$response,,specs$cambio_shock],
#                                                results_lin$irf_lin_low[specs$response,,specs$cambio_shock],
#                                                results_lin$irf_lin_mean[specs$cambio_shock,,specs$cambio_shock],
#                                                results_lin$irf_lin_up[specs$cambio_shock,,specs$cambio_shock],
#                                                results_lin$irf_lin_low[specs$cambio_shock,,specs$cambio_shock]),
#                                      .name_repair = ~ c('RC',
#                                                         'Inflation_Index_mean',
#                                                         'Inflation_Index_upper',
#                                                         'Inflation_Index_lower',
#                                                         'cambio_mean',
#                                                         'cambio_upper',
#                                                         'cambio_lower')))
#   },
#   error = function(error_in_function){
#     message("Error in tibble 9!")
#     print(error_in_function)
#   }
#   )
#   
#   
#   df.new <- RC_lin %>%
#     mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)),
#                                 1,
#                                 0))
#   df.new$sig_RC[1] <- 0 # Primeiro período nunca tem IC
#   df.new
#   
#   # Criando o dataframe que estabelece os momentos a serem localizados pelo ggplot
#   rects <- data.frame(xstart = (which(df.new['sig_RC'] == 1)-1), xend = (which(df.new['sig_RC'] == 1)))
#   rects
#   
#   # Criando o gráfico
#   RC_lin_plot <- ggplot(RC_lin)  + 
#     {if(nrow(rects)>0) geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = 'lightblue', alpha = 0.4, show.legend = F)} +
#     geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
#     geom_line(aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
#     scale_x_continuous(name = "",breaks=seq(0,18,1),) +
#     scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
#     labs(title = paste0('Repasse cambial - ',
#                         stringr::str_to_upper(specs$inflation_index))) +
#     ylab('') +
#     theme_classic() +
#     theme(  panel.grid = element_blank(),
#             panel.border = element_blank(),
#             legend.position="right",
#             legend.title = element_text(hjust = 0.5),
#             legend.text = element_text(size=10),
#             legend.key = element_rect(colour = "black"),
#             legend.box.background = element_rect(colour = "black", size = 1),
#             plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
#             plot.title = ggtext::element_markdown(size = 10, colour = 'black', hjust = 0.5),
#             axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
#             axis.text.y = element_text(size=11,colour = 'black'))
#   
#   # Criando os dois gráficos
#   ggdraw() +
#     draw_plot(RC_lin_plot, x = 0, y = 0, height = 1, width = 1)
#   
#   ggsave(paste0('RC_linear_',
#                 stringr::str_to_upper(specs$inflation_index),'_v1.png'),
#          path = file.path('Output/Figures/', specs$nome_modelo),
#          device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
#   
#   # Fim da função
# }
# 
# 
# # Function to export figures
# 
# export_figures <- function(results_nl,results_lin,specs) {
#   # Creating output directories for each different model run ####
#   ifelse(!dir.exists(file.path('Output/Figures')),
#          dir.create(file.path('Output/Figures')),
#          FALSE)
#   
#   ifelse(!dir.exists(file.path('Output/Figures', specs$nome_modelo)),
#          dir.create(file.path('Output/Figures', specs$nome_modelo)),
#          FALSE)
# 
#   # # Getting the regimes names
#   transition_function_results <- plot_transition_function(results_nl,specs)
# 
#   # Conditional to select plot function (4 or 5 variables)
#   if (specs$n_endo_variables == 5) {
#     plot_nl_results_5_variables(results_nl,specs,transition_function_results)
#     plot_lin_results(results_lin,specs)
#     plot_coef_stderr(results_nl,specs,transition_function_results)
#   } else if (specs$n_endo_variables == 4) {
#     plot_nl_results_4_variables(results_nl,specs,transition_function_results) 
#     plot_lin_results(results_lin,specs)
#     plot_coef_stderr(results_nl,specs,transition_function_results)
#   } else {
#     print('Model with only 3 variables not available!')
#   }
# }
# 
# ggplot(IRF_df)  + 
#   geom_ribbon(aes(x=0:18, ymin=`IRF_lower s1`, ymax =`IRF_upper s1`),
#               fill = "indianred", alpha = 0.3) +
#   geom_line(aes(x=horizon, y=`IRF_lower s2`),color = "GREY50", alpha =  1, show.legend = F) +
#   geom_line(aes(x=horizon, y=`IRF_upper s2`),color = "GREY50", alpha =  1, show.legend = F) +
#   # geom_point(data = IRF_df %>% %>%
#   #              filter(if_all(-horizon, ~ .x > 0) |
#   #                       if_all(-horizon, ~ .x < 0)) %>%
#   #              slice(-1),
#   #            aes(x=horizon, y = IRF),  colour = 'cadetblue', size = 3,
#   #            shape = 24, fill = 'cadetblue', stroke = 2) +
#   geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
#   geom_line(aes(x=horizon, y=`IRF s1`), color = 'indianred',linetype = 'dashed',size = 1.5) +
#   geom_line(aes(x=horizon, y=`IRF s2`), color = 'GREY10', linetype = 'solid' ) +
#   scale_x_continuous(name = "",breaks=seq(0,18,1), 
#   ) +
#   labs(title = '') +
#   guides()  +
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
#           axis.text.x = element_text(angle = 45,
#                                      vjust = 0.6, 
#                                      hjust = 0.6,
#                                      size=11,
#                                      colour = 'black'),
#           axis.text.y = element_text(size=11,colour = 'black'))
# 
# paste0('Resposta de ',
#        stringr::str_to_upper(specs$inflation_index),
#        ' ao choque de ',
#        specs$endo_variables[i],
#        ' - ',
#        regime_1)
# 
# ggsave(paste0('Resposta de ',
#               stringr::str_to_upper(specs$inflation_index),
#               ' ao choque de ',
#               specs$endo_variables[i],
#               ' - ',
#               regime_1,'.png'),
#        path = file.path('Output/Figures/'),
#        device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
# # }