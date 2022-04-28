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
    regime_1 = 'depreciacao'
    regime_2 = 'apreciacao' 
  } else {
    regime_1 = 'apreciacao'
    regime_2 = 'depreciacao' 
  }
 
  
  date <- date[(max(c(lag_endog,lag_exog))):(length(date)-1)]
  length(date)

  # Criando dataframe dos dados
  # tryCatch é usado para evitar uma mensagem chata de "New names"
  tryCatch(expr = {df_state <- suppressMessages(
    bind_cols(results_nl$fz,
              date,
    .name_repair = ~ c('transition_function',
                                 'date')
    ))
  },
  error = function(error_in_function){
             message("Error in tibble!")
             print(error_in_function)
             }
  )
  df_state
  
  # Plotando o gráfico da função de transição
  state_plot <- ggplot(df_state)  + 
    geom_line(aes(x=date, y=transition_function),
              size = 0.75, color = 'darkred') +
    scale_x_continuous(breaks=seq(2000,2020,1),                       
                       labels=paste0(c(""),
                                     c(rep(2000:2019,each=1),2020)),
                       expand = c(0.01, 0.01)) +
    scale_y_continuous(name = "",
                       breaks = scales::pretty_breaks(n = 6),
                       expand = c(0.01, 0.01)) +
    labs(title = 'Transition function for the state of low level exchange rate') +
    scale_fill_brewer(palette="Blues") +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_blank(),
            plot.title = ggtext::element_markdown(hjust = 0.5,
                                                  size = 12, 
                                                  colour = 'black',
                                                  face ='bold'),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.5,
                                       hjust = 0.6,
                                       size=12, 
                                       colour = 'black',
                                       face = 'bold'),
            axis.text.y = element_text(size=12,face = 'bold',
                                       colour = 'black')) 
  state_plot
  date

  df <- raw_data %>%
    select(cambio,date) %>%
    slice(4:(n()-2))
  
  cambio_plot <- ggplot(df)  + 
    geom_line(aes(x=date, y=cambio),
              size = 0.75, color = 'darkred') +
    scale_x_continuous(breaks=seq(2000,2020.23,1),                       
                       labels=paste0(c(""),
                                     c(rep(2000:2019,each=1),2020)),
                       expand = c(0.02, 0.001)) +
    scale_y_continuous(name = "",
                       breaks = scales::pretty_breaks(n = 6),
                       expand = c(0.002, 0.001)) +
    labs(title = 'Nominal exchange rate') +
    scale_fill_brewer(palette="Blues") +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      size = 12,
                                      colour = 'black',
                                      face ='bold'),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.5,
                                       hjust = 0.6,
                                       size=12, 
                                       colour = 'black',
                                       face = 'bold'),
            axis.text.y = element_text(size=12,
                                       face = 'bold',
                                       colour = 'black')) 
  
  plot_grid(cambio_plot,
            NULL,
            state_plot,
            align = 'v',
            axis = 'r',
            ncol = 1,
            nrow = 3,
            rel_heights =  c(1,0,1)) +
    theme_minimal() +
    theme(  panel.grid = element_blank(),
            plot.margin = unit(c(0,0,0,0), "cm"),
            panel.border = element_blank())
  
  
  ggsave(paste0('State_',regime_2,'.png'),
         path = file.path('Output/Figures/'),
         device = "png",width = 12, height = 8, units = "cm",scale = 2) 
}


# Funções de Impulso Resposta com 5 variáveis endógenas (5 gráficos de IRFs) ####
plot_IRFs <- function(results_lin,results_nl,specs) {
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
  
  shock_names <- c('Commodities','Exchange rate','Aggregate demand','Interest rate','Inflation rate')
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst_nl_s1 <- vector("list", length = specs$n_endo_variables)
  plot_lst_nl_s2 <- vector("list", length = specs$n_endo_variables)
  
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
    plot_lst_nl_s1[[i]] <- ggplot(IRF_df %>% select(matches('*s1$'),horizon))  + 
      geom_ribbon(aes(x=0:18, ymin=`IRF_lower s1`, ymax =`IRF_upper s1`),
                  fill = "indianred", alpha = 0.3) +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=horizon, y=`IRF_lower s1`),
                color = "indianred", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF_upper s1`),
                color = "indianred", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF s1`), color = 'indianred',linetype = 'dashed',size = 0.75) +
      geom_point(data = IRF_df %>%
                   select(matches('* s1$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s1`),  size = 2,
                 shape = 24, fill = 'indianred') +
      scale_x_continuous(name = "",breaks=seq(0,18,2)) +
      scale_y_continuous(name = "",
                         breaks = scales::pretty_breaks(n = 8),
                         expand = c(0, 0)) +
      
      labs(title = paste0(shock_names[i],' shock')) +
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
              plot.title = ggtext::element_markdown(size = 10, 
                                                    colour = 'black',
                                                    face ='bold'),
              axis.text.x = element_text(angle = 45,
                                         vjust = 0.6, 
                                         hjust = 0.6,
                                         size=11,
                                         colour = 'black'),
              axis.text.y = element_text(size=11,colour = 'black'))
    
    IRF_df
    IRF_df %>% select(matches('*s2$'),horizon)

    plot_lst_nl_s2[[i]] <- ggplot(IRF_df %>% select(matches('*s2$'),horizon))  + 
      geom_ribbon(aes(x=0:18, ymin=`IRF_lower s2`, ymax =`IRF_upper s2`),
                  fill = "lightblue", alpha = 0.3) +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=horizon, y=`IRF_lower s2`),
                color = "lightblue", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF_upper s2`),
                color = "lightblue", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF s2`), color = 'dodgerblue',linetype = 'solid',size = 0.75) +
      geom_point(data = IRF_df %>%
                   select(matches('* s2$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s2`),  size = 2,
                 shape = 24, fill = 'dodgerblue') +
      scale_x_continuous(name = "",breaks=seq(0,18,2)) +
      scale_y_continuous(name = "",
                         breaks = scales::pretty_breaks(n = 8),
                         expand = c(0, 0)) +
      labs(title = paste0(shock_names[i],' shock')) +
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
              plot.title = ggtext::element_markdown(size = 10, 
                                                    colour = 'black',
                                                    face ='bold'),
              axis.text.x = element_text(angle = 45,
                                         vjust = 0.6, 
                                         hjust = 0.6,
                                         size=11,
                                         colour = 'black'),
              axis.text.y = element_text(size=11,colour = 'black'))
    
    
  }
  
  
  # IRF_df2 <- IRF_df[,1:3] %>%
  #   pivot_longer(cols = c(everything(),-horizon))
  # 
  # legend_plot_1 <- ggplot(IRF_df2) +
  #   geom_ribbon(aes(x=horizon, ymin=value, ymax =value, fill = 'indianred'), alpha = 0.3, show.legend = T) +
  #   geom_ribbon(aes(x=horizon, ymin=value, ymax =value, fill = 'lightblue'), alpha = 0.3, show.legend = T) +
  #   geom_line(aes(x=horizon, y=value, color = name, linetype = name),size = 0.5, show.legend = T) +
  #   scale_fill_manual(name = 'State', labels = c('High level',
  #                                                'Low level'),
  #                       values = c('indianred', 'lightblue'),guide = 'legend') +
  #   scale_color_manual(name = 'State', values = c('indianred','dodgerblue'),
  #                      labels = c('High level','Low level')) +
  #   scale_linetype_manual(name = 'State', values = c('dashed','solid'),
  #                      labels = c('High level','Low level')) +
  #   guides()  +
  #   ylab('') +
  #   xlab('') +
  #   theme_classic() +
  #   theme(  panel.grid = element_blank(), 
  #           panel.border = element_blank(),
  #           legend.position="right",
  #           legend.title = ggtext::element_markdown(size = 12, 
  #                                                   colour = 'black',
  #                                                   face ='bold'),
  #           legend.text = ggtext::element_markdown(size = 11, 
  #                                                  colour = 'black'),
  #           legend.key =  element_rect(colour = "black"),
  #           legend.box.background = element_rect(colour = "black", size = 1))
  # 
  # legend_plot_2 <- ggplot(IRF_df2) +
  #   geom_point(data = IRF_df2 %>%
  #                filter(name == 'IRF s1'),
  #              aes(x=horizon, y = value,  fill = name,
  #                  shape = 'filled triangle point-up blue'),  
  #              size = 2) +
  #   geom_point(data = IRF_df2 %>%
  #                filter(name == 'IRF_upper s1'),
  #              aes(x=horizon, y = value, shape = name, fill = name),  
  #              size = 2) +
  #   scale_shape_manual(name = 'Statiscal significance at 5%', values = c(24,24),
  #                      labels = c('High level',
  #                                 'Low level')) +
  #   scale_fill_manual(name = 'Statiscal significance at 5%', values = c('indianred','GREY50'),
  #                      labels = c('High level',
  #                                 'Low level')) +
  #   guides()  +
  #   ylab('') +
  #   xlab('') +
  #   theme_classic() +
  #   theme(  panel.grid = element_blank(), 
  #           panel.border = element_blank(),
  #           legend.position="right",
  #           legend.title = ggtext::element_markdown(size = 12, 
  #                                                   colour = 'black',
  #                                                   face ='bold'),
  #           legend.text = ggtext::element_markdown(size = 11, 
  #                                                  colour = 'black'),
  #           legend.key =  element_rect(colour = "black"),
  #           legend.box.background = element_rect(colour = "black", size = 1))
  #   
  # legend_plot_2
  # legend_plot_1 <- get_legend(legend_plot_1)  
  # legend_plot_2 <- get_legend(legend_plot_2)  
  # 
  # 
  # library(cowplot)
  # legend_plot
  # 
  # plot_grid(plotlist = plot_lst_nl,
  #           align = 'v',
  #           axis = 'r',
  #           ncol = 3,
  #           nrow = 2) +
  #   draw_grob(legend_plot_1, 2/3, 0.15, 1/3, 0.5) +
  #   draw_grob(legend_plot_2, 2/3, -0.05, 1/3, 0.5)
  #   
  #   
  # ggsave(paste0('IRFs_nonlinear.png'),
  #        path = file.path('Output/Figures/'),
  #        device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #                     Funções Impulso Resposta - Linear               #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst_lin <- vector("list", length = specs$n_endo_variables)

  for (i in 1:specs$n_endo_variables) {
    tryCatch(expr = {
      IRF_lin <- bind_cols(0:18,
                           results_lin$irf_lin_mean[specs$response,,i],
                           results_lin$irf_lin_up[specs$response,,i],
                           results_lin$irf_lin_low[specs$response,,i],
                                        .name_repair = ~ c(
                                          'horizon',
                                          'IRF','IRF_upper','IRF_lower'))
    },
    error = function(error_in_function){
      message("Error in tibble 8!")
      print(error_in_function)
    }
    )



    plot_lst_lin[[i]] <- ggplot(IRF_lin)  +
      geom_ribbon(aes(x=0:18, ymin=`IRF_lower`, ymax =`IRF_upper`),
                  fill = "GREY70", alpha = 0.3) +
      geom_line(aes(x=horizon, y=`IRF_lower`),
                color = "GREY80", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF_upper`),
                color = "GREY80", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_point(data = IRF_lin %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF`),  size = 2,
                 shape = 24, fill = 'black') +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'black',
                linetype = 'dotted',size = 1) +
      scale_x_continuous(name = "",breaks=seq(0,18,2),
      ) +
      labs(title = paste0(shock_names[i],' shock')) +
      ylab('') +
      theme_classic() +
      theme_classic() +
      theme(  panel.grid = element_blank(), 
              panel.border = element_blank(),
              legend.position="right",
              legend.title = element_text(hjust = 0.5),
              legend.text = element_text(size=10),
              legend.key = element_rect(colour = "black"),
              legend.box.background = element_rect(colour = "black", size = 1),
              plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
              plot.title = ggtext::element_markdown(size = 10, 
                                                    colour = 'black',
                                                    face ='bold'),
              axis.text.x = element_text(angle = 45,
                                         vjust = 0.6, 
                                         hjust = 0.6,
                                         size=11,
                                         colour = 'black'),
              axis.text.y = element_text(size=11,colour = 'black'))
    

  }
  
  # Workaround to get legends
  IRF_df2 <- IRF_df[,1:4] %>%
    pivot_longer(cols = c(everything(),-horizon))
  
  legend_plot_1 <- ggplot(IRF_df2) +
    geom_ribbon(aes(x=horizon, ymin=value, ymax =value, fill = name), alpha = 0.3, show.legend = T) +
    geom_line(aes(x=horizon, y=value, color = name, linetype = name),size = 0.5, show.legend = T) +
    scale_fill_manual(name = 'State', labels = c('High level',
                                                 'Low level',
                                                 'Linear model'),
                      values = c('indianred', 
                                 'lightblue',
                                 'GREY70'),guide = 'legend') +
    scale_color_manual(name = 'State', values = c('indianred',
                                                  'dodgerblue',
                                                  'black'),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_linetype_manual(name = 'State', values = c('dashed',
                                                     'solid',
                                                     'dotted'),
                          labels = c('High level',
                                     'Low level',
                                     'Linear model')) +
    guides()  +
    theme(  legend.position="right",
            legend.title = ggtext::element_markdown(size = 12, 
                                                    colour = 'black',
                                                    face ='bold'),
            legend.text = ggtext::element_markdown(size = 11, 
                                                   colour = 'black'),
            legend.key =  element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1))
  
  legend_plot_2 <- ggplot(IRF_df2) +
    geom_point(aes(x=horizon, y = value, shape = name, fill = name),  
               size = 2) +
    scale_shape_manual(name = 'Statiscal significance at 5%', values = c(24,24,24),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_fill_manual(name = 'Statiscal significance at 5%', values = c('indianred',
                                                                        'dodgerblue',
                                                                        'black'),
                      labels = c('High level',
                                 'Low level',
                                 'Linear model')) +
    guides()  +
    theme(  legend.position="right",
            legend.title = ggtext::element_markdown(size = 12, 
                                                    colour = 'black',
                                                    face ='bold'),
            legend.text = ggtext::element_markdown(size = 11, 
                                                   colour = 'black'),
            legend.key =  element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1))
  
  legend_plot_2
  legend_plot_1 <- get_legend(legend_plot_1)  
  legend_plot_2 <- get_legend(legend_plot_2)  
  
  plot_lst_nl_s1[[2]]$labels$title <- NULL
  plot_lst_nl_s2[[2]]$labels$title <- NULL
  plot_lst_lin[[2]]$labels$title <- NULL
  plot_grid(plot_lst_nl_s1[[2]],
            plot_lst_nl_s2[[2]],
            plot_lst_lin[[2]],
            align = 'v',
            axis = 'r',
            ncol = 3,
            nrow = 2,
            rel_heights =  c(1,0.3)) +
    draw_grob(legend_plot_1, 1/10, -0.1, 1/3, 0.5) +
    draw_grob(legend_plot_2, 6/10, -0.1, 1/3, 0.5) +
    theme_minimal() +
    theme(  panel.grid = element_blank(),
            panel.border = element_blank())



  ggsave(paste0('IRFs_model_comparison.png'),
         path = file.path('Output/Figures/'),
         device = "png",width = 12, height = 8, units = "cm",scale = 2,
         bg = 'transparent')
 }

plot_Belaisch_ERPT <- function(results_lin,results_nl,specs) {

  #---------------------------------------------------------------------#
  #                                                                     #
  #             Repasse cambial conforme Belaisch (2003)                #
  #                                                                     #
  #---------------------------------------------------------------------#

  tryCatch(expr = {
    RC_df <- suppressMessages(
      bind_cols(0:18,
                cumsum(results_nl$irf_s1_mean[specs$response,,specs$cambio_shock])
                /cumsum(results_nl$irf_s1_mean[specs$cambio_shock,,specs$cambio_shock]),
                results_nl$irf_s1_up[specs$response,,specs$cambio_shock],
                results_nl$irf_s1_low[specs$response,,specs$cambio_shock],
                
                cumsum(results_nl$irf_s2_mean[specs$response,,specs$cambio_shock])
                /cumsum(results_nl$irf_s2_mean[specs$cambio_shock,,specs$cambio_shock]),
                results_nl$irf_s2_up[specs$response,,specs$cambio_shock],
                results_nl$irf_s2_low[specs$response,,specs$cambio_shock],
                
                cumsum(results_lin$irf_lin_mean[specs$response,,specs$cambio_shock])
                /cumsum(results_lin$irf_lin_mean[specs$cambio_shock,,specs$cambio_shock]),
                results_lin$irf_lin_up[specs$response,,specs$cambio_shock],
                results_lin$irf_lin_low[specs$response,,specs$cambio_shock],
             .name_repair = ~ c('horizon',
                                'RC_s1',
                                'RC_s1 upper',
                                'RC_s1 lower',
                                'RC_s2',
                                'RC_s2 upper',
                                'RC_s2 lower',
                                'RC_lin',
                                'RC_lin upper',
                                'RC_lin lower')))

  },
  error = function(error_in_function){
    message("Error in tibble 2!")
    print(error_in_function)
  }
  )
  RC_df %>% select(horizon,RC_lin, RC_s2,RC_s1) %>%
    pivot_longer(cols = c(everything(),-horizon)) 

  RC_plot <- ggplot(RC_df)  + 
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    
    # Linhas
    geom_line(data = RC_df %>% select(horizon,RC_lin, RC_s2,RC_s1) %>%
                pivot_longer(cols = c(everything(),-horizon)),
      aes(x=horizon, y = value, color = name, linetype = name), size = 0.75) +
    
    # Pontos de significancia
    geom_point(data = RC_df %>% 
                 select(horizon, matches('lin')) %>%
                 filter(if_all(!c(horizon,RC_lin), ~ .x > 0) | if_all(!c(horizon,RC_lin), ~ .x < 0)) %>%
                 slice(-1),
               aes(x=horizon, y = RC_lin, shape = 'diamond', fill = 'red'),
               size = 3) +
    geom_point(data = RC_df %>% 
                 select(horizon, matches('s1')) %>%
                 filter(if_all(!c(horizon,RC_s1), ~ .x > 0) | if_all(!c(horizon,RC_s1), ~ .x < 0)) %>%
                 slice(-1),
               aes(x=horizon, y = RC_s1, shape = 'cross', fill = 'blue'),  
               size = 3) +
    geom_point(data = RC_df %>% 
                 select(horizon, matches('s2')) %>%
                 filter(if_all(!c(horizon,RC_s2), ~ .x > 0) | if_all(!c(horizon,RC_s2), ~ .x < 0)) %>%
                 slice(-1),
               aes(x=horizon, y = RC_s2, shape = 'triangle', fill = 'yellow'),
               size = 3) +
    
    
    # Escala
    scale_x_continuous(name = "",breaks=seq(0,18,2),) +
    scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),
                       labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
    
    # Legendas
    scale_color_manual(name = 'State', values = c('black',
                                                  'indianred',
                                                  'dodgerblue'),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_linetype_manual(name = 'State', values = c('dotted',
                                                     'dashed',
                                                     'solid'),
                          labels = c('High level',
                                     'Low level',
                                     'Linear model')) +
    scale_shape_manual(name = 'Statiscal significance at 5%', 
                       values = c(24,24,24),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_fill_manual(name = 'Statiscal significance at 5%', values = c('indianred',
                                                                        'black',
                                                                        'dodgerblue'),
                      labels = c('High level',
                                 'Low level',
                                 'Linear model')) +
    
    # Outras configurações
    labs(title = '') +
    ylab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position="none",
            legend.title = element_text(hjust = 0.5),
            legend.text = element_text(size=10),
            legend.key = element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1),
            plot.margin=grid::unit(c(0,-2,0,-5), "mm"),
            plot.title = ggtext::element_markdown(size = 10, 
                                                  colour = 'black',
                                                  hjust = 0.5),
            axis.text.x = element_text(angle = 45,
                                       vjust = 0.6,
                                       hjust = 0.6,
                                       size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
 
  RC_plot
  # Workaround to get legends
  IRF_df2 <- RC_df[,1:4] %>%
    pivot_longer(cols = c(everything(),-horizon))
  
  legend_plot_1 <- ggplot(IRF_df2) +
    geom_line(aes(x=horizon, y=value, color = name, linetype = name),size = 0.5, show.legend = T) +
    scale_color_manual(name = 'State', values = c('indianred',
                                                  'dodgerblue',
                                                  'black'),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_linetype_manual(name = 'State', values = c('dashed',
                                                     'solid',
                                                     'dotted'),
                          labels = c('High level',
                                     'Low level',
                                     'Linear model')) +
    guides()  +
    theme(  legend.position="right",
            legend.title = ggtext::element_markdown(size = 12, 
                                                    colour = 'black',
                                                    face ='bold'),
            legend.text = ggtext::element_markdown(size = 11, 
                                                   colour = 'black'),
            legend.key =  element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1))
  
  legend_plot_2 <- ggplot(IRF_df2) +
    geom_point(aes(x=horizon, y = value, shape = name, fill = name),  
               size = 2) +
    scale_shape_manual(name = 'Statiscal significance at 5%', values = c(24,24,24),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_fill_manual(name = 'Statiscal significance at 5%', values = c('indianred',
                                                                        'dodgerblue',
                                                                        'black'),
                      labels = c('High level',
                                 'Low level',
                                 'Linear model')) +
    guides()  +
    theme(  legend.position="right",
            legend.title = ggtext::element_markdown(size = 12, 
                                                    colour = 'black',
                                                    face ='bold'),
            legend.text = ggtext::element_markdown(size = 11, 
                                                   colour = 'black'),
            legend.key =  element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1))
  
  legend_plot_2
  legend_plot_1 <- get_legend(legend_plot_1)  
  legend_plot_2 <- get_legend(legend_plot_2)  

  plot_grid(RC_plot,
            align = 'v',
            axis = 'r',
            ncol = 1,
            nrow = 2,
            rel_heights =  c(1,0.3)) +
    draw_grob(legend_plot_1, 1/10, -0.12, 1/3, 0.5) +
    draw_grob(legend_plot_2, 6/10, -0.12, 1/3, 0.5) +
    theme_minimal() +
    theme(  panel.grid = element_blank(),
            panel.border = element_blank())
  
  ggsave(paste0('ERPT_Belaisch.png'),
         path = file.path('Output/Figures/'),
         device = "png",width = 12, height = 8, units = "cm",scale = 2)
  
}
