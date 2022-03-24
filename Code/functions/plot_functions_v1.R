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
plot_IRFs <- function(results_lin,results_nl,specs) {
  # Elabora os 5 gráficos das IRF's 
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #                     Funções Impulso Resposta                        #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  specs <- model_specs
  
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
  plot_lst_nl <- vector("list", length = specs$n_endo_variables)
  
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
    
    

    plot_lst_nl[[i]] <- ggplot(IRF_df)  + 
      geom_ribbon(aes(x=0:18, ymin=`IRF_lower s1`, ymax =`IRF_upper s1`),
                  fill = "indianred", alpha = 0.3) +
      geom_line(aes(x=horizon, y=`IRF_lower s2`),color = "GREY50", alpha =  1, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF_upper s2`),color = "GREY50", alpha =  1, show.legend = F, size = 0.75) +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=horizon, y=`IRF s1`), color = 'indianred',linetype = 'dashed',size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF s2`), color = 'GREY10', linetype = 'solid' , size = 0.75) +
      geom_point(data = IRF_df %>%
                   select(matches('* s1$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s1`),  size = 2,
                 shape = 24, fill = 'indianred') +
      geom_point(data = IRF_df %>%
                   select(matches('* s2$*') | matches('horizon')) %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF s2`),  size = 2,
                 shape = 24, fill = 'GREY50') +
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
    
  IRF_df2 <- IRF_df[,1:3] %>%
    pivot_longer(cols = c(everything(),-horizon))

  legend_plot_1 <- ggplot(IRF_df2) +
    geom_ribbon(aes(x=horizon, ymin=value, ymax =value, fill = 'indianred'), alpha = 0.3, show.legend = T) +
    geom_ribbon(aes(x=horizon, ymin=value, ymax =value, fill = 'white'), alpha = 0.3, show.legend = T) +
    geom_line(aes(x=horizon, y=value, color = name, linetype = name),size = 0.5, show.legend = T) +
    scale_fill_manual(name = 'State', labels = c('High level',
                                                 'Low level'),
                        values = c('indianred', 'white'),guide = 'legend') +
    scale_color_manual(name = 'State', values = c('indianred','GREY10'),
                       labels = c('High level','Low level')) +
    scale_linetype_manual(name = 'State', values = c('dashed','solid'),
                       labels = c('High level','Low level')) +
    guides()  +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
            legend.title = ggtext::element_markdown(size = 12, 
                                                    colour = 'black',
                                                    face ='bold'),
            legend.text = ggtext::element_markdown(size = 11, 
                                                   colour = 'black'),
            legend.key =  element_rect(colour = "black"),
            legend.box.background = element_rect(colour = "black", size = 1))
  
  legend_plot_2 <- ggplot(IRF_df2) +
    geom_point(data = IRF_df2 %>%
                 filter(name == 'IRF s1'),
               aes(x=horizon, y = value,  fill = name,
                   shape = 'filled triangle point-up blue'),  
               size = 2) +
    geom_point(data = IRF_df2 %>%
                 filter(name == 'IRF_upper s1'),
               aes(x=horizon, y = value, shape = name, fill = name),  
               size = 2) +
    scale_shape_manual(name = 'Statiscal significance at 5%', values = c(24,24),
                       labels = c('High level',
                                  'Low level')) +
    scale_fill_manual(name = 'Statiscal significance at 5%', values = c('indianred','GREY50'),
                       labels = c('High level',
                                  'Low level')) +
    guides()  +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(  panel.grid = element_blank(), 
            panel.border = element_blank(),
            legend.position="right",
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
  

  library(cowplot)
  legend_plot
  
  plot_grid(plotlist = plot_lst_nl,
            align = 'v',
            axis = 'r',
            ncol = 3,
            nrow = 2) +
    draw_grob(legend_plot_1, 2/3, 0.15, 1/3, 0.5) +
    draw_grob(legend_plot_2, 2/3, -0.05, 1/3, 0.5)
    
    
  ggsave(paste0('IRFs_nonlinear.png'),
         path = file.path('Output/Figures/'),
         device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
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
                  fill = "cadetblue", alpha = 0.3) +
      geom_line(aes(x=horizon, y=`IRF_lower`),
                color = "cadetblue", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_line(aes(x=horizon, y=`IRF_upper`),
                color = "cadetblue", alpha =  0.6, show.legend = F, size = 0.75) +
      geom_point(data = IRF_lin %>%
                   filter(if_all(-horizon, ~ .x > 0) |
                            if_all(-horizon, ~ .x < 0)) %>%
                   slice(-1),
                 aes(x=horizon, y = `IRF`),  size = 2,
                 shape = 24, fill = 'cadetblue') +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'cadetblue',
                linetype = 'dotdash',size = 1.25) +
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
                                 'white',
                                 'cadetblue'),guide = 'legend') +
    scale_color_manual(name = 'State', values = c('indianred',
                                                  'GREY10',
                                                  'cadetblue'),
                       labels = c('High level',
                                  'Low level',
                                  'Linear model')) +
    scale_linetype_manual(name = 'State', values = c('dashed',
                                                     'solid',
                                                     'dotdash'),
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
                                                                        'GREY50',
                                                                        'cadetblue'),
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
  
  plot_lst_nl[[2]]$labels$title <- NULL
  plot_lst_lin[[2]]$labels$title <- NULL
  plot_grid(plot_lst_nl[[2]],
            plot_lst_lin[[2]],
            align = 'v',
            axis = 'r',
            ncol = 2,
            nrow = 2,
            rel_heights =  c(1,0.3)) +
    draw_grob(legend_plot_1, 1/10, -0.1, 1/3, 0.5) +
    draw_grob(legend_plot_2, 6/10, -0.1, 1/3, 0.5)



    ggsave(paste0('IRFs_model_comparison.png'),
           path = file.path('Output/Figures/'),
           device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
 }

plot_Belaisch_ERPT <- function(results_lin,results_nl,specs) {


  #---------------------------------------------------------------------#
  #                                                                     #
  #             Repasse cambial conforme Belaisch (2003)                #
  #                                                                     #
  #---------------------------------------------------------------------#

  tryCatch(expr = {
    RC_nl <- suppressMessages(
      bind_cols(cumsum(results_nl$irf_s1_mean[specs$response,,specs$cambio_shock])
                /cumsum(results_nl$irf_s1_mean[specs$cambio_shock,,specs$cambio_shock]),
                       results_nl$irf_s1_up[specs$response,,specs$cambio_shock],
                       results_nl$irf_s1_low[specs$response,,specs$cambio_shock],
                cumsum(results_nl$irf_s2_mean[specs$response,,specs$cambio_shock])
                /cumsum(results_nl$irf_s2_mean[specs$cambio_shock,,specs$cambio_shock]),
                results_nl$irf_s2_up[specs$response,,specs$cambio_shock],
                results_nl$irf_s2_low[specs$response,,specs$cambio_shock],
             .name_repair = ~ c('RC_r1',
                                'RC_r1_upper',
                                'RC_r1_lower',
                                'RC_r2',
                                'RC_r2_upper',
                                'RC_r2_lower')))

  },
  error = function(error_in_function){
    message("Error in tibble 2!")
    print(error_in_function)
  }
  )
  RC_nl %>% select(matches('*RC_r[0-9]$'))
  
  RC_nl_plot <- ggplot(RC_nl)  + 
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(data = RC_nl %>% select(matches('*RC_r[0-9]$')),
      aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,2),) +
    scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),
                       labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
    labs(title = paste0('Repasse cambial - ',regime_1)) +
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
                                                  hjust = 0.5),
            axis.text.x = element_text(angle = 45,
                                       vjust = 0.6,
                                       hjust = 0.6,
                                       size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
  # Criando tibble com os dados de CI e da LP (Regime 2)
  tryCatch(expr = {
    RC_r2 <- suppressMessages(tibble(bind_cols(cumsum((results_nl$irf_s2_mean[specs$response,,specs$cambio_shock])
                                                      /cumsum(results_nl$irf_s2_mean[specs$cambio_shock,,specs$cambio_shock])),
                                               results_nl$irf_s2_mean[specs$response,,specs$cambio_shock],
                                               results_nl$irf_s2_up[specs$response,,specs$cambio_shock],
                                               results_nl$irf_s2_low[specs$response,,specs$cambio_shock],
                                               results_nl$irf_s2_mean[specs$cambio_shock,,specs$cambio_shock],
                                               results_nl$irf_s2_up[specs$cambio_shock,,specs$cambio_shock],
                                               results_nl$irf_s2_low[specs$cambio_shock,,specs$cambio_shock]),
                                     .name_repair = ~ c('RC',
                                                        'Inflation_Index_mean',
                                                        'Inflation_Index_upper',
                                                        'Inflation_Index_lower',
                                                        'cambio_mean',
                                                        'cambio_upper',
                                                        'cambio_lower')))
  },
  error = function(error_in_function){
    message("Error in tibble 3!")
    print(error_in_function)
  }
  )
  
  
  # Criando tibble que define os momentos de significância estatística de ambos as LP's
  # df.new <- RC_r2 %>%
  #   mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)) &
  #                                 ((cambio_lower >0 ) & (cambio_upper > 0)  | (cambio_lower <0) & (cambio_upper < 0)),
  #                               1,
  #                               0))
  df.new <- RC_r2 %>%
    mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)) ,
                                1,
                                0))
  df.new$sig_RC[1] <- 0  # Primeiro período nunca tem IC
  df.new
  
  RC_r2
  # Criando o dataframe que estabelece os momentos a serem localizados pelo ggplot
  rects <- data.frame(xstart = (which(df.new['sig_RC'] == 1)-1), xend = (which(df.new['sig_RC'] == 1)))
  rects
  # Criando o gráfico
  RC_r2_plot <- ggplot(RC_r2)  + 
    {if(nrow(rects)>0) geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = 'lightblue', alpha = 0.4, show.legend = F)} +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1),) +
    scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
    labs(title = paste0('Repasse cambial - ',regime_2)) +
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
            plot.title = ggtext::element_markdown(size = 10, colour = 'black', hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
  # Criando os dois gráficos
  ggdraw() +
    draw_plot(RC_r1_plot, x = 0, y = 0.5, height = .5, width = 1) +
    draw_plot(RC_r2_plot, x = 0, y = 0, height = .5, width = 1)
  
  ggsave(paste0('RC_',
                stringr::str_to_upper(specs$inflation_index),'_v1.png'),
         path = file.path('Output/Figures/', specs$nome_modelo),
         device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
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
}