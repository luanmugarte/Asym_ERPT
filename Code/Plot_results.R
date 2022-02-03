# Elaboração dos gráficos para o modelo de LP com 4 variáveis endógenas (comm exógeno)

# Obtendo o objeto dos plots ####
nl_plots <- plot_nl(results_nl)

# Criando diretório para exportar gráficos ####
ifelse(!dir.exists(file.path('Output/Figures')),
       dir.create(file.path('Output/Figures')),
       FALSE)

ifelse(!dir.exists(file.path('Output/Figures', nome_modelo)),
       dir.create(file.path('Output/Figures', nome_modelo)),
       FALSE)
setwd(file.path('Output/Figures/', nome_modelo))
modelo_endo

#---------------------------------------------------------------------#
#                                                                     #
#                     Função de Transição                             #
#                                                                     #
#------------------------------------------------------------------####

# Elabora o gráfico da função de transição da estimação ####

# Regime 1 é a probabilidade de (1 - evento da variável de transição).
# Regime 2 é a probabilidade do evento da variável de transição. 

if (modelo == 'mensal') {
  transition_function <- as.xts(ts(results_nl$fz, start = c(2000,2), end = c(2020,2), frequency = 12))
} else {
  transition_function <- as.xts(ts(results_nl$fz, start = c(2000,2), end = c(2019,4), frequency = 4))
}

transition_function["2002:10"]

plot(transition_function)
date <- time(transition_function)
date
if (modelo == 'mensal') {
  if ( (transition_function['2002:8'] < 0.2) &&
       (transition_function['2002:9'] < 0.2) &&
       (transition_function['2002:10'] < 0.2)) {
    regime_1 = 'Regime de Depreciação'
    regime_2 = 'Regime de Apreciação' 
  } else {
    regime_1 = 'Regime de Apreciação'
    regime_2 = 'Regime de Depreciação' 
  }
} else {
  if ((transition_function['2002:7'] < 0.2) &&
      (transition_function['2002:10'] < 0.2)) {
    regime_1 = 'Regime de Depreciação'
    regime_2 = 'Regime de Apreciação' 
  } else {
    regime_1 = 'Regime de Apreciação'
    regime_2 = 'Regime de Depreciação' 
  }
}
modelo

if (modelo == "mensal") {
  date <- date[(max(c(lag_endog,lag_exog))):(length(date)-1)]
} else {
  date <- date[(max(c(lag_endog,lag_exog))):(length(date)-2)]
}

length(date)

length(results_nl$fz)
results_nl$fz

# Criando dataframe dos dados
# tryCatch é usado para evitar uma mensagem chata de "New names"
tryCatch(expr = {df <- suppressMessages(as_tibble(bind_cols(results_nl$fz,date), .name_repair = ~vctrs::vec_as_names(c('transition_function','date'), repair = "unique", quiet = TRUE)))},
         error = function(error_in_function){
           message("Error in tibble!")
           print(error_in_function)
         }
)
df

# Plotando o gráfico da função de transição
ggplot(df)  + 
  geom_line(aes(x=date, y=transition_function), size = 0.75, color = 'darkred') +
  scale_x_continuous(breaks=seq(2000,2020.23,0.5),                       
                     labels=paste0(c("Jan ",'Jun '),c(rep(2000:2019,each=2),2020)),expand = c(0, 0)) +
  labs(title = paste0('Função de Transição - ',regime_2, ' - gamma = ', as.character(gamma_transition))) +
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
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=12, colour = 'black',face = 'bold'),
          axis.text.y = element_text(size=12,face = 'bold')) 

ggsave(paste0('Funcao_Transicao_',stringr::str_to_upper(inflation_index),'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
plot(cambio)
length(seq(2000.33,2020.23,0.5))
length(paste(c("Jun",'Jan'),c(rep(2000:2019,each=2),2020)))

# Funções de Impulso Resposta com comm exógena (4 gráficos de IRFs) ####
plot_IRFs_4_variables <- function() {
  # Elabora os 4 gráficos das IRF's e do cálculo do repasse cambial conforme Belaisch (2003). 
  # É "necessário" duas funções diferentes pelo fato do número de gráficos serem diferentes.
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #                     Funções Impulso Resposta                        #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  # IRF - Regime 1
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst <- vector("list", length = length(modelo_endo))
  

  # For loop para fazer o gráfico da IRF para cada variável de resposta
  for (i in 1:length(modelo_endo)) {
    
    tryCatch(expr = {
      IRF_s1 <- suppressMessages(tibble(bind_cols(results_nl$irf_s1_mean[response,,i],
                                 results_nl$irf_s1_up[response,,i],
                                 results_nl$irf_s1_low[response,,i]),
                                 .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
        mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    },
             error = function(error_in_function){
               message("Error in tibble!")
               print(error_in_function)
             }
    )
    
    
    plot_lst[[i]] <- ggplot(IRF_s1)  + 
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'cadetblue', size = 0.75) +
      scale_x_continuous(name = "",breaks=seq(0,18,1), 
      ) +
      labs(title = paste0('Resposta de  ',stringr::str_to_upper(inflation_index),' ao choque de ',colnames(modelo_endo[,i]),' - ',regime_1)) +
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
    draw_plot(plot_lst[[1]], x = 0, y = 0.5, height = .5, width = .5) +
    draw_plot(plot_lst[[2]], x = 0.5, y = 0.5, height = .5, width = .5) +
    draw_plot(plot_lst[[3]], x = 0, y = 0, height = .5, width = .5) +
    draw_plot(plot_lst[[4]], x = 0.5, y = 0, height = .5, width = .5)
  
  
  # ggsave(paste0(nome_modelo,'_',regime_1),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  ggsave(paste0('IRF_',stringr::str_to_upper(inflation_index),'_',regime_1,'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  
  # IRF - Regime 2
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst <- vector("list", length = length(modelo_endo))
  
  
  IRF_s2 <- suppressMessages(tibble(bind_cols(results_nl$irf_s2_mean[response,,i],
                                              results_nl$irf_s2_up[response,,i],
                                              results_nl$irf_s2_low[response,,i]),
                                    .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
    mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
    mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
    dplyr::select(!c(IRF_upper_base,IRF_lower_base))

  # For loop para fazer o gráfico da IRF para cada variável de resposta
  for (i in 1:(length(modelo_endo))) {
    
    tryCatch(expr = {
      IRF_s2 <- suppressMessages(tibble(bind_cols(results_nl$irf_s2_mean[response,,i],
                                 results_nl$irf_s2_up[response,,i],
                                 results_nl$irf_s2_low[response,,i]),
                                 .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
        mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    },
    error = function(error_in_function){
      message("Error in tibble!")
      print(error_in_function)
    }
    )
  

    plot_lst[[i]] <- ggplot(IRF_s2)  + 
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'bisque3', size = 0.75) +
      scale_x_continuous(name = "",breaks=seq(0,18,1), 
      ) +
      labs(title = paste0('Resposta de ',stringr::str_to_upper(inflation_index), ' ao choque de ',colnames(modelo_endo[,i]),' - ',regime_2)) +
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
    draw_plot(plot_lst[[1]], x = 0, y = 0.5, height = .5, width = .5) +
    draw_plot(plot_lst[[2]], x = 0.5, y = 0.5, height = .5, width = .5) +
    draw_plot(plot_lst[[3]], x = 0, y = 0, height = .5, width = .5) +
    draw_plot(plot_lst[[4]], x = 0.5, y = 0, height = .5, width = .5)
  
  
  ggsave(paste0('IRF_',stringr::str_to_upper(inflation_index),'_',regime_2,'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #             Repasse cambial conforme Belaisch (2003)                #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  # Criando tibble com os dados de CI e da LP (Regime 1)
  tryCatch(expr = {
    RC_r1 <- suppressMessages(tibble(bind_cols(cumsum((results_nl$irf_s1_mean[response,,cambio_shock])
                                     /cumsum(results_nl$irf_s1_mean[cambio_shock,,cambio_shock])),
                              results_nl$irf_s1_mean[response,,cambio_shock],
                              results_nl$irf_s1_up[response,,cambio_shock],
                              results_nl$irf_s1_low[response,,cambio_shock],
                              results_nl$irf_s1_mean[cambio_shock,,cambio_shock],
                              results_nl$irf_s1_up[cambio_shock,,cambio_shock],
                              results_nl$irf_s1_low[cambio_shock,,cambio_shock]),
                    .name_repair = ~ c('RC',
                                       'Inflation_Index_mean',
                                       'Inflation_Index_upper',
                                       'Inflation_Index_lower',
                                       'cambio_mean',
                                       'cambio_upper',
                                       'cambio_lower')))
  },
  error = function(error_in_function){
    message("Error in tibble!")
    print(error_in_function)
  }
  )
  regime_1
  
  # Criando tibble que define os momentos de significância estatística de ambos as LP's
  # df.new <- RC_r1 %>%
  #   mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)) &
  #                                 ((cambio_lower >0 ) & (cambio_upper > 0)  | (cambio_lower <0) & (cambio_upper < 0)),
  #                               1,
  #                               0))
  # df.new
  
  df.new <- RC_r1 %>%
    mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)),
                                1,
                                0))
  df.new$sig_RC[1] <- 0  # Primeiro período nunca tem IC
  df.new
  # Criando o dataframe que estabelece os momentos a serem localizados pelo ggplot
  rects <- data.frame(xstart = (which(df.new['sig_RC'] == 1)-1), xend = (which(df.new['sig_RC'] == 1)))
  rects
  
  # Criando o gráfico
  RC_r1_plot <- ggplot(RC_r1)  + 
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = 'lightblue', alpha = 0.4, show.legend = F) +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1),) +
    scale_y_continuous(name = "",labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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
            plot.title = ggtext::element_markdown(size = 10, colour = 'black', hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
  # Criando tibble com os dados de CI e da LP (Regime 2)
  tryCatch(expr = {
    RC_r2 <- suppressMessages(tibble(bind_cols(cumsum((results_nl$irf_s2_mean[response,,cambio_shock])
                                     /cumsum(results_nl$irf_s2_mean[cambio_shock,,cambio_shock])),
                              results_nl$irf_s2_mean[response,,cambio_shock],
                              results_nl$irf_s2_up[response,,cambio_shock],
                              results_nl$irf_s2_low[response,,cambio_shock],
                              results_nl$irf_s2_mean[cambio_shock,,cambio_shock],
                              results_nl$irf_s2_up[cambio_shock,,cambio_shock],
                              results_nl$irf_s2_low[cambio_shock,,cambio_shock]
    ),
    .name_repair = ~ c('RC',
                       'Inflation_Index_mean',
                       'Inflation_Index_upper',
                       'Inflation_Index_lower',
                       'cambio_mean',
                       'cambio_upper',
                       'cambio_lower')))
  },
  error = function(error_in_function){
    message("Error in tibble!")
    print(error_in_function)
  }
  )
  
  RC_r2
  
  # Criando tibble que define os momentos de significância estatística de ambos as LP's
  # df.new <- RC_r2 %>%
  #   mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)) &
  #                                 ((cambio_lower >0 ) & (cambio_upper > 0)  | (cambio_lower <0) & (cambio_upper < 0)),
  #                               1,
  #                               0))
  df.new <- RC_r2 %>%
    mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)),
                                1,
                                0))
  df.new$sig_RC[1] <- 0  # Primeiro período nunca tem IC
  df.new
  
  # Criando o dataframe que estabelece os momentos a serem localizados pelo ggplot
  rects <- data.frame(xstart = (which(df.new['sig_RC'] == 1)-1), xend = (which(df.new['sig_RC'] == 1)))
  rects
  
  # Criando o gráfico
  RC_r2_plot <- ggplot(RC_r2)  + 
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = 'lightblue', alpha = 0.4, show.legend = F) +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1),) +
    scale_y_continuous(name = "",labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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
  
  ggsave(paste0('RC_',stringr::str_to_upper(inflation_index),'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  # Final da função
}

# Funções de Impulso Resposta com comm exógena (5 gráficos de IRFs) ####
plot_IRFs_5_variables <- function() {
  # Elabora os 5 gráficos das IRF's e do cálculo do repasse cambial conforme Belaisch (2003). 
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #                     Funções Impulso Resposta                        #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  # IRF - Regime 1
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst <- vector("list", length = length(modelo_endo))
  
  for (i in 1:length(modelo_endo)) {
    tryCatch(expr = {
      IRF_s1 <- suppressMessages(tibble(bind_cols(results_nl$irf_s1_mean[response,,i],
                                 results_nl$irf_s1_up[response,,i],
                                 results_nl$irf_s1_low[response,,i]),
                                 .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
        mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    },
    error = function(error_in_function){
      message("Error in tibble!")
      print(error_in_function)
    }
    )
    
    
    
    plot_lst[[i]] <- ggplot(IRF_s1)  + 
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'cadetblue', size = 0.75) +
      scale_x_continuous(name = "",breaks=seq(0,18,1), 
      ) +
      labs(title = paste0('Resposta de ',stringr::str_to_upper(inflation_index),' ao choque de ',colnames(modelo_endo[,i]),' - ',regime_1)) +
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
  
  
  
  ggsave(paste0('IRF_',stringr::str_to_upper(inflation_index),'_',regime_1,'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  
  # IRF - Regime 2
  
  # Criando um vetor que contem os objetos dos plots
  plot_lst <- vector("list", length = length(modelo_endo))
  
  for (i in 1:length(modelo_endo)) {
    tryCatch(expr = {
      IRF_s2 <- suppressMessages(tibble(bind_cols(results_nl$irf_s2_mean[response,,i],
                                 results_nl$irf_s2_up[response,,i],
                                 results_nl$irf_s2_low[response,,i]),
                                 .name_repair = ~ c('IRF','IRF_upper_base','IRF_lower_base'))) %>%
        mutate(IRF_upper = if_else(IRF_upper_base < IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        mutate(IRF_lower = if_else(IRF_upper_base > IRF_lower_base, IRF_lower_base,IRF_upper_base)) %>%
        dplyr::select(!c(IRF_upper_base,IRF_lower_base))
    },
    error = function(error_in_function){
      message("Error in tibble!")
      print(error_in_function)
    }
    )
    
    
    plot_lst[[i]] <- ggplot(IRF_s2)  + 
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF, ymax =IRF_upper), fill = "GREY90") +
      geom_ribbon(aes(x=c(0:hor_lps), ymin=IRF_lower, ymax =IRF), fill = "GREY90") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_upper), color = "GREY70") +
      geom_line(aes(x=c(0:hor_lps), y=IRF_lower), color = "GREY70") +
      geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
      geom_line(aes(x=c(0:hor_lps), y=IRF), colour = 'bisque3', size = 0.75) +
      scale_x_continuous(name = "",breaks=seq(0,18,1), 
      ) +
      labs(title = paste0('Resposta de ', stringr::str_to_upper(inflation_index), ' ao choque de ',colnames(modelo_endo[,i]),' - ',regime_2)) +
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
  
  ggsave(paste0('IRF_',stringr::str_to_upper(inflation_index),'_',regime_2,'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  
  
  #---------------------------------------------------------------------#
  #                                                                     #
  #             Repasse cambial conforme Belaisch (2003)                #
  #                                                                     #
  #---------------------------------------------------------------------#
  
  # Criando tibble com os dados de CI e da LP (Regime 1)
  tryCatch(expr = {
    RC_r1 <- suppressMessages(tibble(bind_cols(cumsum((results_nl$irf_s1_mean[response,,cambio_shock])
                                     /cumsum(results_nl$irf_s1_mean[cambio_shock,,cambio_shock])),
                              results_nl$irf_s1_mean[response,,cambio_shock],
                              results_nl$irf_s1_up[response,,cambio_shock],
                              results_nl$irf_s1_low[response,,cambio_shock],
                              results_nl$irf_s1_mean[cambio_shock,,cambio_shock],
                              results_nl$irf_s1_up[cambio_shock,,cambio_shock],
                              results_nl$irf_s1_low[cambio_shock,,cambio_shock]),
                    .name_repair = ~ c('RC',
                                       'Inflation_Index_mean',
                                       'Inflation_Index_upper',
                                       'Inflation_Index_lower',
                                       'cambio_mean',
                                       'cambio_upper',
                                       'cambio_lower')))
  },
  error = function(error_in_function){
    message("Error in tibble!")
    print(error_in_function)
  }
  )
  
  
  # Criando tibble que define os momentos de significância estatística de ambos as LP's
  # df.new <- RC_r1 %>%
  #   mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)) &
  #                                 ((cambio_lower >0 ) & (cambio_upper > 0)  | (cambio_lower <0) & (cambio_upper < 0)),
  #                               1,
  #                               0))
  df.new <- RC_r1 %>%
    mutate(., sig_RC = if_else( ((Inflation_Index_lower > 0) & (Inflation_Index_upper > 0) | (Inflation_Index_lower < 0) & (Inflation_Index_upper < 0)),
                                1,
                                0))
  df.new$sig_RC[1] <- 0 # Primeiro período nunca tem IC
  df.new
  
  # Criando o dataframe que estabelece os momentos a serem localizados pelo ggplot
  rects <- data.frame(xstart = (which(df.new['sig_RC'] == 1)-1), xend = (which(df.new['sig_RC'] == 1)))
  rects
    
  # Criando o gráfico
  RC_r1_plot <- ggplot(RC_r1)  + 
    {if(nrow(rects)>0) geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = 'lightblue', alpha = 0.4, show.legend = F)} +
    geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
    geom_line(aes(x=c(0:hor_lps), y=RC), colour = 'darkgrey', size = 0.75) +
    scale_x_continuous(name = "",breaks=seq(0,18,1),) +
    scale_y_continuous(name = "",labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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
            plot.title = ggtext::element_markdown(size = 10, colour = 'black', hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.6,size=11, colour = 'black'),
            axis.text.y = element_text(size=11,colour = 'black'))
  
  # Criando tibble com os dados de CI e da LP (Regime 2)
  tryCatch(expr = {
    RC_r2 <- suppressMessages(tibble(bind_cols(cumsum((results_nl$irf_s2_mean[response,,cambio_shock])
                                     /cumsum(results_nl$irf_s2_mean[cambio_shock,,cambio_shock])),
                              results_nl$irf_s2_mean[response,,cambio_shock],
                              results_nl$irf_s2_up[response,,cambio_shock],
                              results_nl$irf_s2_low[response,,cambio_shock],
                              results_nl$irf_s2_mean[cambio_shock,,cambio_shock],
                              results_nl$irf_s2_up[cambio_shock,,cambio_shock],
                              results_nl$irf_s2_low[cambio_shock,,cambio_shock]),
    .name_repair = ~ c('RC',
                       'Inflation_Index_mean',
                       'Inflation_Index_upper',
                       'Inflation_Index_lower',
                       'cambio_mean',
                       'cambio_upper',
                       'cambio_lower')))
  },
  error = function(error_in_function){
    message("Error in tibble!")
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
    scale_y_continuous(name = "",labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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
  
  ggsave(paste0('RC_',stringr::str_to_upper(inflation_index),'.png'),device = "png",width = 12, height = 8, units = "cm",scale = 2.5)
  

}

# Exportando os resultados ####
if (comm_endo == T & desemprego_on == T) {
  plot_IRFs_5_variables()
  } else if ((comm_endo == T & desemprego_on == F) | (comm_endo == F | desemprego_on == T)) {
  plot_IRFs_4_variables()
} else {
  print('Model with only 3 variables not available!')
}

# Exportando dados das configurações do modelo rodado
config_list <- c(modelo = modelo,
                 DA_variable = DA_variable,
                 comm_endo = comm_endo,
                 inflation_index = inflation_index,
                 model_trend = model_trend,
                 contemp_effect = contemp_effect,
                 lag_endog = lag_endog,
                 lag_exog = lag_exog,
                 ext_inflation = ext_inflation,
                 lag_switch_variable = lag_switch_variable,
                 gamma_transition = gamma_transition,
                 include_gfc_dummy = include_gfc_dummy,
                 sig_IC = sig_IC,
                 desemprego_on = desemprego_on,
                 desemprego_diff = desemprego_diff,
                 desemprego_exog = desemprego_exog,
                 hor_lps = hor_lps
                 )

config_list <- data.frame(config_list)

output_without_message <- capture.output(stargazer(stargazer(config_list,
                                                             summary = FALSE,
                                                             type = "text",
                                                             out = paste0("config_",stringr::str_to_upper(inflation_index),".txt"))
))


# Necessário para retornar ao diretório padrão, principalmente caso for rodar vários modelos
path_directory <- '/home/luanmugarte/Artigos/Asym_ERPT'
setwd(path_directory)

