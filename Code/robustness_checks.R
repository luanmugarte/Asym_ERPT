# Robustness check ####

# Single for each ####
RC_DA <-   vector("list", length = 3*4)
RC_IR <- vector("list", length = 2*4)
RC_comm <- vector("list", length = 2*4)
RC_result <- vector("list", length = 3)

# Lista de outras opções
DA_option <- c('pib_hiato_real', 'pimpf','capacidade')
gamma_option <- c(6,9,12,15)
IR_option <- c('taxa_juros','selic')
comm_option <- c('comm','petro')

# Contador simples
counter <- 0
loop_counter <- 0

# Código do for loop
permutation_for_loop_DA <- crossing(DA_option, gamma_option)
permutation_for_loop_IR <- crossing(IR_option, gamma_option)
permutation_for_loop_comm <- crossing(comm_option, gamma_option)

# Each
library(foreach)
cl            <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)


# DA --------------------------------------------------------------
RC_result[[1]] <- foreach( i        = 1:length(RC_DA),
                           .packages = pkgs_list) %dopar%{
                             
                             lag_endog = 4
                             gamma_transition = as.numeric(permutation_for_loop_DA[i,2])
                             nome_modelo = 'default'
                             lambda_hp = 192600
                             DA_variable = as.character(permutation_for_loop_DA[i,1])
                             chol_decomp = NULL
                             include_interest_rate = T
                             include_selic = F
                             
                             # Ordem das variáveis
                             vars_order <- c(ext_inflation, 'cambio',DA_variable,
                                             'taxa_juros',inflation_index)
                             
                             model_obj <- get_model_specification(raw_data)
                             model_specs <- model_obj[[1]]
                             model_data <- model_obj[[2]]
                             
                             model_results <- run_models(model_data,model_specs)
                             results_nl <- model_results[[1]]
                             results_lin <- model_results[[2]]
                             
                             # Exporting ERPT to list object
                             RC_df <- bind_cols(cumsum(results_nl$irf_s1_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s1_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s1_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s1_low[model_specs$response,,model_specs$cambio_shock],
                                                
                                                cumsum(results_nl$irf_s2_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s2_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s2_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s2_low[model_specs$response,,model_specs$cambio_shock],
                                                .name_repair = ~ c( 'RC_s1',
                                                                    'RC_s1 upper',
                                                                    'RC_s1 lower',
                                                                    'RC_s2',
                                                                    'RC_s2 upper',
                                                                    'RC_s2 lower')) %>%
                               mutate(RC_sig_s1 = case_when((`RC_s1 upper` > 0 & `RC_s1 lower` > 0) | 
                                                              (`RC_s1 upper` < 0  & `RC_s1 lower` < 0) ~ 1,
                                                            TRUE ~ 0)) %>%
                               mutate(RC_sig_s2 = case_when((`RC_s2 upper` > 0 & `RC_s2 lower` > 0) |
                                                              (`RC_s2 upper` < 0  & `RC_s2 lower` < 0) ~ 1,
                                                            TRUE ~ 0)) %>%
                               select(RC_s1,RC_s2,RC_sig_s1,RC_sig_s2)
                             
                             RC_DA[[i]] <- RC_df
                             
                           }

parallel::stopCluster(cl)

glimpse(RC_result[[1]])

# Interest Rate ------------------------------------------------------------

cl            <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
RC_result[[2]] <- foreach( i        = 1:length(RC_IR),
                           .packages = pkgs_list) %dopar%{
                             
                             lag_endog = 4
                             gamma_transition = as.numeric(permutation_for_loop_IR[i,2])
                             nome_modelo = 'default'
                             lambda_hp = 192600
                             DA_variable = 'pimpf'
                             chol_decomp = NULL
                             
                             if (permutation_for_loop_IR[i,1] == 'taxa_juros'){
                               include_interest_rate = T
                               include_selic = F
                             } else {
                               include_interest_rate = F
                               include_selic = T
                             }
                             
                             
                             # Ordem das variáveis
                             vars_order <- c(ext_inflation, 'cambio',DA_variable,
                                             as.character(permutation_for_loop_IR[i,1]),
                                             inflation_index)
                             
                             model_obj <- get_model_specification(raw_data)
                             model_specs <- model_obj[[1]]
                             model_data <- model_obj[[2]]
                             
                             model_results <- run_models(model_data,model_specs)
                             results_nl <- model_results[[1]]
                             results_lin <- model_results[[2]]
                             
                             # Exporting ERPT to list object
                             RC_df <- bind_cols(cumsum(results_nl$irf_s1_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s1_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s1_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s1_low[model_specs$response,,model_specs$cambio_shock],
                                                
                                                cumsum(results_nl$irf_s2_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s2_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s2_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s2_low[model_specs$response,,model_specs$cambio_shock],
                                                .name_repair = ~ c( 'RC_s1',
                                                                    'RC_s1 upper',
                                                                    'RC_s1 lower',
                                                                    'RC_s2',
                                                                    'RC_s2 upper',
                                                                    'RC_s2 lower'))
                             
                             RC_IR[[i]] <- RC_df
                             
                           }

parallel::stopCluster(cl)

glimpse(RC_result[[2]])




# External inflation --------------------------------------------------------------

cl            <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)

RC_result[[3]] <- foreach( i        = 1:length(RC_comm),
                           .packages = pkgs_list) %dopar%{
                             
                             lag_endog = 4
                             gamma_transition = as.numeric(permutation_for_loop_comm[i,2])
                             nome_modelo = 'default'
                             lambda_hp = 192600
                             DA_variable = 'pimpf'
                             ext_inflation = as.character(permutation_for_loop_comm[i,1])
                             chol_decomp = NULL
                             include_interest_rate = T
                             include_selic = F
                             
                             # Ordem das variáveis
                             vars_order <- c(ext_inflation, 'cambio',DA_variable,
                                             'taxa_juros',inflation_index)
                             
                             model_obj <- get_model_specification(raw_data)
                             model_specs <- model_obj[[1]]
                             model_data <- model_obj[[2]]
                             
                             model_results <- run_models(model_data,model_specs)
                             results_nl <- model_results[[1]]
                             results_lin <- model_results[[2]]
                             
                             # Exporting ERPT to list object
                             RC_df <- bind_cols(cumsum(results_nl$irf_s1_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s1_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s1_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s1_low[model_specs$response,,model_specs$cambio_shock],
                                                
                                                cumsum(results_nl$irf_s2_mean[model_specs$response,,model_specs$cambio_shock])
                                                /cumsum(results_nl$irf_s2_mean[model_specs$cambio_shock,,model_specs$cambio_shock]),
                                                results_nl$irf_s2_up[model_specs$response,,model_specs$cambio_shock],
                                                results_nl$irf_s2_low[model_specs$response,,model_specs$cambio_shock],
                                                .name_repair = ~ c( 'RC_s1',
                                                                    'RC_s1 upper',
                                                                    'RC_s1 lower',
                                                                    'RC_s2',
                                                                    'RC_s2 upper',
                                                                    'RC_s2 lower'))
                             
                             RC_comm[[i]] <- RC_df
                             
                           }

parallel::stopCluster(cl)

glimpse(RC_result[[3]])

# Plots for robustness ----------------------------

# Aggregate demand -----
df <-RC_result[[1]][[1]] 


df$horizon <- 0:18
df[1,] <- df %>%
  slice(1) %>%
  mutate(across(matches('RC_sig'),  ~ 0))

p <- ggplot(df) +
  geom_rect(data = df %>%
              filter(RC_sig_s1 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'green'),
            alpha = 0.05) +
  geom_rect(data = df %>%
              filter(RC_sig_s2 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'red'),
            alpha = 0.05) +
  geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
              filter(name == c('RC_s1','RC_s2')),
            aes(y = value, x = horizon, color = name),
            alpha = 1) +
  scale_fill_manual(name = 'Significance',
                    values = c('indianred','dodgerblue'),
                    labels = c('High level', 'Low level')) +
  scale_color_manual(name = 'State',
                     values = c('indianred','dodgerblue'),
                     labels = c('High level', 'Low level')) +
  theme_classic()

for (i in 2:length(RC_result[[1]])) {
  df <- RC_result[[1]][[i]] 
  df$horizon <- 0:18
  df[1,] <- df %>%
    slice(1) %>%
    mutate(across(matches('RC_sig'),  ~ 0))
  
  p <- p +
    geom_rect(data = df %>%
                filter(RC_sig_s1 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'green'),
              alpha = 0.05) +
    geom_rect(data = df %>%
                filter(RC_sig_s2 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'red'),
              alpha = 0.05) +
    geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
                filter(name == c('RC_s1','RC_s2')),
              aes(y = value, x = horizon, color = name),
              alpha = 1) 
}
DA_robust_plot <- p +
  xlab('') +
  ylab('') +
  labs(title = 'Aggregate demand') +
  geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
  scale_x_continuous(name = "",breaks=seq(0,18,2),) +
  scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),
                     labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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

# Interest rate ----
df <- RC_result[[2]][[1]] 

df$horizon <- 0:18
df[1,] <- df %>%
  slice(1) %>%
  mutate(across(matches('RC_sig'),  ~ 0))

p <- ggplot(df) +
  geom_rect(data = df %>%
              filter(RC_sig_s1 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'green'),
            alpha = 0.05) +
  geom_rect(data = df %>%
              filter(RC_sig_s2 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'red'),
            alpha = 0.05) +
  geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
              filter(name == c('RC_s1','RC_s2')),
            aes(y = value, x = horizon, color = name),
            alpha = 1) +
  scale_fill_manual(name = 'Significance',
                    values = c('indianred','dodgerblue'),
                    labels = c('High level', 'Low level')) +
  scale_color_manual(name = 'State',
                     values = c('indianred','dodgerblue'),
                     labels = c('High level', 'Low level')) +
  theme_classic()

for (i in 2:length(RC_result[[2]])) {
  df <- RC_result[[2]][[i]] 
  df$horizon <- 0:18
  df[1,] <- df %>%
    slice(1) %>%
    mutate(across(matches('RC_sig'),  ~ 0))
  
  p <- p +
    geom_rect(data = df %>%
                filter(RC_sig_s1 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'green'),
              alpha = 0.05) +
    geom_rect(data = df %>%
                filter(RC_sig_s2 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'red'),
              alpha = 0.05) +
    geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
                filter(name == c('RC_s1','RC_s2')),
              aes(y = value, x = horizon, color = name),
              alpha = 1) 
}

IR_robust_plot <- p +
  xlab('') +
  ylab('') +
  labs(title = 'Interest rate')
geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
  scale_x_continuous(name = "",breaks=seq(0,18,2),) +
  scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),
                     labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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





# External inflation
df <- RC_result[[3]][[1]] 


df$horizon <- 0:18
df[1,] <- df %>%
  slice(1) %>%
  mutate(across(matches('RC_sig'),  ~ 0))

p <- ggplot(df) +
  geom_rect(data = df %>%
              filter(RC_sig_s1 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'green'),
            alpha = 0.05) +
  geom_rect(data = df %>%
              filter(RC_sig_s2 == 1),
            aes(xmin=horizon-0.5,xmax=horizon+0.5,
                ymin = -Inf, ymax = Inf, fill = 'red'),
            alpha = 0.05) +
  geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
              filter(name == c('RC_s1','RC_s2')),
            aes(y = value, x = horizon, color = name),
            alpha = 1) +
  scale_fill_manual(name = 'Significance',
                    values = c('indianred','dodgerblue'),
                    labels = c('High level', 'Low level')) +
  scale_color_manual(name = 'State',
                     values = c('indianred','dodgerblue'),
                     labels = c('High level', 'Low level')) +
  theme_classic()

for (i in 2:length(RC_result[[3]])) {
  df <- RC_result[[3]][[i]] 
  df$horizon <- 0:18
  df[1,] <- df %>%
    slice(1) %>%
    mutate(across(matches('RC_sig'),  ~ 0))
  
  p <- p +
    geom_rect(data = df %>%
                filter(RC_sig_s1 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'green'),
              alpha = 0.05) +
    geom_rect(data = df %>%
                filter(RC_sig_s2 == 1),
              aes(xmin=horizon-0.5,xmax=horizon+0.5,
                  ymin = -Inf, ymax = Inf, fill = 'red'),
              alpha = 0.05) +
    geom_line(data = df %>% pivot_longer(cols = !horizon) %>%
                filter(name == c('RC_s1','RC_s2')),
              aes(y = value, x = horizon, color = name),
              alpha = 1) 
}

IR_robust_plot <- p +
  xlab('') +
  ylab('') +
  labs(title = 'External inflation')
geom_hline(yintercept = 0, colour= 'darkgrey', linetype = 'dashed') +
  scale_x_continuous(name = "",breaks=seq(0,18,2),) +
  scale_y_continuous(name = "",breaks = scales::pretty_breaks(n = 8),
                     labels = function(x) paste0(x*100, "%"),expand = c(0, 0)) +
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


# Old -------

# col_names = list()
# 
# for (i in 1:length(RC_result[[1]])) {
#   col_names[[i]] <- paste0(colnames(df),'_',i)
# }
# 
# for (i in 2:length(RC_result[[1]])) {
#   df <- bind_cols(df,RC_result[[1]][[i]],
#                   .name_repair = 'minimal')
# }
# 
# colnames(df) <- unlist(col_names)
# 
# 
# df$horizon = 0:18
# 
# 
# 
# df
# 
# df %>%
#   select(matches('RC_sig')) %>%
#   filter(if_any(everything(), ~ . == 1))
# 
# 
# ggplot(df) +
#   geom_rect(data = df %>%
#               select(horizon,matches('RC_sig')) %>%
#               filter(if_any(everything(), ~ . == 1)),
#             aes(xmin=horizon-0.5,xmax=horizon+0.5,
#                 ymin = -Inf, ymax = Inf, fill = 'green'),
#             alpha = 0.2) +
#   geom_rect(data = df %>%
#               filter(RC_sig_s2_1 == 1),
#             aes(xmin=horizon-0.5,xmax=horizon+0.5,
#                 ymin = -Inf, ymax = Inf, fill = 'red'),
#             alpha = 0.2) +
#   geom_line(data = df %>% pivot_longer(cols = !horizon) %>% 
#               filter(name == c('RC_s1_1','RC_s2_1')),
#             aes(y = value, x = horizon, color = name)) +
#   scale_fill_manual(name = 'Significance',
#                     values = c('indianred','dodgerblue'),
#                     labels = c('High level', 'Low level')) +
#   scale_color_manual(name = 'State',
#                      values = c('indianred','dodgerblue'),
#                      labels = c('High level', 'Low level')) +
#   theme_classic()