# Other plots for the article

# Exchange rate and inflation dynamics

# IPCA - Cheio 
ipca_cheio <- get_series(13522, start_date = '1999-01-01', as = 'xts')
ipca_cheio

# Taxa de Cambio Nominal
taxa_cambio <- get_series(3697, start_date = '1998-12-01', as = 'xts')
taxa_cambio <- diff(log(taxa_cambio))
time(taxa_cambio)

# Plot

df <- bind_cols(time(ipca_cheio),
                ipca_cheio,
                taxa_cambio[2:nrow(taxa_cambio)],
                .name_repair = ~c('date','IPCA','taxa_cambio'))

df %>%
  filter(date > '2010-01-01') %>%
  mutate(cambio = taxa_cambio) %>%
  mutate(n = n()) %>%
  select(cambio,n) %>%
  filter(cambio > 0) %>%
  summarize(count = paste0('N_dep: ',count(.), ' N_ap: ', n - count(.)))

df %>%
  filter(date > '2010-01-01') %>%
  mutate(cambio = SMA(taxa_cambio, n = 3)) %>%
  mutate(n = n()) %>%
  select(cambio,n) %>%
  filter(cambio > 0) %>%
  summarize(count = paste0('N_dep: ',count(.), ' N_ap: ', n - count(.)))

df %>%
  filter(date > '2010-01-01') %>%
  mutate(cambio = SMA(taxa_cambio, n = 5)) %>%
  select(cambio) %>%
  filter(cambio > 0) %>%
  summarize(count = paste0('N_dep: ',count(.), ' N_ap: ', nrow(df) - count(.)))

df %>%
  mutate(cambio = SMA(taxa_cambio, n = 7)) %>%
  select(cambio) %>%
  filter(cambio > 0) %>%
  summarize(count = paste0('N_dep: ',count(.), ' N_ap: ', nrow(df) - count(.)))


df
df_rects <- df %>%
  mutate(ER_devaluation = case_when(taxa_cambio > 0.05 ~ 'Bigger than 5%',
                                    taxa_cambio > 0 & taxa_cambio < 0.05 ~ 'Less than 5%',
                                    TRUE ~ 'Nominal valuation')) %>%
  select(date, ER_devaluation)

levels_order <- c('Nominal valuation',
                  'Less than 5%',
                  'Bigger than 5%')


ggplot(df) +
  geom_rect(data = df_rects, 
            aes(xmin = date, xmax = date + 31, ymin = -Inf, ymax = Inf,
                fill = factor(ER_devaluation, levels = levels_order)),
            alpha = 0.45) +
  geom_line(aes(y = IPCA, x = date), color = 'black', size = 1) +
  scale_x_date(date_breaks = '12 months', 
               date_minor_breaks = '6 months',
               date_labels = "%m-%Y",expand = c(0.0, 0)) +
  scale_y_continuous(name = "",
                     labels = function(x) paste0(x, "%"),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = c(0.01, 0.01)) +
  labs(title = '') +
  scale_fill_manual(name = '',
                    values = c('lightblue', "#FC9272", "#DE2D26")) +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.title = ggtext::element_markdown(hjust = 0.5,
                                                size = 12, 
                                                colour = 'black',
                                                face ='bold'),
          legend.text = element_text(size=10, color = 'black',
                                      face = 'bold'),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5,
                                     hjust = 0.7,
                                     size=11, 
                                     colour = 'black',
                                     face = 'bold'),
          axis.text.y = element_text(size=12,face = 'bold',
                                     colour = 'black')) 
  

ggsave(paste0('ER_inflation_dynamics.png'),
       path = file.path('Output/Figures/'),
       device = "png",width = 12, height = 8, units = "cm",scale = 2)
