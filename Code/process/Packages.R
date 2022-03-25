#### Carregando pacotes ####
if(!require("pacman")) install.packages("pacman")

# Testing version of lpirfs package
remotes::install_github("https://github.com/luanmugarte/lpirfs/tree/testing",
                        force = F)
pkgs_list <- c('AER',
               'readxl',
               'xts',
               'dynlm', 
               'urca',
               'FinTS',
               'seasonal',
               'x13binary',
               'smooth',
               'forecast',
               'vars',
               'tseries',
               'TTR',
               'mFilter',
               'cowplot',
               'scales',
               'RColorBrewer',
               'tidyr',
               'dplyr',
               'tibble',
               'ggplot2',
               'stargazer',
               'stats',
               'olsrr',
               'here',
               'remotes',
               'stringr',
               'lpirfs')


pacman::p_load(pkgs_list, character.only = T)

