#### Carregando pacotes ####
if(!require("pacman")) install.packages("pacman")

pkgs_list <- list('AER',
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
               'stringr')


pacman::p_load(pkgs_list, character.only = T)

# Testing version of lpirfs package
remotes::install_github("https://github.com/luanmugarte/lpirfs/tree/testing",
                        force = F)
library(lpirfs)
