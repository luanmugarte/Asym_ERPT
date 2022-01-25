# Code for cleaning and manipulating the data 
# into the necessary objects for the estimation

# Estabelecendo diretório padrão

# Carregando os dados ####

# # IPCA
ipca <- read.csv("Data/Analytic/ipcacompleto.csv")
ipca

# IPCA índice
ipcaindice <- read.csv("Data/Analytic/ipcaindice.csv")
ipcaindice

# IGP-10 índice
igp <- read.csv("Data/Analytic/igp10.csv")

# IPA-10 índice
ipa <- read.csv("Data/Analytic/ipa10.csv")

# IPCA decomposto
ipcadecomp <- read.csv('Data/Analytic/IPCAdecomposto.csv')

# Cambio 
cambiocompleto <- read.csv("Data/Analytic/cambiocompleto.csv")

# Capacidade
capacidade <- read.csv("Data/Analytic/capacidade.csv")

# Commodities
comm <- read.csv("Data/Analytic/comm.csv")

# PIM-PF
pimpf <- read.csv("Data/Analytic/pimpf.csv")

# PIM-PF antiga
pimpfantiga <- read.csv("Data/Analytic/pimpfantiga.csv")

# Desemprego (Retropolada)
desemprego <- read.csv('Data/Analytic/PNADc Retropolada (Carvalho, 2016) - Taxa de Desemprego.csv', header = F, dec = ",")

# Índice Petróleo - FMI
petro <- read.csv("Data/Analytic/APSP_oil.csv")
petro

# PIB
pib_mensal <- read.csv("Data/Analytic/pib_mensal.csv", sep = ';')
pib_mensal

#### Manipulando as séries ####

# Ajustando nomes das colunas
colnames(capacidade) <- cbind('index','data','valores')

# Compatilizando PIM-PF ####

# Convertendo séries em objetos ts
pimpf <- ts(pimpf[1:nrow(pimpf),2], start = c(2002,1), end = c(2020,02), frequency = 12)

pimpfantiga <- ts(pimpfantiga[1:nrow(pimpfantiga),2], start = c(1999,1), frequency = 12)

# Valor de proporção
pimpf[1]/pimpfantiga[37]

# Compatibilizando a PIM-PF antiga com o valor de proporção
pimpfantiga <- ts(pimpfantiga*(pimpf[1]/pimpfantiga[37]), start = c(1999,1), frequency = 12)

# Ano de 2002
pimpfantiga[37:48]

# Criando série unificada
pimpf <- ts(c(pimpfantiga[7:36],pimpf[1:length(pimpf)]), start = c(1999,7), end = c(2020,2), frequency = 12)



# Vendo as séries
ipcaindice[1,]
ipa[1,]
igp[1,]
ipcadecomp[1,]
cambiocompleto[1,]
comm[1,]
capacidade[1,]
desemprego[1,]
pimpf[1]

# Convertendo em objetos ts - Período: 07/1999 - 02/2020 ####

ipca <- ts((ipca[7:nrow(ipca),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPCA - Índice
ipcaindice <- ts((ipcaindice[7:nrow(ipcaindice),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# Cambio
cambio <- (ts(cambiocompleto[7:nrow(cambiocompleto),2], start = c(1999,7),  end = c(2020,2), frequency = 12))

# Commodities
comm <- ts((comm[7:nrow(comm),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)

# Utilizacao de capacidade
capacidade <- ts((capacidade[91:nrow(capacidade),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)

# Taxa de desemprego
desemprego <- ts(desemprego[19:nrow(desemprego),2], start = c(1999,7), end = c(2020,2), frequency = 12)

# IGP
igp <- ts((igp[71:nrow(igp),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPA
ipa <- ts((ipa[71:nrow(ipa),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPCA - Alimentos e Bebidas
ipca_alimbebs <- ts((ipcadecomp[8:nrow(ipcadecomp),3]), start = c(1999,7), end = c(2020,2), frequency = 12)

# Petroleo
petro <- ts((petro[1:nrow(petro),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)

# PIB mensal
pib_mensal <- ts((pib_mensal[1:nrow(pib_mensal),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)

# Dummy para a GFC
gfc_dummy <- tibble(seq(from = as.Date("1999-07-01"), to = as.Date("2020-02-01"), by = 'month'), .name_repair = ~c("date")) %>%
  mutate(dummy = ifelse(((date > "2008-08-01") & (date < "2009-08-01")), 1,0)) %>%
  dplyr::select(dummy)
gfc_dummy

# Plotando as śeries
par(mfrow=c(1,1))
plot.ts(ipca)
plot.ts(igp)
plot.ts(ipa)
plot.ts(ipca_alimbebs)
plot.ts(comm)
plot.ts(capacidade)
plot.ts(cambio)
plot.ts(desemprego)
plot.ts(pimpf)


#### Dessazonalizando as séries ####

# IPCA
ipca
monthplot(ipca)
seas_ipca <- seas(x = ipca)
summary(seas_ipca)

seasonal::qs(seas_ipca)

ipcadessaz <- series(seas_ipca, "s11")
par(mfrow=c(3,1))
plot(ipca)
plot(diff(log(ipcaindice)))
plot(seas_ipca)
ipca <- ipcadessaz

# Commodities

monthplot(comm)
seas_comm <- seas(x = comm)
summary(seas_comm)

seasonal::qs(seas_comm)

# Taxa de desemprego
desemprego
monthplot(desemprego)
seas_desemprego <- seas(x = desemprego)
summary(seas_desemprego)

seasonal::qs(seas_desemprego)

desempregodessaz <- series(seas_desemprego, "s11")
par(mfrow=c(2,1))
plot(desemprego)
plot(seas_desemprego)
desemprego <- desempregodessaz
par(mfrow=c(1,1))


# PIM-PF
pimpf
monthplot(pimpf)
seas_pimpf <- seas(x = pimpf)
summary(seas_pimpf)

seasonal::qs(seas_pimpf)

pimpfdessaz <- series(seas_pimpf, "s11")
par(mfrow=c(2,1))
plot(pimpf)
plot(seas_pimpf)
pimpf <- pimpfdessaz
par(mfrow=c(1,1))

# PIB
pib_mensal
monthplot(pib_mensal)
seas_pib_mensal <- seas(x = pib_mensal)
summary(seas_pib_mensal)

seasonal::qs(seas_pib_mensal)

pib_mensaldessaz <- series(seas_pib_mensal, "s11")
par(mfrow=c(2,1))
plot(pib_mensal)
plot(seas_pib_mensal)
pib_mensal <- pib_mensaldessaz
par(mfrow=c(1,1))



# Extraindo hiato do produto
pib_mensal.hp <- hpfilter(log(pib_mensal), freq = 12, type = 'frequency')

par(mfrow=c(2,1))
plot(pib_mensal.hp$trend)
plot(pib_mensal)
plot(log(pib_mensal)-pib_mensal.hp$trend)
plot(diff(log(pib_mensal)))
par(mfrow=c(1,1))
pib_hiato <- (log(pib_mensal)-pib_mensal.hp$trend)
pib_hiato

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##
#                                                                     #
#                     DADOS MENSAIS - 1999-2020                       #
#                                                                     #
#######################################################################

# Verificando tamanho dos dados (Deve ser igual a 242 obs)
length(ipcaindice)
length(ipca)
length(igp)
length(ipa)
length(ipca_alimbebs)
length(pimpf)
length(cambio)
length(comm)
length(capacidade)
length(desemprego)
length(pib_hiato)

# Exportando os dados

# Coluna de data

date <- seq(from = as.Date("1999-07-01"), to = as.Date("2020-02-01"), by = 'month')

length(date)

# Criando a base de dados ####


dadosbrutos <- tibble(date,
                      ipca,
                      ipa,
                      igp,
                      capacidade, 
                      desemprego, #10
                      comm, 
                      cambio,
                      pimpf,
                      ipcaindice,
                      petro,
                      gfc_dummy,
                      pib_mensal,
                      pib_hiato,
                       .name_repair = ~  c("date",
                                           "ipca",
                                           'ipa',
                                           'igp',
                                           "capacidade",
                                           "desemprego",
                                           "comm",
                                           "cambio",
                                           "pimpf",
                                           "ipcaindice",
                                           "petro",
                                           "gfc_dummy",
                                           'pib',
                                           'pib_hiato'
                                           ))


#######################################################################
#                                                                     #
#                     DADOS TRIMESTRAIS                               #
#                                                                     #
#######################################################################

# PIB real trimestral
pib <- read_excel('Data/Analytic/pib_real.xlsx', col_names = c('data','pib'))
pib <- ts(pib[3:nrow(pib),2], start = c(1999,4), end = c(2019,4), frequency = 4)
pib


# Hiato do produto (IPEA)
pib_hiato <- read_excel('Data/Analytic/hiatodoproduto.xlsx', col_names = c('data','hiato'))
pib_hiato <- ts(pib_hiato[29:nrow(pib_hiato),2],  start = c(1999,4), end = c(2019,4), frequency = 4) 

# Transformando em objetos xts e reajustando para que os trimestre comecem em outubro e terminem em dezembro de 2019 ####

ipca <- as.xts(ipca)[4:(length(ipca)-2)]
igp <- as.xts(igp)[4:(length(igp)-2)]
ipa <- as.xts(ipa)[4:(length(ipa)-2)]
cambio <- as.xts(cambio)[4:(length(cambio)-2)]
capacidade <- as.xts(capacidade)[4:(length(capacidade)-2)]
comm <- as.xts(comm)[4:(length(comm)-2)]
pimpf <- as.xts(pimpf)[4:(length(pimpf)-2)]
desemprego <- as.xts(desemprego)[4:(length(desemprego)-2)]
ipcaindice <- as.xts(ipcaindice)[4:(length(ipcaindice)-2)]
petro <- as.xts(petro)[4:(length(petro)-2)]
pib_hiato <- as.xts(pib_hiato)
pib <- as.xts(pib)

# Transformando em dados trimestrais
ipca_trim <- apply.quarterly(ipca, mean)
igp_trim <- apply.quarterly(igp, mean)
ipa_trim <- apply.quarterly(ipa, mean)
cambio_trim <- apply.quarterly(cambio, mean)
comm_trim <- apply.quarterly(comm, mean)
capacidade_trim <- apply.quarterly(capacidade, mean)
pimpf_trim <- apply.quarterly(pimpf, mean)
desemprego_trim <- apply.quarterly(desemprego, mean)
ipcaindice_trim <- apply.quarterly(ipcaindice, mean)
petro_trim <- apply.quarterly(petro, mean)

date_trim <- seq(from = as.Date("1999-10-01"), to = as.Date("2019-12-01"), by = 'quarter')

# Exportando os dados ####


# Verificando nº de obs
length(ipca_trim)
length(ipcaindice_trim)
length(capacidade_trim)
length(desemprego_trim)
length(comm_trim)
length(cambio_trim)
length(pib)
length(pib_hiato)
length(pimpf_trim)
length(ipa_trim)
length(igp_trim)


dadosbrutos_trim <- tibble(date_trim,
                           ipca_trim,
                           igp_trim,
                           ipa_trim,
                           capacidade_trim,
                           pib,
                           desemprego_trim,
                           comm_trim,
                           cambio_trim,
                           pimpf_trim,
                           ipcaindice_trim,
                           pib_hiato,
                           petro_trim,
                           .name_repair = ~  c("date",
                                               "ipca",
                                               'igp',
                                               'ipa',
                                               "capacidade",
                                               "pib",
                                               "desemprego",
                                               "comm",
                                               "cambio",
                                               "pimpf",
                                               "ipcaindice",
                                               "pib_hiato",
                                               "petro"))




# Fim do código
