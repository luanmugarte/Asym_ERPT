# Code for cleaning and manipulating the data 
# into the necessary objects for the estimation

# Estabelecendo diretório padrão

setwd('/home/luanmugarte/Artigos/Asym_ERPT')

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


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##
#                                                                     #
#                     DADOS MENSAIS - 1999-2020                       #
#                                                                     #
#######################################################################

# Verificando tamanho dos dados (Deve ser igual a 242 obs)
# Verificando numero de obs
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

# Exportando os dados

# Coluna de data

date = seq(from = as.Date("1999-07-01"), to = as.Date("2020-02-01"), by = 'month')

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
                       .name_repair = ~  c("date",
                                           "ipca",
                                           'ipa',
                                           'igp',
                                           "capacidade",
                                           "desemprego",
                                           "comm",
                                           "cambio",
                                           "pimpf",
                                           "ipcaindice"))

# Fim do código
